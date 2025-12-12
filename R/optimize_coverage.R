
optimize_monitor_coverage <- function(locations, uncovered, cover_dist) {
  # add generic id for tracking
  uncovered <- uncovered |>
    dplyr::mutate(.id = dplyr::row_number())

  # Determine which of uncovered are covered by each location
  distances <- sf::st_distance(uncovered, locations)
  coverages <- distances |>
    # Convert to data frame
    as.data.frame() |>
    setNames(1:nrow(locations)) |> 
    dplyr::mutate(uncovered_id = uncovered$.id) |>
    # Wide to long
    tidyr::pivot_longer(-uncovered_id,
      names_to = "location_id",
      values_to = "dist",
      names_transform = as.numeric
    ) |>
    # include weighting column if present
    dplyr::left_join(
      uncovered |> handyr::sf_as_df() |> dplyr::select(.id, dplyr::any_of("total")), 
      by = c(uncovered_id = ".id")
    )
  # Add weighting column if not already present
  if(! "total" %in% names(coverages)) {
    coverages <- coverages |>
      dplyr::mutate(total = 1)
  }
  # Join all communities near each location into a single entry per location
  # ie: 3 rows (1 for each nearby community) for a site -> 1 row with a column
  #   that has all 3 community ids pasted together with a "|" between
  coverages <- coverages |>
    dplyr::group_by(location_id) |> # for each location
    dplyr::filter(dist <= cover_dist) |> # drop communities outside coverage range
    dplyr::summarise(
      n = sum(total), # count how many uncovered within range
      nearby_totals = paste(total, collapse = "|"), # Paste the totals together
      nearby_ids = paste(uncovered_id, collapse = "|") # Paste the ids together
    )

  # Keep placing `locations` w/ best coverage until no more `uncovered` to cover
  optimized_locations <- list()
  while (sum(coverages$n) != 0 & length(optimized_locations) < nrow(locations)) {
    # Store the location with best coverage
    coverages <- dplyr::arrange(coverages, desc(n))
    optimized_locations <- optimized_locations |>
      c(list(coverages[1, ]))
    # Get uncovered now covered by that location
    newly_covered <- coverages$nearby_ids[1] |>
      stringr::str_split("\\|", simplify = T) |> 
      _[1, ]
    # Remove those from the nearby_ids list, and adjust nearby counts and totals
    # (eventually this will stop the loop once all are covered)
    coverages[, c("n", "nearby_totals", "nearby_ids")] <- 1:nrow(coverages) |> 
      handyr::for_each(function(i) {
        current_nearby <- coverages$nearby_ids[i] |> 
          stringr::str_split("\\|", simplify = TRUE) |> 
          _[1, ]
        current_totals <- coverages$nearby_totals[i] |> 
          stringr::str_split("\\|", simplify = TRUE) |> 
          _[1, ] |>
          as.numeric()
        updated_nearby <- current_nearby[!current_nearby %in% newly_covered]
        updated_totals <- current_totals[!current_nearby %in% newly_covered]
        if (length(updated_nearby) == 1) if (updated_nearby == "") updated_nearby <- character(0)
        if (length(updated_totals) == 1) if (is.na(updated_totals)) updated_totals <- numeric(0)
        data.frame(
          n = sum(updated_totals),
          nearby_totals = paste(updated_totals, collapse = "|"),
          nearby_ids = paste(updated_nearby, collapse = "|")
        )
      }, .as_list = TRUE, .bind = TRUE)
  }
  # Return locations of suggested monitors
  locations[
    optimized_locations |>
      dplyr::bind_rows() |>
      dplyr::pull(location_id), 
  ]
}