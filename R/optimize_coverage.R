
optimize_coverage <- function(locations, to_cover, cover_dist) {
  # add generic id for tracking
  to_cover <- to_cover |>
    dplyr::mutate(.id = dplyr::row_number())

  # Determine which of to_cover are covered by each location
  distances <- to_cover |> 
    sf::st_distance(locations)
  coverages <- distances |>
    # Convert to data frame
    as.data.frame() |>
    setNames(seq_len(nrow(locations))) |> 
    dplyr::mutate(to_cover_id = to_cover$.id) |>
    tidyr::pivot_longer(
      cols = -"to_cover_id",
      names_to = "location_id",
      values_to = "dist",
      names_transform = as.numeric
    ) |>
    # include weighting column if present
    dplyr::left_join(
      to_cover |> handyr::sf_as_df() |> dplyr::select(".id", dplyr::any_of("total")), 
      by = c(to_cover_id = ".id")
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
    dplyr::group_by("location_id") |> # for each location
    dplyr::filter(.data$dist <= cover_dist) |> # drop to_cover outside coverage range
    dplyr::summarise(
      n = .data$total |> sum(),
      nearby_totals = .data$total |> paste(collapse = "|"),
      nearby_ids = .data$to_cover_id |> paste(collapse = "|")
    )

  # Keep placing `locations` w/ best coverage until no more `to_cover` to cover
  optimized_locations <- list()
  while (sum(coverages$n) != 0 & length(optimized_locations) < nrow(locations)) {
    # Store the location with best coverage
    coverages <- coverages |> dplyr::arrange(dplyr::desc(.data$n))
    optimized_locations <- optimized_locations |>
      c(list(coverages[1, ]))
    # Get to_cover now covered by that location
    newly_covered <- stringr::str_split(
      string = coverages$nearby_ids[1],
      pattern ="\\|", 
      simplify = TRUE
    )[1, ]
    # Remove those from the nearby_ids list, and adjust nearby counts and totals
    # (eventually this will stop the loop once all are covered)
    coverages[, c("n", "nearby_totals", "nearby_ids")] <- seq_len(nrow(coverages)) |> 
      handyr::for_each( 
        .bind = TRUE,
        \(i) {
          current_nearby <- stringr::str_split(
            string = coverages$nearby_ids[i],
            pattern = "\\|", 
            simplify = TRUE
          )[1, ]
          current_totals <- stringr::str_split(
            string = coverages$nearby_totals[i],
            pattern = "\\|", 
            simplify = TRUE
          )[1, ] |>
            as.numeric()
          updated_nearby <- current_nearby[!current_nearby %in% newly_covered]
          updated_totals <- current_totals[!current_nearby %in% newly_covered]
          if (length(updated_nearby) == 1) if (updated_nearby == "") updated_nearby <- character(0)
          if (length(updated_totals) == 1) if (is.na(updated_totals)) updated_totals <- numeric(0)
          data.frame(
            n = updated_totals |> sum(),
            nearby_totals = updated_totals |> paste(collapse = "|"),
            nearby_ids = updated_nearby |> paste(collapse = "|")
          )
        } 
      )
  }
  # Return locations of suggested monitors
  rows <- optimized_locations |>
    dplyr::bind_rows() |>
    dplyr::pull(location_id) 
  locations[rows, ]
}