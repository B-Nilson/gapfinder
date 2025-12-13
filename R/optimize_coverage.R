#' @importFrom rlang :=
optimize_coverage <- function(install_at, to_cover, cover_distance = 25 |> units::set_units("km"), weight_column = ".weight") {
  # add generic id for tracking
  to_cover <- to_cover |>
    dplyr::mutate(.id = dplyr::row_number())

  # Determine which of to_cover are covered by each install_at
  coverages <- to_cover |> 
    sf::st_is_within_distance(y = install_at, dist = cover_distance) |>
    tibble::enframe(name = "to_cover_id", value = "matches") |>
    tidyr::unnest(matches) |>
    dplyr::rename(install_at_id = matches) |>
    # include weighting column if present
    dplyr::left_join(
      to_cover |> handyr::sf_as_df() |> dplyr::select(".id", dplyr::any_of(weight_column)), 
      by = c(to_cover_id = ".id")
    )
  
  # Add weighting column if not already present
  if(! weight_column %in% names(coverages)) {
    warning("No `weight_column` column found in `to_cover`, assuming equal weighting of points to cover.")
    coverages <- coverages |>
      dplyr::mutate(!!weight_column := 1)
  }
  # Join all to_cover near each location into a single entry per location
  # ie: 3 rows (1 for each nearby community) for a site -> 1 row with a column
  #   that has all 3 community ids pasted together with a "|" between
  coverages <- coverages |>
    dplyr::group_by(.data$install_at_id) |> # for each `install_at` point
    dplyr::summarise(
      n = .data[[weight_column]] |> sum(),
      nearby_weights = .data[[weight_column]] |> list(),
      nearby_ids = .data$to_cover_id |> list()
    )

  # Keep placing `install_at` w/ best coverage until no more `to_cover` to cover
  optimized_locations <- list()
  while(sum(coverages$n) != 0 & length(optimized_locations) < nrow(install_at)) {
    # Store the location with best coverage
    coverages <- coverages |> 
      dplyr::arrange(dplyr::desc(.data$n)) |> 
      dplyr::filter(.data$n != 0)
    optimized_locations <- optimized_locations |>
      c(list(coverages[1, ]))
    newly_covered <- coverages$nearby_ids[[1]]
    coverages <- coverages[-1, ]
    if (nrow(coverages) == 0) {
      break
    } 

    # Remove those from the nearby_ids list, and adjust nearby counts and weights
    # (eventually this will stop the loop once all are covered)
    coverages[, c("n", "nearby_ids", "nearby_weights")] <- coverages$nearby_ids |> 
      handyr::for_each( 
        .bind = TRUE,
        .enumerate = TRUE,
        .show_progress = FALSE,
        \(current_nearby, i) {
          current_weights <- coverages$nearby_weights[[i]]
          keep <- !current_nearby %in% newly_covered
          updated_nearby <- current_nearby[keep]
          updated_weights <- current_weights[keep]
          updated_n <- sum(updated_weights)
          list(
            n = updated_weights |> sum(),
            nearby_ids = updated_nearby |> list(),
            nearby_weights = updated_weights |> list()
          )
        } 
      )
  }
  # Return selected install_at points
  rows <- optimized_locations |>
    dplyr::bind_rows() |>
    dplyr::pull(install_at_id) 
  install_at[rows, ]
}
