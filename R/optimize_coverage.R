#' Optimize coverage of a set of locations by a set of installations.
#'
#' This function takes a set of points to cover and a set of installations
#' and returns the subset of installations that cover the most points within a set distance
#' with the least number of installations. It allows for weighting of
#' points to cover and installations based on columns in the data frames.
#'
#' @param install_at An `sf` data frame containing the locations of the installations.
#' @param to_cover An `sf` data frame containing the locations of the places to cover with installations.
#' @param existing_locations A data frame containing the locations of existing installations.
#'   `to_cover` will be filtered to exclude points that are already covered by `existing_locations`.
#' @param cover_distance The distance from an installation that a point is considered to be covered.
#'   Expected to be a `units` object, otherwise assumed to be in km.
#'   Defaults to 25 km.
#' @param weight_columns A character vector containing the names of the columns in `install_at` and `to_cover`
#'   that should be used as weights when calculating the coverage of each installation.
#'   A weight of 2 is akin to double coverage from/for that location.
#'   If no columns match the names in `weight_columns`, equal weighting is assumed.
#'   If only one column is provided, it will be used for both `install_at` and `to_cover`.
#'   Defaults to c(".weight", ".weight").
#'
#' @return A data frame containing the subset of installations that cover the most points with the least number of installations, weighted by `weight_columns`.
#'
#' @export
#' @importFrom rlang :=
optimize_coverage <- function(
  install_at,
  to_cover,
  existing_locations = NULL,
  cover_distance = 25 |> units::set_units("km"),
  weight_columns = c(".weight", ".weight")
) {
  stopifnot(
    inherits(install_at, "sf") & nrow(install_at) > 0,
    inherits(to_cover, "sf") & nrow(to_cover) > 0,
    is.null(existing_locations) | inherits(existing_locations, "sf"),
    inherits(cover_distance, "units") | is.numeric(cover_distance),
    length(weight_columns) %in% c(1, 2),
    is.character(weight_columns)
  )

  # Cleanup inputs
  cover_distance <- cover_distance |>
    units::set_units("km") # just in case
  if (length(weight_columns) == 1) {
    weight_columns <- rep(weight_columns, 2)
  }
  names(weight_columns) <- c("to_cover_weight", "install_at_weight")

  # add generic id for tracking
  to_cover <- to_cover |>
    dplyr::mutate(.id = dplyr::row_number())
  install_at <- install_at |>
    dplyr::mutate(.id = dplyr::row_number())

  # Drop to_cover that are already covered by existing_locations
  if (!is.null(existing_locations)) {
    covered_by_existing <- existing_locations |>
      get_covered(to_cover = to_cover, cover_distance = cover_distance) |>
      dplyr::pull(to_cover_id) |>
      unique()
    to_cover <- to_cover |>
      dplyr::filter(!.data$.id %in% covered_by_existing) |>
      dplyr::mutate(.id = dplyr::row_number())
  }

  # Determine which of to_cover are covered by each `install_at`
  coverages <- install_at |>
    get_covered(to_cover = to_cover, cover_distance = cover_distance) |>
    add_weight_columns(
      to_cover = to_cover,
      install_at = install_at,
      weight_columns = weight_columns
    )

  # Combine and sum all ids/weights for each `install_at`
  coverages <- coverages |>
    dplyr::group_by(.data$install_at_id) |>
    dplyr::mutate(
      weight = .data[[names(weight_columns)[1]]] +
        .data[[names(weight_columns)[2]]]
    ) |>
    dplyr::summarise(
      n = .data$weight |> sum(),
      nearby_weights = .data$weight |> list(),
      nearby_ids = .data$to_cover_id |> list()
    )

  # Keep placing `install_at` w/ best coverage until no more `to_cover` to cover
  optimized_locations <- list()
  is_more_to_cover <- function() {
    sum(coverages$n) != 0 & length(optimized_locations) < nrow(install_at)
  }
  while (is_more_to_cover()) {
    # Store the location with best coverage
    coverages <- coverages |>
      dplyr::arrange(dplyr::desc(.data$n)) |>
      dplyr::filter(.data$n != 0)
    optimized_locations <- optimized_locations |>
      c(list(coverages[1, ]))

    # Remove now covered ids from the nearby_ids list, and adjust nearby counts and weights
    coverages <- coverages[-1, ] |>
      update_coverage(newly_covered_ids = coverages$nearby_ids[[1]])
  }

  # Return selected install_at points
  rows <- optimized_locations |>
    dplyr::bind_rows() |>
    dplyr::pull(install_at_id)
  install_at[rows, ] |>
    dplyr::select(-".id")
}

update_coverage <- function(coverages, newly_covered_ids) {
  if (nrow(coverages) == 0) {
    return(coverages)
  }
  modified_cols <- c("n", "nearby_ids", "nearby_weights")
  coverages[, modified_cols] <- coverages$nearby_ids |>
    handyr::for_each(
      .bind = TRUE,
      .enumerate = TRUE,
      .show_progress = FALSE,
      \(current_nearby, i) {
        current_weights <- coverages$nearby_weights[[i]]
        keep <- !current_nearby %in% newly_covered_ids
        updated_weights <- current_weights[keep]
        list(
          n = updated_weights |> sum(),
          nearby_ids = current_nearby[keep] |> list(),
          nearby_weights = updated_weights |> list()
        )
      }
    )
  return(coverages)
}

get_covered <- function(install_at, to_cover, cover_distance) {
  to_cover |>
    sf::st_is_within_distance(y = install_at, dist = cover_distance) |>
    tibble::enframe(name = "to_cover_id", value = "matches") |>
    tidyr::unnest(matches) |>
    dplyr::rename(install_at_id = matches)
}

add_weight_columns <- function(
  coverages,
  to_cover,
  install_at,
  weight_columns
) {
  coverages <- coverages |>
    # include weighting columns if present
    dplyr::left_join(
      install_at |>
        handyr::sf_as_df() |>
        dplyr::select(".id", dplyr::any_of(weight_columns[1])),
      by = c(install_at_id = ".id")
    ) |>
    dplyr::left_join(
      to_cover |>
        handyr::sf_as_df() |>
        dplyr::select(".id", dplyr::any_of(weight_columns[2])),
      by = c(to_cover_id = ".id")
    )

  # Add weighting columns if not already present
  if (!names(weight_columns[1]) %in% names(coverages)) {
    warning(
      "No `weight_columns[1]` column found in `to_cover`, assuming equal weighting of points to cover."
    )
    coverages <- coverages |>
      dplyr::mutate(!!names(weight_columns[1]) := 1)
  }
  if (!names(weight_columns[2]) %in% names(coverages)) {
    warning(
      "No `weight_columns[2]` column found in `install_at`, assuming equal weighting of installation locations."
    )
    coverages <- coverages |>
      dplyr::mutate(!!names(weight_columns[2]) := 1)
  }

  return(coverages)
}
