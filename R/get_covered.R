#' Get locations within a specified distance of another set of locations
#'
#' @description
#'   Checks whether each location in `to_cover` is within `cover_distance` of each location in `install_at`.
#'   Leverages the fast [sf::st_is_within_distance()] function to perform the check, so distances are not provided.
#'   See [mark_nearest_location()] if the nearest distance is needed as well.
#' @param install_at A spatial data frame of installation points
#' @param to_cover A spatial data frame of points to cover
#' @param cover_distance The distance each installation point covers
#' @return A tibble data frame with two columns:
#'   - `to_cover_id`: the row indices of `to_cover` that are within the `cover_distance` of `install_at`
#'   - `install_at_id`: the row indices of `install_at` that cover `to_cover_id`
#'   A `to_cover` location may be covered by multiple (or no) `install_at` locations.
#' @export
get_covered <- function(
  install_at,
  to_cover,
  cover_distance = 25 |> units::set_units("km")
) {
  stopifnot(
    inherits(install_at, "sf") & nrow(install_at) > 0,
    inherits(to_cover, "sf") & nrow(install_at) > 0,
    inherits(cover_distance, "units") | is.numeric(cover_distance)
  )

  to_cover |>
    sf::st_is_within_distance(
      y = install_at,
      dist = cover_distance
    ) |>
    tibble::enframe(
      name = "to_cover_id",
      value = "matches"
    ) |>
    tidyr::unnest("matches") |>
    dplyr::rename(install_at_id = "matches")
}
