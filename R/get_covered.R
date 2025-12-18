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
