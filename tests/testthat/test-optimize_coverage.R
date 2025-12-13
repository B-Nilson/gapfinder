test_that("real case works", {
  install_at <- canadata::communities |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") |>
    dplyr::mutate(ease_of_install = as.numeric(forcats::fct_rev(type)))

  to_cover <- canadata::gridded_2016_population |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")

  optimized_locations <- optimize_coverage(
    install_at = install_at,
    to_cover = to_cover,
    cover_distance = units::set_units(100, "km"),
    weight_columns = c("ease_of_install", "total_population")
  ) |>
    expect_silence()

  skip_if_not(interactive())

  list(
    install_at,
    to_cover,
    optimized_locations
  ) |>
    handyr::for_each(.enumerate = TRUE, \(x, i) {
      aqmapr::PointLayer(
        data = x,
        group = c(
          "Able to Install At",
          "Points to Cover",
          "Optimized Installations"
        )[i]
      )
    }) |> 
      aqmapr::make_leaflet_map(point_layers = _)
})
