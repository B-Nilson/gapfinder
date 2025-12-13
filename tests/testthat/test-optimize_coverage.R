test_that("real case works", {
  install_at <- canadata::communities |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") |>
    dplyr::mutate(ease_of_install = as.numeric(forcats::fct_rev(type)))

  to_cover <- canadata::gridded_2016_population |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")

  existing_locations <- "https://aqmap.ca/aqmap/data/aqmap_most_recent_obs.csv" |>
    read.csv() |> 
    dplyr::select(site_id = sensor_index, network, lng, lat, name = monitor) |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")

  optimized_locations <- optimize_coverage(
    install_at = install_at,
    to_cover = to_cover,
    existing_locations = existing_locations,
    cover_distance = units::set_units(25, "km"),
    weight_columns = c("ease_of_install", "total_population")
  ) |>
    expect_silence()

  skip_if_not(interactive())

  list(
    install_at,
    existing_locations,
    optimized_locations
  ) |>
    handyr::for_each(.enumerate = TRUE, \(x, i) {
      aqmapr::PointLayer(
        data = x,
        group = c(
          "Able to Install At",
          "Existing Installations",
          "Optimized Installations"
        )[i],
        label = ~name,
        fill = c("grey", "green","blue")[i]
      )
    }) |>
    aqmapr::make_leaflet_map(point_layers = _)
})
