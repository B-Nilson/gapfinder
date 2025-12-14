test_that("real case works", {
  # Define where we could install monitors (Yukon communities)
  install_at <- canadata::communities |>
    dplyr::filter(prov_terr == "YT") |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") |>
    # Cities are easier to install than hamlets
    # here they provide 4x more coverage (4 levels of type)
    dplyr::mutate(ease_of_install = nlevels(type) + 1 - as.numeric(type))

  # Define what we want the monitors to cover (Yukon population AND Yukon communities)
  to_cover <- canadata::gridded_2016_population |>
    dplyr::bind_rows(
      canadata::communities |> dplyr::mutate(total_population = 1)
    ) |>
    dplyr::filter(stringr::str_detect(prov_terr, "YT")) |> # any cell/community that intersects Yukon
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")

  # Define existing monitors (Canadian PM2.5 monitoring network - NAPS, PurpleAir, and AQEgg networks)
  existing_locations <- "https://aqmap.ca/aqmap/data/aqmap_most_recent_obs.csv" |>
    read.csv() |>
    dplyr::select(site_id = sensor_index, network, lng, lat, name = monitor) |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")

  # Find the optimal locations to get all YT population within 10 km of a monitor
  # Coverage depends on population x ease of install
  optimized_locations <- install_at |>
    optimize_coverage(
      to_cover = to_cover,
      existing_locations = existing_locations,
      cover_distance = units::set_units(10, "km"),
      weight_columns = c("ease_of_install", "total_population")
    ) |>
    expect_silent()

  skip_if_not(interactive())

  to_cover_areas <- to_cover |>
    sf::st_buffer(dist = units::set_units(25, "km")) |>
    sf::st_union()

  list(
    install_at,
    existing_locations,
    optimized_locations
  ) |>
    handyr::for_each(.enumerate = TRUE, .show_progress = FALSE, \(x, i) {
      aqmapr::PointLayer(
        data = x,
        group = c(
          "Able to Install At",
          "Existing Installations",
          "Optimized Installations"
        )[i],
        label = ~name,
        fill = c("grey", "green", "blue")[i]
      )
    }) |>
    aqmapr::make_leaflet_map(
      point_layers = _,
      polygon_layers = list(aqmapr::PolygonLayer(
        data = to_cover_areas |>
          sf::st_sf() |>
          dplyr::rename(geometry = "to_cover_areas"),
        group = "To Cover",
        fill = "green",
        opacity = 0.3
      ))
    )
})
