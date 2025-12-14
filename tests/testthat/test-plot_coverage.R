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

  cover_distance <- units::set_units(25, "km")

  # Find the optimal locations to get all YT population within 10 km of a monitor
  # Coverage depends on population x ease of install
  optimized_locations <- install_at |>
    optimize_coverage(
      to_cover = to_cover,
      existing_locations = existing_locations,
      cover_distance = cover_distance,
      weight_columns = c("ease_of_install", "total_population")
    )

  install_at |>
    plot_coverage(
      to_cover = to_cover,
      existing_locations = existing_locations,
      optimized_locations = optimized_locations,
      cover_distance = cover_distance,
      weight_columns = c(
        "Ease of Install\n(Community Type)" = "ease_of_install",
        "2016 Population\n(To Cover)" = "total_population"
      ),
      in_canada = TRUE
    ) |>
    expect_s3_class("ggplot") |>
    expect_warning(
      "attribute variables are assumed to be spatially constant throughout all geometries"
    ) |> # TODO: fix?
    expect_silent()
})
