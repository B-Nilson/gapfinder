test_that("real case works", {
  install_at <- canadata::communities |>
    dplyr::filter(prov_terr == "YT") |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") |>
    # Cities are easier to install than hamlets
    # here they provide 4x more coverage (4 levels of type)
    dplyr::mutate(ease_of_install = nlevels(type) + 1 - as.numeric(type))

  # Define what we want the monitors to cover (Yukon population AND Yukon communities)
  to_cover <- canadata::gridded_2016_population |>
    dplyr::rename(prov_terr = "prov_terrs") |>
    dplyr::mutate(type = "population") |>
    dplyr::bind_rows(
      canadata::communities |>
        dplyr::mutate(total_population = 1)
    ) |>
    dplyr::filter(stringr::str_detect(prov_terr, "YT")) |> # any cell/community that intersects Yukon
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")

  # Find the optimal locations to get all YT population within 10 km of a monitor
  # Coverage depends on population x ease of install
  prioritized_locations <- install_at |>
    prioritize_installations(
      to_cover = to_cover |> dplyr::filter(type == "population"),
      cover_distance = units::set_units(10, "km"),
      weight_columns = c("total_population", "ease_of_install"),
      suffix = "_population"
    ) |>
    prioritize_installations(
      to_cover = to_cover |> dplyr::filter(type != "population") |> dplyr::group_by(type),
      cover_distance = units::set_units(10, "km"),
      weight_columns = c("total_population", "ease_of_install"),
      suffix = "_communities"
    ) |> 
    expect_silent() |> 
    expect_snapshot()
})
