# Where to install monitors - Yukon communities
install_at_types <- c(levels(canadata::communities$type), "indigenous")
install_at <- canadata::communities |>
  dplyr::bind_rows(
    canadata::indigenous_communities |>
      dplyr::mutate(type = "indigenous")
  ) |>
  dplyr::mutate(type = .data$type |> factor(levels = install_at_types)) |>
  dplyr::filter(prov_terr == "YT") |>
  sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") |>
  # Ease of installation scales from 1 (most difficult) - 3 (easiest)
  # Linear scale from Indigenous communities, to hamlets, villages, towns, then cities
  dplyr::mutate(
    ease_of_install = as.numeric(forcats::fct_rev(type)) |>
      scales::rescale(to = c(1, 3))
  )

# Define what we want the monitors to cover (Yukon population AND Yukon communities)
to_cover_types <- c(install_at_types, c("rural_population", "urban_population"))
to_cover <- canadata::gridded_2016_population |>
  dplyr::mutate(urban_population = total_population - rural_population) |>
  dplyr::select(
    lat,
    lng,
    prov_terr = "prov_terrs",
    fcst_zone = "fcst_zones",
    "rural_population",
    "urban_population"
  ) |>
  dplyr::filter(stringr::str_detect(prov_terr, "YT")) |> # any cell that intersects Yukon
  tidyr::pivot_longer(
    dplyr::ends_with("_population"),
    names_to = "type",
    values_to = "total"
  ) |>
  dplyr::filter(total > 0) |>
  sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") |>
  dplyr::bind_rows(
    install_at |>
      dplyr::mutate(total = 1) |>
      dplyr::select(c("prov_terr", "fcst_zone", "type", "total"))
  ) |>
  dplyr::mutate(type = .data$type |> factor(levels = to_cover_types))

# Define existing monitors (Canadian PM2.5 monitoring network - NAPS, PurpleAir, and AQEgg networks)
existing_locations <- "https://aqmap.ca/aqmap/data/aqmap_most_recent_obs.csv" |>
  read.csv() |>
  tibble::as_tibble() |>
  dplyr::select(site_id = sensor_index, network, lng, lat, name = monitor) |>
  dplyr::mutate(network = factor(network, levels = c("FEM", "PA", "EGG"))) |>
  sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")

# Combine and store for use in tests
test_case <- list(
  install_at = install_at, # TODO: group by type
  to_cover = to_cover, # TODO: group by type
  existing_locations = existing_locations, # TODO: group by network
  cover_distance = units::set_units(10, "km"),
  weight_columns = c("ease_of_install", "total")
)

saveRDS(test_case, "tests/testthat/fixtures/test-case.rds")
