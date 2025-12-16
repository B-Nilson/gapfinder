test_that("real case works", {
  test_case <- readRDS("tests/testthat/fixtures/test-case.rds")

  # Find the optimal install locations
  optimized_locations <- test_case$install_at |>
    optimize_coverage(
      to_cover = test_case$to_cover,
      existing_locations = test_case$existing_locations,
      cover_distance = test_case$cover_distance,
      weight_columns = test_case$weight_columns
    )

  # Prioritize installations and find newly added coverage if installed in priority order 
  population_types <- c("rural_population", "urban_population")
  prioritized_locations <- test_case$install_at |>
    prioritize_installations(
      to_cover = test_case$to_cover |> dplyr::filter(type %in% population_types) |> dplyr::group_by(type),
      cover_distance = test_case$cover_distance,
      weight_columns = test_case$weight_columns,
      suffix = "_population"
    ) |>
    prioritize_installations(
      to_cover = test_case$to_cover |> dplyr::filter(!type %in% population_types) |> dplyr::group_by(type),
      cover_distance = test_case$cover_distance,
      weight_columns = test_case$weight_columns,
      suffix = "_communities"
    ) |> 
    expect_silent() |> 
    expect_snapshot()
})
