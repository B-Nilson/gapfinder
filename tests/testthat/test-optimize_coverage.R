test_that("test case works", {
  test_case <- test_path("fixtures", "test-case.rds") |>
    readRDS()

  optimized_locations <- test_case$install_at |>
    optimize_coverage(
      to_cover = test_case$to_cover,
      existing_locations = test_case$existing_locations,
      cover_distance = test_case$cover_distance,
      weight_columns = test_case$weight_columns
    ) |>
    expect_silent()
})
