test_that("test case works", {
  test_case <- test_path("fixtures", "test-case.rds") |>
    readRDS()

  # Find the optimal install locations
  optimized_locations <- test_case$install_at |>
    optimize_coverage(
      to_cover = test_case$to_cover,
      existing_locations = test_case$existing_locations,
      cover_distance = test_case$cover_distance,
      weight_columns = test_case$weight_columns
    )

  test_case$install_at |>
    plot_coverage(
      to_cover = test_case$to_cover,
      existing_locations = test_case$existing_locations,
      optimized_locations = optimized_locations,
      cover_distance = test_case$cover_distance,
      weight_columns = test_case$weight_columns |>
        stats::setNames(c(
          "Ease of Install\n(Community Type)",
          "2016 Population\n(To Cover)"
        )),
      in_canada = TRUE
    ) |>
    expect_s3_class("ggplot") |>
    expect_warning(
      "attribute variables are assumed to be spatially constant throughout all geometries"
    ) |> # TODO: fix?
    expect_silent()
})
