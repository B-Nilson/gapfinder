test_that("test case works", {
  test_case <- test_path("fixtures", "test-case.rds") |>
    readRDS()

  get_covered(
    install_at = test_case$install_at,
    to_cover = test_case$to_cover,
    cover_distance = test_case$cover_distance
  ) |>
    expect_silent() |>
    print() |>
    expect_snapshot()
})
