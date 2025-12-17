test_that("test case works", {
  test_case <- test_path("fixtures", "test-case.rds") |>
    readRDS()

  test_case$to_cover |>
    mark_nearest_location(
      to = test_case$install_at,
      within = test_case$cover_distance
    ) |>
    expect_silent() |>
    expect_snapshot()
})

test_that("grouped data works", {
  test_case <- test_path("fixtures", "test-case.rds") |>
    readRDS()

  # grouped `from`
  test_case$to_cover |>
    dplyr::group_by(type) |>
    mark_nearest_location(
      to = test_case$install_at,
      within = test_case$cover_distance
    ) |>
    expect_silent() |>
    expect_snapshot()

  # grouped `to`
  test_case$to_cover |>
    mark_nearest_location(
      to = test_case$install_at |>
        dplyr::group_by(type),
      within = test_case$cover_distance
    ) |>
    expect_silent() |>
    expect_snapshot()

  # grouped `to` and `from`
  test_case$to_cover |>
    dplyr::group_by(type) |>
    mark_nearest_location(
      to = test_case$install_at |>
        dplyr::group_by(type),
      within = test_case$cover_distance
    ) |>
    expect_silent() |>
    expect_snapshot()
})

test_that("repeat calls handled correctly", {
  test_case <- test_path("fixtures", "test-case.rds") |>
    readRDS()

  once <- test_case$to_cover |>
    dplyr::group_by(type) |>
    mark_nearest_location(
      to = test_case$install_at |>
        dplyr::group_by(type),
      within = test_case$cover_distance
    ) |>
    expect_silent()
  
  twice <- once |>
    dplyr::group_by(type) |>
    mark_nearest_location(
      to = test_case$install_at |>
        dplyr::group_by(type),
      within = test_case$cover_distance
    ) |>
    expect_silent() 

  expect_identical(once, twice)
})