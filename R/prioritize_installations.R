prioritize_installations <- function(
  install_at,
  to_cover,
  cover_distance = 25 |> units::set_units("km"),
  weight_columns = c(".weight", ".weight"),
  suffix = ""
) {
  if (nrow(install_at) == 0 | nrow(to_cover) == 0) {
    warning("No entries in either `install_at` or `to_cover`")
    return(
      install_at |>
        dplyr::mutate(
          !!paste0("newly_covered", suffix) := 0,
          !!paste0("priority", suffix) := NA_integer_
        )
    )
  }

  # Handle weight columns
  if (length(weight_columns) == 1) {
    weight_columns <- rep(weight_columns, 2)
  }
  if (!weight_columns[1] %in% names(to_cover)) {
    to_cover[weight_columns[1]] <- 1
  }
  if (!weight_columns[2] %in% names(install_at)) {
    install_at[weight_columns[2]] <- 1
  }
  names(weight_columns) <- c("to_cover_weight", "install_at_weight")

  # Determine which "to_cover" are in range of each `install_at`
  coverages <- to_cover |>
    get_covered(
      install_at = install_at,
      cover_distance = cover_distance
    )

  # Handle no coverage
  if (nrow(coverages) == 0) {
    warning("No coverage from any `install_at`")
    return(
      install_at |>
        dplyr::mutate(
          !!paste0("newly_covered", suffix) := 0,
          !!paste0("priority", suffix) := NA_integer_
        )
    )
  }

  # Get coverage information - will be altered as installations are placed
  install_at <- install_at |> dplyr::mutate(.id = dplyr::row_number())
  new_coverage <- install_at[coverages$install_at_id, ] |>
    dplyr::select(
      install_at_id = ".id",
      dplyr::any_of(weight_columns[2])
    ) |>
    handyr::sf_as_df(keep_coords = FALSE) |>
    dplyr::mutate(to_cover_id = coverages$to_cover_id) |>
    dplyr::left_join(
      to_cover |>
        dplyr::mutate(to_cover_id = dplyr::row_number()) |>
        dplyr::select("to_cover_id", dplyr::any_of(weight_columns[1])) |>
        handyr::sf_as_df(keep_coords = FALSE),
      by = "to_cover_id"
    )

  # Determine best coveraging monitor, store,
  #  then remove that monitor and those newly covered for next iteration
  priority <- list()
  for (i in seq_len(nrow(install_at))) {
    # Find best coverage
    best_coverage <- new_coverage |>
      dplyr::group_by(.data$install_at_id) |>
      dplyr::summarise(
        to_cover_weight = sum(.data$to_cover_weight),
        weight = sum(.data$to_cover_weight * .data$install_at_weight),
        covered = list(.data$to_cover_id),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(.data$weight)) |>
      dplyr::slice(1)

    # Drop the ones that covers from new_coverage (for next run)
    new_coverage <- new_coverage |>
      dplyr::filter(!.data$to_cover_id %in% unlist(best_coverage$covered))

    # Place monitor where most covered
    priority[[i]] <- install_at |>
      dplyr::filter(.data$.id == best_coverage$install_at_id) |>
      dplyr::mutate(
        !!paste0("newly_covered", suffix) := best_coverage$to_cover_weight
      )

    # Stop if no more coverage
    if (nrow(new_coverage) == 0) {
      break
    }
  }
  priority <- priority |>
    dplyr::bind_rows()

  # Append remaining install_at
  priority <- priority |>
    dplyr::bind_rows(
      install_at |>
        dplyr::filter(!.data$.id %in% priority$.id) |>
        dplyr::mutate(!!paste0("newly_covered", suffix) := 0)
    ) |>
    dplyr::select(-".id")

  # add priorities
  priority |>
    dplyr::arrange(dplyr::desc(
      get(paste0("newly_covered", suffix)) * get(weight_columns[2])
    )) |>
    dplyr::mutate(
      !!paste0("priority", suffix) := (.data[[paste0(
        "newly_covered",
        suffix
      )]] ==
        0) |>
        ifelse(NA, dplyr::row_number())
    )
}
