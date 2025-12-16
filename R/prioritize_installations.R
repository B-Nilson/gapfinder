prioritize_installations <- function(
  install_at,
  to_cover,
  cover_distance = 25 |> units::set_units("km"),
  suffix = ""
) {
  if (nrow(install_at) == 0 | nrow(to_cover) == 0) {
    return(NULL)
  }

  # Determine which "to_cover" are in range of each `install_at`
  coverages <- to_cover |>
    get_covered(install_at = install_at, cover_distance = cover_distance)

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
  new_coverage <- install_at[coverages$install_at_id, ] |>
    dplyr::select(install_at_id = id, install_at_type = type) |>
    handyr::sf_as_df(keep_coords = FALSE) |>
    dplyr::mutate(to_cover_id = coverages$to_cover_id) |>
    dplyr::left_join(
      to_cover |>
        dplyr::mutate(to_cover_id = dplyr::row_number()) |>
        dplyr::select(to_cover_id, to_cover_type = type, total) |>
        handyr::sf_as_df(keep_coords = FALSE),
      by = "to_cover_id"
    )

  # Determine best coveraging monitor, store,
  #  then remove that monitor and those newly covered for next iteration
  priority <- list()
  for (i in seq_len(nrow(install_at))) {
    # Find best coverage
    best_coverage <- new_coverage |>
      dplyr::group_by(install_at_id, install_at_type) |>
      dplyr::summarise(
        total = sum(total),
        covered = list(to_cover_id),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(total)) |>
      dplyr::slice(1)

    # Drop the ones that covers from new_coverage (for next run)
    new_coverage <- new_coverage |>
      dplyr::filter(!to_cover_id %in% unlist(best_coverage$covered))

    # Place monitor where most covered
    priority[[i]] <- install_at |>
      dplyr::filter(id == best_coverage$install_at_id) |>
      dplyr::mutate(newly_covered = best_coverage$total)

    # Stop if no more coverage
    if (nrow(new_coverage) == 0) {
      break
    }
  }
  priority <- dplyr::bind_rows(
    priority,
    # Append remaining install_at
    install_at |>
      dplyr::filter(!id %in% priority$id) |>
      dplyr::mutate(!!paste0("newly_covered", suffix) := 0)
  )

  # add priorities for each resolution
  new_cols <- c("newly_covered", "priority_provterr", "priority_canada")
  priority |>
    # P/T priority
    dplyr::arrange(prov_terr, desc(newly_covered)) |>
    dplyr::mutate(
      priority_provterr = ifelse(newly_covered == 0, NA, dplyr::row_number()),
      .by = "prov_terr"
    ) |>
    # Nationwide priority
    dplyr::arrange(desc(newly_covered)) |>
    dplyr::mutate(
      priority_canada = ifelse(newly_covered == 0, NA, dplyr::row_number())
    ) |>
    dplyr::rename_with(
      \(col_name) paste0(col_name, suffix),
      .cols = dplyr::all_of(new_cols)
    )
}