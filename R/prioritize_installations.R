prioritize_installations <- function(
  install_at,
  to_cover,
  cover_distance = 25 |> units::set_units("km"),
  weight_columns = c(".weight", ".weight"),
  suffix = ""
) {
  new_columns <- c("newly_covered", "priority") |>
    paste0(suffix)
  if (nrow(install_at) == 0 | nrow(to_cover) == 0) {
    warning("No entries in either `install_at` or `to_cover`")
    return(
      install_at |>
        dplyr::mutate(
          !!new_columns[1] := 0,
          !!new_columns[2] := NA_integer_
        )
    )
  }

  # Handle weight columns
  if (length(weight_columns) == 1) {
    weight_columns <- rep(weight_columns, 2)
  }
  names(weight_columns) <- c("install_at_weight", "to_cover_weight")

  # Add default constant `install_at` weighting column if not already present
  if (!weight_columns[1] %in% names(install_at)) {
    warning(
      "No `weight_columns[1]` column found in `install_at`, assuming equal weighting of points to cover."
    )
    install_at <- install_at |>
      dplyr::mutate(!!weight_columns[1] := 1)
  }

  # Add default constant `to_cover` weighting column if not already present
  if (!weight_columns[2] %in% names(to_cover)) {
    warning(
      "No `weight_columns[2]` column found in `to_cover`, assuming equal weighting of installation locations."
    )
    to_cover <- to_cover |>
      dplyr::mutate(!!weight_columns[2] := 1)
  }

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
          !!new_columns[1] := 0,
          !!new_columns[2] := NA_integer_
        )
    )
  }

  # Get coverage information - will be altered as installations are placed
  install_at <- install_at |> dplyr::mutate(.id = dplyr::row_number())
  to_cover_groups <- as.character(dplyr::groups(to_cover))
  new_coverage <- install_at[coverages$install_at_id, ] |>
    dplyr::select(
      install_at_id = ".id",
      dplyr::any_of(weight_columns[1])
    ) |>
    handyr::sf_as_df(keep_coords = FALSE) |>
    dplyr::mutate(to_cover_id = coverages$to_cover_id) |>
    dplyr::left_join(
      to_cover |>
        dplyr::ungroup() |>
        dplyr::mutate(to_cover_id = dplyr::row_number()) |>
        dplyr::select(
          "to_cover_id",
          dplyr::any_of(c(weight_columns[2], to_cover_groups))
        ) |>
        handyr::sf_as_df(keep_coords = FALSE),
      by = "to_cover_id"
    )

  # Determine best coveraging monitor, store,
  #  then remove that monitor and those newly covered for next iteration
  priority <- list()
  for (i in seq_len(nrow(install_at))) {
    # Find best coverage
    best_coverage <- new_coverage |>
      dplyr::group_by(
        .data$install_at_id,
        dplyr::pick(dplyr::any_of(to_cover_groups))
      ) |>
      dplyr::summarise(
        to_cover_weight = sum(.data$to_cover_weight),
        weight = sum(.data$to_cover_weight * .data$install_at_weight),
        covered = list(.data$to_cover_id),
        .groups = "drop"
      )

    if (length(to_cover_groups) > 0) {
      best_coverage <- best_coverage |>
        tidyr::pivot_wider(
          names_from = dplyr::all_of(to_cover_groups),
          values_from = "to_cover_weight",
          names_glue = "newly_covered_{.name}",
          names_expand = TRUE # in case some factor levels are missing
        ) |>
        dplyr::group_by(.data$install_at_id) |>
        dplyr::summarise(
          weight = sum(.data$weight),
          covered = list(unlist(.data$covered)),
          dplyr::across(
            dplyr::starts_with("newly_covered_"),
            \(x) sum(x, na.rm = TRUE)
          ),
        ) |>
        dplyr::mutate(
          to_cover_weight = rowSums(dplyr::across(dplyr::starts_with(
            "newly_covered_"
          )))
        )
    }
    best_coverage <- best_coverage |>
      dplyr::arrange(dplyr::desc(.data$weight)) |>
      dplyr::slice(1)

    # Drop the ones that covers from new_coverage (for next run)
    new_coverage <- new_coverage |>
      dplyr::filter(!.data$to_cover_id %in% unlist(best_coverage$covered))

    # Place monitor where most covered
    priority[[i]] <- install_at |>
      dplyr::filter(.data$.id == best_coverage$install_at_id) |>
      dplyr::mutate(
        !!new_columns[1] := best_coverage$to_cover_weight
      ) |>
      dplyr::left_join(
        best_coverage |>
          dplyr::select("install_at_id", dplyr::starts_with("newly_covered_")),
        by = c(.id = "install_at_id")
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
        dplyr::mutate(!!new_columns[1] := 0)
    ) |>
    dplyr::select(-".id") |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("newly_covered_"), \(x) {
      handyr::swap(x, what = NA, with = 0)
    }))

  # add priorities
  priority |>
    dplyr::arrange(dplyr::desc(
      get(new_columns[1]) * get(new_columns[1])
    )) |>
    dplyr::mutate(
      !!new_columns[2] := (.data[[new_columns[1]]] == 0) |>
        ifelse(NA, dplyr::row_number())
    )
}
