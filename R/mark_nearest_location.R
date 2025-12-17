# TODO: Handle new columns already existing (like if the function were called a second time with the same data)
mark_nearest_location <- function(
  from,
  to,
  within = Inf |> units::set_units("km"),
  from_id_col = ".id",
  to_id_col = ".id"
) {
  # Handle `from`/`to` being grouped
  from_groups <- from |> dplyr::groups() |> as.character()
  from <- from |> dplyr::ungroup()
  to_groups <- to |> dplyr::groups() |> as.character()
  if (length(to_groups) > 0) {
    to_group_values <- to |>
      dplyr::group_keys() |>
      tidyr::unite("values", dplyr::everything(), sep = "_") |>
      dplyr::pull("values")
    to <- to |> dplyr::ungroup()
  } else {
    to_group_values <- NULL
  }

  # Handle id columns not in data
  need_to_add_from_id <- !from_id_col %in% names(from)
  if (need_to_add_from_id) {
    from <- from |>
      dplyr::mutate(!!from_id_col := dplyr::row_number())
  }
  if (!to_id_col %in% names(to)) {
    to <- to |>
      dplyr::mutate(!!to_id_col := dplyr::row_number())
  }

  nearest_features <- to |>
    # For each group in `to`,
    dplyr::group_by(dplyr::pick(dplyr::any_of(to_groups))) |>
    dplyr::group_split() |>
    # Find the nearest location in `from` and mark its ID, distance, and whether it is within `within`
    handyr::for_each(
      add_nearby_to_columns,
      from = from,
      from_id_col = from_id_col,
      to_id_col = to_id_col,
      within = within,
      to_groups = to_groups,
      .join = TRUE,
      .show_progress = FALSE
    ) |>
    # Include ungrouped (overall) nearby columns
    summarise_nearest_groups(
      to = to,
      group_values = to_group_values,
      from_id_col = from_id_col,
      to_id_col = to_id_col
    ) |>
    dplyr::arrange(.data[[from_id_col]])

  # Join back to `from` and cleanup
  result <- from |>
    dplyr::left_join(nearest_features, by = from_id_col)
  if (need_to_add_from_id) {
    # Remove added id column
    result <- result |>
      dplyr::select(-dplyr::all_of(from_id_col))
  }
  if (length(from_groups)) {
    # Add groups back in if originally included
    result <- result |>
      dplyr::group_by(dplyr::pick(dplyr::any_of(from_groups)))
  }
  return(result)
}

# Function to locate and mark nearest PA and FEM to each location
find_nearest <- function(from, to, from_id_col = ".id", to_id_col = ".id") {
  nearest_to_each_from <- from |>
    sf::st_nearest_feature(to)
  from |>
    as.data.frame() |>
    dplyr::select(dplyr::all_of(from_id_col)) |>
    dplyr::mutate(
      .nearest_to_id = to[[to_id_col]][nearest_to_each_from]
    )
}

add_nearby_to_columns <- function(
  from,
  to,
  from_id_col,
  to_id_col,
  within,
  to_groups
) {
  new_columns <- c(".nearest_to_id", ".nearest_to_distance", ".has_nearby")
  to_id_and_groups <- to |>
    as.data.frame() |>
    dplyr::select(dplyr::any_of(c(to_id_col, to_groups)))

  nearest <- from |>
    # Add .nearest_to_id column
    find_nearest(to = to, from_id_col = from_id_col, to_id_col = to_id_col) |>
    # Join on that column to include groups if provided
    dplyr::left_join(to_id_and_groups, by = c(.nearest_to_id = to_id_col)) |>
    # Add other columns
    dplyr::mutate(
      .nearest_to_distance = from[.data[[from_id_col]], ] |>
        sf::st_distance(
          y = to[.nearest_to_id |> match(to[[to_id_col]]), ],
          by_element = TRUE
        ) |>
        units::set_units("km") |>
        round(digits = 1), # TODO: what if sub km data used?
      .has_nearby = .nearest_to_distance <= within
    )

  # Widen by groups if present
  if (length(to_groups) > 0) {
    nearest <- nearest |>
      tidyr::pivot_wider(
        names_from = dplyr::any_of(to_groups),
        values_from = new_columns
      )
  }
  return(nearest)
}

summarise_nearest_groups <- function(
  nearest_features,
  to,
  group_values = NULL,
  from_id_col,
  to_id_col
) {
  if (is.null(group_values)) {
    return(nearest_features)
  }
  grouped_columns <- c(
    ".nearest_to_id",
    ".nearest_to_distance",
    ".has_nearby"
  ) |>
    paste(sep = "_", group_values |> paste(collapse = "_"))
  nearest_features |>
    dplyr::mutate(
      !!grouped_columns[1] := from |>
        find_nearest(
          to = dplyr::ungroup(to),
          from_id_col = from_id_col,
          to_id_col = to_id_col
        ) |>
        dplyr::pull(".nearest_to_id"),
      !!grouped_columns[2] := pmin |>
        do.call(c(
          dplyr::across(dplyr::starts_with(".nearest_to_distance")),
          na.rm = TRUE
        )),
      !!grouped_columns[3] := rowSums(dplyr::across(
        dplyr::starts_with(".has_nearby")
      )) >
        0
    )
}
