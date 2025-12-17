#' Add identifiers marking the nearest locations and their distance to each point.
#'
#' This function marks the nearest location (id and distance) from a set of points to another set of points.
#' For locations within `within`, a flag is raised indicating that site has a location nearby.
#' If `to` is a grouped dataframe (see [dplyr::group_by]), the nearest location of each grouping is marked, as well as the normal overall ungrouped columns.
#' 
#' This function relies on unique row identifier columns for both `from` and `to`, proovided via the `from_id_col` and `to_id_col` arguments.
#' If no such column is present, a row identifier column is added with default name `".id"`, and will be removed before returning the output.
#'
#' @param from `sf` data frame containing the points to mark
#' @param to `sf` data frame containing the points to mark nearest to
#' @param within distance to mark nearest points within. Either a `units` object (see [units::set_units]), or a `numeric` in km
#' @param from_id_col,to_id_col column name in `from`/`to` containing unique row identifiers. If not present will be included (using [dplyr::row_number]) and removed within the function.
#' @return data frame with the nearest location in `to` to each location in `from` marked. 
#'   Columns `.nearest_to_id`, `.nearest_to_distance`, and `.has_nearby` will be added, 
#'   and if `to` is grouped, those will be repeated for each grouping as well.
#'   If `from` is grouped the results will not be different from ungrouped, but the grouping will be maintained.
#' @export
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
      from = from,
      to = to,
      group_values = to_group_values,
      from_id_col = from_id_col,
      to_id_col = to_id_col
    ) |>
    dplyr::arrange(.data[[from_id_col]])

  # Join back to `from` and cleanup
  result <- from |>
    # Ensure no duplicate columns from running the function repeatedly
    dplyr::select(-(dplyr::any_of(names(nearest_features)) & !dplyr::all_of(from_id_col))) |>
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
          y = to[.data$.nearest_to_id |> match(to[[to_id_col]]), ],
          by_element = TRUE
        ) |>
        units::set_units("km") |>
        round(digits = 1), # TODO: what if sub km data used?
      .has_nearby = .data$.nearest_to_distance <= within
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
  from,
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
