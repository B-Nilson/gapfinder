# TODO: Handle new columns already existing (like if the function were called a second time with the same data)
mark_nearest_location <- function(
  from,
  to,
  within = Inf |> units::set_units("km"),
  from_id_col = ".id",
  to_id_col = ".id"
) {
  to_groups <- to |> dplyr::groups() |> as.character()
  if (length(to_groups) > 0) {
    to_group_values <- to |>
      dplyr::group_keys() |>
      tidyr::unite("values", dplyr::everything(), sep = "_") |>
      dplyr::pull("values")
  } else {
    to_group_values <- NULL
  }

  need_to_add_from_id <- !from_id_col %in% names(from)
  if (need_to_add_from_id) {
    from <- from |>
      dplyr::mutate(!!from_id_col := seq_len(nrow(from)))
  }
  if (!to_id_col %in% names(to)) {
    to <- to |>
      dplyr::mutate(!!to_id_col := seq_len(nrow(to)))
  }

  nearest_features <- to |>
    dplyr::group_split() |>
    handyr::for_each(.join = TRUE, .show_progress = FALSE, \(to_group_data) {
      nearest <- from |>
        find_nearest(
          to = to_group_data,
          from_id_col = from_id_col,
          to_id_col = to_id_col
        ) |>
        # Add group columns if present
        dplyr::left_join(
          to_group_data |>
            as.data.frame() |>
            dplyr::select(dplyr::any_of(c(to_id_col, to_groups))),
          by = c(.nearest_to_id = to_id_col)
        ) |>
        dplyr::mutate(
          .nearest_to_distance = from[get(from_id_col), ] |>
            sf::st_distance(
              to[match(.nearest_to_id, to[[to_id_col]]), ],
              by_element = TRUE
            ) |>
            units::set_units("km") |>
            round(digits = 1),
          .has_nearby = .nearest_to_distance <= within
        )
      if (length(to_groups) > 0) {
        nearest <- nearest |>
          tidyr::pivot_wider(
            names_from = dplyr::any_of(to_groups),
            values_from = c(.nearest_to_id, .nearest_to_distance, .has_nearby)
          )
      }
      return(nearest)
    }) |>
    dplyr::mutate(
      !!paste0(c(".nearest_to_id", to_group_values), collapse = "_") := from |>
        find_nearest(
          to = dplyr::ungroup(to),
          from_id_col = from_id_col,
          to_id_col = to_id_col
        ) |>
        dplyr::pull(.nearest_to_id),
      !!paste0(
        c(".nearest_to_distance", to_group_values),
        collapse = "_"
      ) := do.call(
        pmin,
        c(
          dplyr::across(dplyr::starts_with(".nearest_to_distance")),
          na.rm = TRUE
        )
      ),
      !!paste0(
        c(".has_nearby", to_group_values),
        collapse = "_"
      ) := rowSums(dplyr::across(
        dplyr::starts_with(".has_nearby")
      )) >
        0
    ) |>
    dplyr::arrange(.data[[from_id_col]])

  result <- from |> 
    dplyr::left_join(nearest_features, by = from_id_col)

  if (need_to_add_from_id) {
    result <- result |>
      dplyr::select(-dplyr::all_of(from_id_col))
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