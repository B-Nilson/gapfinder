plot_coverage <- function(
  install_at,
  to_cover,
  existing_locations = NULL,
  optimized_locations = NULL,
  cover_distance = 25 |> units::set_units("km"),
  weight_columns = c(".weight", ".weight"),
  background_map = NULL,
  in_canada = FALSE,
  colours = list(
    to_cover = "#FC4E07",
    existing = "#00AFBB",
    proposed = "#E7B800",
    others = "black"
  ),
  fill_labels = list(
    to_cover = "To Cover",
    existing = "Existing",
    proposed = "Proposed",
    others = "Others"
  ),
  fill_opacity = 0.7,
  point_shape = 21,
  stroke_colour = "black"
) {
  # Ensure weight_columns has names
  if (is.null(names(weight_columns))) {
    names(weight_columns) <- weight_columns
  }

  # Find existing coverage if provided
  if (!is.null(existing_locations)) {
    # Filter out existing locations too far to contribute
    to_cover <- to_cover |>
      dplyr::mutate(.id = dplyr::row_number())
    existing_locations <- existing_locations |>
      dplyr::mutate(.id = dplyr::row_number())
    coverage <- existing_locations |>
      get_covered(to_cover = to_cover, cover_distance = cover_distance)
    provides_coverage <- coverage |>
      dplyr::pull("install_at_id") |>
      unique()
    existing_locations <- existing_locations[
      existing_locations$.id %in% provides_coverage,
    ]

    # Get existing coverage
    existing_coverage <- existing_locations |>
      sf::st_buffer(dist = cover_distance) |>
      sf::st_union()
  }

  # Find added coverage if provided
  if (!is.null(optimized_locations)) {
    added_coverage <- optimized_locations |>
      sf::st_buffer(dist = cover_distance) |>
      sf::st_union()
  }

  # Build background map if needed
  if (in_canada & is.null(background_map)) {
    rlang::check_installed("canadata")
    background_map_data <- canadata::provinces_and_territories |>
      sf::st_crop(sf::st_bbox(
        dplyr::bind_rows(
          install_at,
          to_cover,
          existing_locations,
          optimized_locations
        ) |>
          sf::st_convex_hull() |>
          sf::st_buffer(dist = cover_distance * 5)
      ))
    background_map <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = background_map_data,
        fill = "#FFF9EB",
        colour = "black",
        linewidth = 0.2
      )
  } else if (is.null(background_map)) {
    background_map <- ggplot2::ggplot()
  }

  background_map |>
    # Add layers to map
    add_coverage_layers(
      to_cover = to_cover,
      existing_coverage = existing_coverage,
      added_coverage = added_coverage,
      weight_columns = weight_columns,
      fill_labels = fill_labels,
      fill_opacity = fill_opacity,
      point_shape = point_shape,
      stroke_colour = stroke_colour
    ) |> 
    # Adjust theming, projection, etc
    format_coverage_map(
      colours = colours,
      fill_labels = fill_labels,
      weight_columns = weight_columns,
      in_canada = in_canada
    )
}

add_coverage_layers <- function(
  map,
  to_cover,
  existing_coverage,
  added_coverage,
  weight_columns,
  fill_labels,
  fill_opacity,
  point_shape,
  stroke_colour
) {
  # Add locations to cover, scale size by weight column
  coverage_map <- map +
    ggplot2::geom_sf(
      data = to_cover,
      colour = stroke_colour,
      shape = point_shape,
      ggplot2::aes(size = get(weight_columns[2]), fill = fill_labels$to_cover),
      alpha = 1,
      linewidth = 0.1
    )

  # Add existing and added coverage (if provided)
  if (!is.null(existing_locations)) {
    coverage_map <- coverage_map +
      ggplot2::geom_sf(
        data = existing_coverage,
        colour = stroke_colour,
        ggplot2::aes(fill = fill_labels$existing),
        stroke = 0.1,
        linewidth = 0.1,
        alpha = fill_opacity,
        show.legend = FALSE,
      )
  }
  if (!is.null(optimized_locations)) {
    coverage_map <- coverage_map +
      ggplot2::geom_sf(
        data = added_coverage,
        colour = stroke_colour,
        ggplot2::aes(fill = fill_labels$proposed),
        stroke = 0.1,
        linewidth = 0.1,
        alpha = fill_opacity,
        show.legend = FALSE,
      )
  }

  # Add other locations (could be installed at, but not in optimized locations)
  coverage_map <- coverage_map +
    ggplot2::geom_sf(
      data = install_at,
      colour = stroke_colour,
      ggplot2::aes(fill = fill_labels$others),
      stroke = 0.1,
      size = 1,
      shape = 21
    )

  # Add existing and optimized locations
  if (!is.null(existing_locations)) {
    coverage_map <- coverage_map +
      ggplot2::geom_sf(
        data = existing_locations,
        colour = stroke_colour,
        ggplot2::aes(fill = fill_labels$existing),
        shape = point_shape,
        alpha = 1,
        linewidth = 0.1
      )
  }
  if (!is.null(optimized_locations)) {
    coverage_map <- coverage_map +
      ggplot2::geom_sf(
        data = optimized_locations,
        colour = stroke_colour,
        ggplot2::aes(
          shape = as.factor(get(weight_columns[1])),
          fill = fill_labels$proposed
        ),
        alpha = 1,
        linewidth = 0.1
      )
  }
  return(coverage_map)
}

format_coverage_map <- function(coverage_map, colours, fill_labels, weight_columns, in_canada = FALSE) {
  axis_expand <- ggplot2::expansion(0.01)
  coverage_map_pretty <- coverage_map +
    ggplot2::scale_fill_manual(
      values = unname(unlist(colours)),
      breaks = unname(unlist(fill_labels[names(colours)]))
    ) +
    ggplot2::scale_shape_manual(values = 21:25) +
    ggplot2::scale_x_continuous(expand = axis_expand) +
    ggplot2::scale_y_continuous(expand = axis_expand) +
    # ggplot2::scale_fill_manual(values = colours) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = "Locations + Coverage",
      shape = names(weight_columns)[1],
      size = names(weight_columns)[2]
    ) +
    ggpubr::theme_pubr(border = TRUE) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        colour = "black",
        linewidth = 0.1
      ),
      legend.position = "right",
      legend.direction = "vertical",
      legend.background = ggplot2::element_blank(),
      axis.ticks = NULL,
      axis.title = NULL,
      axis.text = NULL,
      axis.ticks.length = ggplot2::unit(0, "cm") #,
      # legend.spacing = ggplot2::unit(-0.75, "cm")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        order = 1,
        override.aes = list(alpha = 1, shape = point_shape)
      ),
      size = ggplot2::guide_legend(
        order = 2,
        override.aes = list(shape = point_shape, fill = colours$to_cover)
      )
    )
  if (in_canada) {
    coverage_map_pretty <- coverage_map_pretty +
      # Canada-specific projection # TODO: remove?
      ggplot2::coord_sf(
        crs = "+proj=lcc +lon_0=-92 +lat_1=49 +lat_2=77",
        default = TRUE
      )
  }
}
