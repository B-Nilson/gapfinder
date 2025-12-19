#' Plot the coverage of a set of installations over a set of points.
#'
#' @param install_at An `sf` data frame containing locations of potential installations.
#' @param to_cover An `sf` data frame containing the locations desired to be covered by `install_at` within `cover_distance`.
#' @param existing_locations An `sf` data frame containing the locations of existing installations.
#'    Defaults to `NULL` - no existing locations will be plotted.
#' @param optimized_locations An `sf` data frame containing the optimized locations for installation.
#'   Defaults to `NULL` - no optimized locations will be plotted.
#' @param cover_distance The distance from an installation that a location is considered to be covered.
#'   Expected to be a `units` object, otherwise a `numeric` which is assumed to be in km.
#'   Defaults to 25 km.
#' @param weight_columns A character vector containing the names of the columns in `install_at` and `to_cover`
#'   that was used as weights when optimizing the coverage of each potential installation.
#'   Defaults to `NA` - no weights will be used.
#' @param background_map A `ggplot` object containing the base map of the plot.
#'   If provided, the coverage will be plotted on top of this map.
#'   Defaults to `NULL` - a blank plot will be used.
#' @param in_canada A logical indicating whether the coverage should be plotted over a map of Canada.
#'   If `TRUE`, a background map of Canada will be generated if `background_map` is `NULL`, and the plot will be projected using a Canada-specific projection.
#'   Defaults to `FALSE`.
#' @param select_prov_terrs A character vector containing the names (abbreviations or EN/FR names) of the provinces and territories to include in the map of Canada.
#'   Ignored if `in_canada` is `FALSE`.
#'   Defaults to `"all"` - meaning a bounding box will be calulated from the extent of the provided data for the map instead of filtering by province/territory.
#' @param colours A list containing the fill colours for coverage and installation locations.
#'   Must be length 4 and have names `to_cover`, `existing`, `proposed`, and `others`.
#'   Defaults to `list(to_cover = "#FC4E07", existing = "#00AFBB", proposed = "#E7B800", others = "black")`.
#' @param fill_labels A list containing the labels to display in the legend got the coloured layers.
#'   Must be length 4 and have names `to_cover`, `existing`, `proposed`, and `others`.
#'   Defaults to `list(to_cover = "To Cover", existing = "Existing", proposed = "Proposed", others = "Others")`.
#' @param fill_opacity A numeric between 0 and 1 indicating the opacity of the coverage layers.
#'   Defaults to 0.7.
#' @param point_shape The shape to use for the non-prioritized installation locations.
#'   Defaults to 21 (a filled circle).
#' @param stroke_colour The colour to use for strokes throughout the plot.
#' @return A `ggplot` object containing the coverage of the installations over the points.
#' @export
plot_coverage <- function(
  install_at,
  to_cover,
  existing_locations = NULL,
  optimized_locations = NULL,
  cover_distance = 25 |> units::set_units("km"),
  weight_columns = NA,
  background_map = NULL,
  in_canada = FALSE,
  select_prov_terrs = "all",
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
  if (length(weight_columns) == 1) {
    weight_columns <- rep(weight_columns, 2)
  }
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

    # Get existing coverage if any nearby
    if (nrow(existing_locations) == 0) {
      existing_locations <- NULL
      existing_coverage <- NULL
    } else {
      existing_coverage <- existing_locations |>
        sf::st_buffer(dist = cover_distance) |>
        sf::st_union()
    }
  }else {
    existing_coverage <- NULL
  }

  # Find added coverage if provided
  if (!is.null(optimized_locations)) {
    added_coverage <- optimized_locations |>
      sf::st_buffer(dist = cover_distance) |>
      sf::st_union()
  } else {
    added_coverage <- NULL
  }

  # Build background map if needed
  need_canada_tiles <- in_canada & is.null(background_map)
  if (need_canada_tiles) {
    if (select_prov_terrs == "all") {
      background_map <- install_at |>
        list(to_cover, existing_locations, optimized_locations) |>
        get_extent(.buffer_distance = cover_distance * 5) |>
        make_canada_map()
    } else {
      background_map <- make_canada_map(prov_terrs = select_prov_terrs)
    }
  } else if (is.null(background_map)) {
    background_map <- ggplot2::ggplot()
  }

  # Ensure data layers are same projection as map
  map_crs <- background_map$layers[[1]]$data |>
    sf::st_crs() |>
    handyr::on_error(.return = NA_character_)
  needs_reproject <- in_canada & !is.na(map_crs)
  if (needs_reproject) {
    install_at <- install_at |>
      sf::st_transform(crs = map_crs)
    to_cover <- to_cover |>
      sf::st_transform(crs = map_crs)
    if (!is.null(existing_locations)) {
      existing_locations <- existing_locations |>
        sf::st_transform(crs = map_crs)
      existing_coverage <- existing_coverage |>
        sf::st_transform(crs = map_crs)
    }
    if (!is.null(optimized_locations)) {
      optimized_locations <- optimized_locations |>
        sf::st_transform(crs = map_crs)
      added_coverage <- added_coverage |>
        sf::st_transform(crs = map_crs)
    }
  }

  background_map |>
    # Add layers to map
    add_coverage_layers(
      install_at = install_at,
      to_cover = to_cover,
      existing_locations = existing_locations,
      optimized_locations = optimized_locations,
      existing_coverage = existing_coverage,
      added_coverage = added_coverage,
      weight_columns = weight_columns,
      fill_labels = fill_labels,
      fill_opacity = fill_opacity,
      point_shape = point_shape,
      stroke_colour = stroke_colour
    ) |>
    # Adjust theming etc
    format_coverage_map(
      colours = colours,
      point_shape = point_shape,
      fill_labels = fill_labels,
      weight_columns = weight_columns
    )
}

get_extent <- function(..., .buffer_distance = 25 |> units::set_units("km")) {
  dplyr::bind_rows(...) |>
    sf::st_union() |>
    sf::st_convex_hull() |>
    sf::st_buffer(dist = .buffer_distance) |>
    sf::st_bbox()
}

make_canada_map <- function(bbox = NULL, prov_terrs = "all") {
  rlang::check_installed("canadata")
  background_map_data <- canadata::provinces_and_territories # TODO: cut out water
  if (prov_terrs != "all") {
    background_map_data <- background_map_data |>
      dplyr::filter(
        (.data$abbreviation %in% prov_terrs) |
          .data$name_en %in% prov_terrs |
          .data$name_fr %in% prov_terrs
      )
  }
  if (!is.null(bbox)) {
    background_map_data <- background_map_data |>
      sf::st_crop(bbox) |>
      suppressWarnings() # attribute variables are assumed to be spatially constant throughout all geometries
  }
  background_map_data <- background_map_data |>
    sf::st_transform(crs = "+proj=lcc +lon_0=-92 +lat_1=49 +lat_2=77")
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = background_map_data,
      fill = "#FFF9EB",
      colour = "black",
      linewidth = 0.2
    )
}

add_coverage_layers <- function(
  map,
  install_at,
  to_cover,
  existing_locations,
  optimized_locations,
  existing_coverage,
  added_coverage,
  weight_columns,
  fill_labels,
  fill_opacity,
  point_shape,
  stroke_colour
) {
  # Add locations to cover, scale size by weight column
  if (!is.na(weight_columns[2])) {
    to_cover <- to_cover |>
      dplyr::arrange(dplyr::desc(.data[[weight_columns[2]]]))
  }
  coverage_map <- map +
    ggplot2::geom_sf(
      data = to_cover,
      colour = stroke_colour,
      shape = point_shape,
      ggplot2::aes(
        size = if (!is.na(weight_columns[2])) get(weight_columns[2]) else 1,
        fill = fill_labels$to_cover
      ),
      alpha = 1,
      linewidth = 0.1
    )
  if (is.na(weight_columns[2])) {
    coverage_map <- coverage_map +
      ggplot2::guides(size = "none")
  }

  # Add existing and added coverage (if provided)
  if (!is.null(existing_locations)) {
    coverage_map <- coverage_map +
      ggplot2::geom_sf(
        shape = 21, # throws warning otherwise
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
          shape = if (!is.na(weight_columns[1])) {
            as.factor(get(weight_columns[1]))
          }else "",
          fill = fill_labels$proposed
        ),
        alpha = 1,
        linewidth = 0.1
      )
    if (is.na(weight_columns[2])) {
      coverage_map <- coverage_map +
        ggplot2::guides(shape = "none")
    }
  }
  return(coverage_map)
}

format_coverage_map <- function(
  coverage_map,
  point_shape,
  colours,
  fill_labels,
  weight_columns
) {
  axis_expand <- ggplot2::expansion(0.01)

  if (!is.na(weight_columns[2])) {
    size_guide <- ggplot2::guide_legend(
      order = 2,
      override.aes = list(shape = point_shape, fill = colours$to_cover)
    )
  } else {
    size_guide <- "none"
  }

  coverage_map +
    ggplot2::scale_fill_manual(
      values = unname(unlist(colours)),
      breaks = unname(unlist(fill_labels[names(colours)]))
    ) +
    ggplot2::scale_size_continuous(range = c(1, 3)) +
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
        colour = "grey",
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
      size = size_guide
    )
}
