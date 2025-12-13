plot_coverage <- function(
  coverage_areas,
  coverage_monitors,
  base_map,
  colours,
  layer_order
) {
  axis_expand <- ggplot2::expansion(0.01)
  legend_title <- "People/Communities Covered By" # TODO: remove?
  legend_position <- c(0.8065, 0.86) # TODO: remove?
  base_map <- ggplot2::ggplot() + # TODO: create
    # Canada-specific projection
    ggplot2::coord_sf(  # TODO: remove?
      crs = "+proj=lcc +lon_0=-92 +lat_1=49 +lat_2=77",
      default = TRUE
    )

  coverage_map <- base_map +
    ggplot2::geom_sf(
      data = coverage_areas |>
        dplyr::filter(type %in% layer_order) |>
        dplyr::arrange(match(type, layer_order)),
      ggplot2::aes(fill = type),
      colour = "black",
      alpha = 1,
      linewidth = 0.1
    ) +
    ggplot2::geom_sf(
      data = coverage_monitors |>
        dplyr::filter(class %in% layer_order),
      ggplot2::aes(fill = class),
      colour = "black",
      alpha = 1,
      stroke = 0.1,
      shape = 21,
      size = 1,
      show.legend = FALSE,
    )

  coverage_map +
    ggplot2::scale_x_continuous(expand = axis_expand) +
    ggplot2::scale_y_continuous(expand = axis_expand) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::labs(
      fill = legend_title,
      colour = legend_title,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme(
      legend.position = legend_position,
      legend.title = ggplot2::element_text(hjust = 0.5),
      legend.background = NULL,
      axis.ticks = NULL,
      axis.title = NULL,
      axis.text = NULL,
      axis.ticks.length = ggplot2::unit(0, "cm"),
      legend.spacing = ggplot2::unit(-0.75, "cm")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1, override.aes = list(alpha = 1)),
      colour = ggplot2::guide_legend(order = 2)
    )

  return(gg)
}
