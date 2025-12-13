gg_coverage_map <- function(coverage_areas, coverage_monitors, base_map, colours, layer_order) {
  capture.output(suppressMessages(
    gg <- base_map +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(0.01)) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(0.01)) +
      ggplot2::geom_sf(
        data = coverage_areas |> 
          dplyr::filter(type %in% layer_order) |> 
          dplyr::arrange(match(type, layer_order)), 
        ggplot2::aes(fill = type), 
        colour = "black", alpha = 1, linewidth = 0.1
      ) +
      ggplot2::geom_sf(
        data = coverage_monitors |>
          dplyr::filter(class %in% layer_order),
        ggplot2::aes(fill = class), 
        colour = "black", alpha = 1, 
        stroke = 0.1, shape = 21, size = 1,
        show.legend = FALSE, 
      ) +
      ggplot2::scale_fill_manual(values = colours)+
      # Canada-specific projection (warns about applying to exisitng scale)
      ggplot2::coord_sf(crs = "+proj=lcc +lon_0=-92 +lat_1=49 +lat_2=77") +
      # Add labels/theming
      ggplot2::labs(
        fill = "People/Communities Covered By",
        colour = "People/Communities Covered By",
        x = ggplot2::element_blank(), y = ggplot2::element_blank()
      ) +
      ggplot2::theme(
        legend.position = c(0.8065, 0.86),
        legend.title = ggplot2::element_text(hjust = 0.5),
        legend.background = ggplot2::element_blank(),
        # legend.background = element_rect(colour = 'black', linewidth = 0.2),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks.length = ggplot2::unit(0, "cm"),
        legend.spacing = ggplot2::unit(-0.75, "cm")
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(order = 2),
        fill = ggplot2::guide_legend(order = 1, override.aes = list(alpha = 1))
      )
  ))
  return(gg)
}
