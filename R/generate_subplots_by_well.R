# Generate dataframe that contain one growth rate plot for each well
#
# The function takes as input the data frame with the raw data and the layout
# combine, also the color of the condition to be colored. Then generate a dataframe
# with one column with the plots for each well stored as S3 objects.
#
# Parameters ar sp_data_layout dataframe and cond_to_col which is the attribute
# in the dataframe to be coloured.
#
# Return the input dataframe with the column subplots
generate_subplots_by_well <- function(sp_data_layout, cond_to_col) {
  colScale <- generate_color_scale(sp_data_layout, cond_to_col)

  max_y <- max(sp_data_layout$growth_rate)
  min_y <- min(sp_data_layout$growth_rate)
  # generate subplots
  sp_data_layout_group <- dplyr::group_by(sp_data_layout, .data$Wells)
  df_sub_plots_well <- dplyr::do(sp_data_layout_group,
    subplots = ggplot2::ggplot(.data, ggplot2::aes(
      x = .data$Time,
      y = .data$growth_rate
    )) +
      ggplot2::geom_line(ggplot2::aes(colour = .data$condition_fc), size = 1) +
      ggplot2::ylim(min_y, max_y) +
      colScale +
      ggplot2::theme(
        legend.position = "none",
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(color = "black", size = 5),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(
          colour = "black",
          fill = NA,
          size = 0.5,
        )
      ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dotted")
  )
  return(df_sub_plots_well)
}

# generate color scale
generate_color_scale <- function(sp_data_layout, cond_to_col) {
  colors_fact <- rev(viridis::viridis(length(unique(sp_data_layout[[cond_to_col]]))))
  names(colors_fact) <- levels(sp_data_layout[["condition_fc"]])
  colScale <- ggplot2::scale_colour_manual(name = "condition_fc", values = colors_fact)
  return(colScale)
}
