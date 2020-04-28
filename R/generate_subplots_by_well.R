
 generate_subplots_by_well <- function(sp_data_layout, var_to_col) {
  colScale <- generate_color_scale(sp_data_layout, var_to_col)
  # generate subplots

  sp_data_layout_group <- dplyr::group_by(sp_data_layout, Wells)
  df_sub_plots_well <- dplyr::do(sp_data_layout_group,
    subplots = ggplot2::ggplot(., ggplot2::aes(x = Time, y = mu)) +
      ggplot2::geom_line(ggplot2::aes(colour = condition_fc), size = 1) +
      colScale +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "none",
        panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 0.5)
      )
  )
  return(df_sub_plots_well)
}

# generte color scale
generate_color_scale <- function(sp_data_layout, var_to_col) {
  colors_fact <- rev(viridis::viridis(length(unique(sp_data_layout[[var_to_col]]))))
  names(colors_fact) <- levels(sp_data_layout[["condition_fc"]])
  colScale <- ggplot2::scale_colour_manual(name = "condition_fc", values = colors_fact)
  return(colScale)
}
