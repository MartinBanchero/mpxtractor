# Generates background plot and combine with subplots
#' @importFrom rlang .data
# First the background plot is created, this is the microplate frame. Over the
# frame the grobs objects in the input dataframe are added.
#
# Parameters are the dataframe with all the subplots, the exp_title to add title,
# cvar_to_col is the attribute to color.
#
# Return a list with all the plots.
combine_subplots_backgr <- function(df_sub_plots_well, exp_title, var_to_col) {
  n_col <- length(unique(df_sub_plots_well[["Column"]]))
  n_row <- length(unique(df_sub_plots_well[["Row"]]))
  color_con <- color_factor_condition(df_sub_plots_well)

  background_plot <- ggplot2::ggplot(
    data = df_sub_plots_well,
    ggplot2::aes(
      x = .data$Column,
      y = .data$Row,
      fill = .data$condition_fc
    )
  ) +
    ggplot2::geom_blank() +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect("black",
      fill = NA,
      size = 1
    ), ) +
    ggplot2::scale_x_continuous(
      breaks = seq(1, n_col),
      position = "top",
      limits = c(1, n_col + 0.2)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(1, n_row),
      labels = LETTERS[n_row:1], position = "left",
      limits = c(0.8, n_row + 0.1)
    ) +
    ggplot2::geom_col(ggplot2::aes(Inf, Inf)) +
    ggplot2::scale_fill_manual(
      name = "condition",
      values = color_con[["values"]]
    ) +
    ggplot2::labs(title = exp_title) +
    ggplot2::theme(
      legend.box = "vertical",
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(color = "black", size = 10),
      axis.text.y = ggplot2::element_text(color = "black", size = 10),
      axis.title = ggplot2::element_text(size = 10)
    )
  all_wells_plot <- background_plot + df_sub_plots_well[["grobs"]]
}


color_factor_condition <- function(df_sub_plots_well) {
  colors_fact_cond <- rev(viridis::viridis(
    length(unique(df_sub_plots_well[["condition_fc"]]))
  ))
  names(colors_fact_cond) <- levels(df_sub_plots_well[["condition_fc"]])
  color_con <- utils::stack(colors_fact_cond)
}
