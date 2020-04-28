
# Function grobfun() generate the graphical objects with the coordinates x-y to plot
# in the background plot.
grobfun <- function(subplots, min_x, max_x, min_y, max_y) {
  ggplot2::annotation_custom(ggplot2::ggplotGrob(subplots),
    xmin = min_x, ymin = min_y,
    xmax = max_x, ymax = max_y
  )
}

subplots_annotated <- function(df_sub_plots_well, sp_data_layout) {
  df_sub_plots_well <- dplyr::mutate(df_sub_plots_well,
    grobs = purrr::pmap(
      subset(
        df_sub_plots_well,
        select = -c(Wells, Row, Column)
      ),
      grobfun
    )
  )
  df_sub_plots_well <- dplyr::inner_join(df_sub_plots_well,
    unique(sp_data_layout[, c("Wells", "condition_fc")]),
    by = "Wells"
  )
}
