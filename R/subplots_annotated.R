# Generate dataframe that contain the subplots with coordinates as grobs.
#
# The function takes as input the output data frame of generate_subplots_with_coordinates(),
# and is adding the coordinates to subplots and generating grobs.
#
# Parameters are df_sub_plots_well dataframe.
#
# Return the input dataframe with the the column grobs, which is a layerinstance
# containing the subplots and the coordinates to placed it in the background plot.
subplots_annotated <- function(df_sub_plots_well, sp_data_layout) {
  Wells <- Row <- Column <- NULL # defined as global variables
  df_sub_plots_well <- dplyr::mutate(df_sub_plots_well,
    grobs = purrr::pmap(#loop through rows
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

# Function grobfun() generate the graphical objects with the coordinates x-y to plot
# in the background plot.
grobfun <- function(subplots, min_x, max_x, min_y, max_y) {
  ggplot2::annotation_custom(ggplot2::ggplotGrob(subplots),
                             xmin = min_x, ymin = min_y,
                             xmax = max_x, ymax = max_y
  )
}
