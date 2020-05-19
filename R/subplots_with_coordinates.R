# Generate dataframe that contain the coordinates where the subplots are placed
#
# The function takes as input the output data frame of generate_subplots_by_well(),
# and is adding to this the coordinates where the subplots are placed in the background
# plot.
#' @importFrom rlang .data
#
# Parameters are df_sub_plots_well dataframe.
# Return the input dataframe with the coordinates min_x, max_x and min_y and max_y
# as columns.
subplots_with_coordinates <- function(df_sub_plots_well) {
  df_sub_plots_well <- add_rows_and_columns(df_sub_plots_well)
  df_sub_plots_well$Row <- rev(df_sub_plots_well[["Row"]])


  df_sub_plots_well$group_x <- cut_into_coordinates(
    df_sub_plots_well[["Column"]],
    length(unique(df_sub_plots_well[["Column"]]))
  )
  df_sub_plots_well$group_y <- cut_into_coordinates(
    df_sub_plots_well[["Row"]],
    length(unique(df_sub_plots_well[["Row"]]))
  )

  df_sub_plots_well <- tidyr::separate(df_sub_plots_well, .data$group_x,
    into = c("min_x", "max_x"),
    sep = ",", convert = TRUE
  )

  df_sub_plots_well <- tidyr::separate(df_sub_plots_well, .data$group_y,
    into = c("min_y", "max_y"),
    sep = ",", convert = TRUE
  )
}


# Function cut_into_coordinates() to generate the coordinates x-y for each subplot.
cut_into_coordinates <- function(variable, ngroups) {
  seq_all <- seq(min(variable) - 0.5, max(variable) + 0.5, by = 1)
  cut(variable,
    breaks = seq_all,
    labels = paste(seq_all[-(ngroups + 1)], seq_all[-1], sep = ","),
    include.lowest = TRUE
  )
}

add_rows_and_columns <- function(df_sub_plots_well) {
  # Add column and row to the dataframe sub_subplots
  df_sub_plots_well <- dplyr::mutate(df_sub_plots_well,
    Row = as.numeric(match(
      toupper(substr( .data$Wells, 1, 1)),
      LETTERS
    )),
    Column = as.numeric(substr(.data$Wells, 2, 5))
  )
  return(df_sub_plots_well)
}
