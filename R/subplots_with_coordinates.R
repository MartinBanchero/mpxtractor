
cut_into_coordinates <- function(variable, ngroups) {
  seq_all <- seq(min(variable) - 0.5, max(variable) + 0.5, by = 1)
  cut(variable,
    breaks = seq_all,
    labels = paste(seq_all[-(ngroups + 1)], seq_all[-1], sep = ","),
    include.lowest = TRUE
  )
}

add_rows_and_columns <- function(df_sub_plots_well) {
  # Add column and row to the table sub_subplots
  df_sub_plots_well <- dplyr::mutate(df_sub_plots_well,
    Row = as.numeric(match(
      toupper(substr(Wells, 1, 1)),
      LETTERS
    )),
    Column = as.numeric(substr(Wells, 2, 5))
  )
  return(df_sub_plots_well)
}


subplots_with_coordinates <- function(df_sub_plots_well) {
  df_sub_plots_well <- add_rows_and_columns(df_sub_plots_well)
  df_sub_plots_well$Row <- rev(df_sub_plots_well[["Row"]])

  # Function cut_into_coordinates() to generate the coordinates x-y for each subplot.
  df_sub_plots_well$group_x <- cut_into_coordinates(
    df_sub_plots_well[["Column"]],
    length(unique(df_sub_plots_well[["Column"]]))
  )
  df_sub_plots_well$group_y <- cut_into_coordinates(
    df_sub_plots_well[["Row"]],
    length(unique(df_sub_plots_well[["Row"]]))
  )

  df_sub_plots_well <- tidyr::separate(df_sub_plots_well, group_x,
    into = c("min_x", "max_x"),
    sep = ",", convert = TRUE
  )

  df_sub_plots_well <- tidyr::separate(df_sub_plots_well, group_y,
    into = c("min_y", "max_y"),
    sep = ",", convert = TRUE
  )
}
