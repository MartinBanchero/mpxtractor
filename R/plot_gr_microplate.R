#' Function to plot growth rates over microplate frame.
#'
#' This function takes a dataframe with the raw data and the information from the
#' layout file. Calculate growth rate and plot this growth rates over a microplate
#' frame.
#'
#' @param df_data Is a dataframe that combines data files with layout files
#' @param var_gr This the attribute to be used to calculate growth rates
#' @param exp_title optional, add the title.
#' @param ws is the windowsize in hours
#' @param cond_to_col The condition from the layout to color
#' @param plate_file plate file to be use to compute growth rates in case of multiple files.
#' @param output_filename The name of the output file followed by proper extension, ie. .png
#'
#' @return Returns the background plot which is the microplate frame and over this
#' the plot of growth rates in each well.
#' The background plot shows in the x-axis the number of columns and in the y-axis
#' the letter to identified the row.
#'
#'
#' @section Warning:
#' Note that the time should be a time series, if the time series is broken the
#' growth rates are not calculated and the process stop.
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Get the data file path
#' file_path_sp <- system.file(
#'  "extdata",
#'  "test_gr_spectramax.txt",
#'  package = "mpxtractor"
#' )
#'
#' # Extract the data stored in the files into a df using proper wrangling function
#' df_sp <- mpxtractor::read_spectramax_data(
#'  file = file_path_sp
#'  )
#'  # get the path to layout file
#' file_path_layout <- system.file(
#'  "extdata", "test_layout_file.csv",
#'  package = "mpxtractor"
#' )
#' # combine raw data with layout scheme
#' df_data_combine <- mpxtractor::combine_data_with_layout(
#'  df_data = df_sp,
#'  reader_type = "spectramax",
#'  layout_files = file_path_layout
#' )
#'
#' microplateplot <- mpxtractor::plot_gr_microplate(
#'  df_data = df_data_combine,
#'  var_gr = "Measurement",
#'  exp_title = "Spectramax experiment",
#'  ws = "2hs",
#'  cond_to_col = "condition",
#'  output_filename = "growth_rates_test.png")
#'
#' #Check vignette **plotting_functions()** for more information.
#'
# Main function
plot_gr_microplate <- function(df_data, var_gr, exp_title = NULL,
                               ws, cond_to_col, plate_file = NULL, output_filename) {
  # Check input
  if (!is.data.frame(df_data)) stop("df_data should be a dataframe")
  check_variables(df_data, var_gr, cond_to_col)
  check_is_multi_plate(df_data, plate_file)

  df_data_gr <- compute_growth_rates(df_data, var_gr, ws, plate_file)
  df_data_gr <- factor_to_color(df_data_gr, cond_to_col)

  df_sub_plots_well <- generate_subplots_by_well(df_data_gr, cond_to_col)
  df_sub_plots_well <- subplots_with_coordinates(df_sub_plots_well)
  df_sub_plots_well <- subplots_annotated(df_sub_plots_well, df_data_gr)
  all_wells_plot <- combine_subplots_backgr(df_sub_plots_well, exp_title, cond_to_col)
  save_plot(df_data, all_wells_plot, output_filename)

}


check_variables <- function(df_data, var_gr, cond_to_col) {
  if (!var_gr %in% colnames(df_data)) {
    stop("The variable (var_gr) to calculate growth rate is not present in the
         input data.")
  }
  if (!cond_to_col %in% colnames(df_data)) {
    stop("The variable (cond_to_col) assigned to color is not present in the
         input data.")
  }
}

check_is_multi_plate <- function(df_data, plate_file){
  if (is.null(plate_file) && !is.null(.data$plate_filename) &&
      length(unique(df_data$plate_filename)) > 1) {
    stop("Sorry, there is more than one plate present in the data. You have to
         specify which plate to use." )
  }
  df_data
}



# add column condition_fc with the different conditions as factors
factor_to_color <- function(sp_data_layout, cond_to_col) {
  sp_data_layout$condition_fc <- factor(sp_data_layout[[cond_to_col]],
    levels = unique(sp_data_layout[[cond_to_col]])
  )
  return(sp_data_layout)
}

save_plot <- function(df_data, all_wells_plot, output_filename ){
  if (length(unique(df_data$Wells)) == 96) {
    ggplot2::ggsave(
      filename = output_filename,
      plot = all_wells_plot,
      width = 15,
      height = 10,
      units = "cm")

  } else if (length(unique(df_data$Wells)) == 384) {
    ggplot2::ggsave(
      filename = output_filename,
      plot = all_wells_plot,
      width = 50,
      height = 30,
      units = "cm")
  }
}
