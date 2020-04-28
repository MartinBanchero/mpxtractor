#' Function to plot growth rates from spectramax data.
#'
#' This function recive two files which are the layout file .csv and the spectramax
#' data file .txt.
#' plot that represents the growth rates into the microplate frame.
#'
#' @param file The path to a proper .csv layout file.
#' @param var_shape Assign shape to represent one variable
#' @param var_colour Assign colour to other variable
#' @param name_plate_layout This argument is optional, add the title.
#'
#' @return Returns a plot that represents the microplate with the given layout.
#' The plot shows in the x-axis the number of columns and in the y-axis the
#' letter to identified the row.
#'
#'
#' @section \code{file} format:
#' Note that the .csv layout file needs to be formatted in the correct way. See
#' examples below.
#'
#'
#' @export
#' @examples
#' file_path <- system.file("extdata", "spectraMax_layout_plate1.csv",
#'   package = "mpxtractor"
#' )
#'
#' # Data is store as a tibble
#' plot_plate <- plot_layout_file(
#'   file = file_path, var_shape = "basic", var_colour = "condition",
#'   name_plate_layout = "My experiment"
#' )
#'
#' # Show the plot
#' plot_plate
#'
#'
#' # Main function
plot_gr_microplate <- function(df_data, var_gr,
                               time = NULL,
                               exp_title = NULL,
                               windowsize,
                               var_to_col) {
  # Check input
  if (!is.data.frame(df_data)) stop("df_data should be a dataframe")
  check_variables(df_data, var_gr, var_to_col)
  df_data <- check_Na_Inf_in_var_gr(df_data, var_gr)
  df_data <- format_time(df_data, time)
  df_data <- check_time_series(df_data)
  df_data <- calculate_growth_rates(df_data, var_gr, windowsize)

  df_data_ <- factor_to_color(df_data, var_to_col)

  df_sub_plots_well <- generate_subplots_by_well(df_data_, var_to_col)
  df_sub_plots_well <- subplots_with_coordinates(df_sub_plots_well)
  df_sub_plots_well <- subplots_annotated(df_sub_plots_well, df_data_)
  all_wells_plot <- plot_gr_in_plate(df_sub_plots_well, exp_title, var_to_col)
  all_wells_plot
}

check_variables <- function(df_data, var_gr, var_to_col) {
  if (!var_gr %in% colnames(df_data)) {
    stop("The variable (var_gr) to calculate growth rate is not present in the input data.")
  }
  if (!var_to_col %in% colnames(df_data)) {
    stop("The variable (var_to_col) assigned to color is not present in the input data.")
  }
}


check_Na_Inf_in_var_gr <- function(df_data, var_gr) {
  if (any(is.na(df_data[[var_gr]]))) {
    df_data[[var_gr]] <- imputeTS::na_ma(df_data[[var_gr]], k = 1)
    warning("The NAs present in `var_gr` are imputed by taking the mean between
    the two elements surrounding the center.\n")

    return(df_data)
  }
  if (any(is.infinite(df_data[[var_gr]]))) {
    is.na(df_data[[var_gr]]) <- sapply(df_data[[var_gr]], is.infinite)
    df_data[[var_gr]] <- imputeTS::na_ma(df_data[[var_gr]], k = 1)
    warning("The -Inf/Inf present in var_gr are imputed by the mean between the
            two elements surrounding the center.\n")

    return(df_data)
  }
  else {
     return(df_data)
  }
}

# Function check sequence time for time series
check_time_series <- function(df_data) {
  df_tmp <- dplyr::group_by(df_data, Wells)
  df_tmp1 <- dplyr::mutate(df_tmp, Diff = round(Time - dplyr::lag(Time), digits = 3))
  df_tmp1[is.na(df_tmp1)] <- 0
  if (length(unique(df_tmp1[["Diff"]])) > 2) {
    stop("The sequence of time is not equally spaced, thus is not a time series")
  }
  df_tmp1
  #df_data <- dplyr::select(df_data, -Diff)
}


factor_to_color <- function(sp_data_layout, var_to_col) {
  sp_data_layout$condition_fc <- factor(sp_data_layout[[var_to_col]],
    levels = unique(sp_data_layout[[var_to_col]])
  )
  return(sp_data_layout)
}
