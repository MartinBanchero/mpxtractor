#' Function to plot layout dataframes
#'
#' This function recive dataframe similar to the output from read_layout_files()
#' and generate a plot that represents the designed layout of the microplate.
#'
#' @param platemap_df dataframe structure similar to read_layout_files() output
#' @param var_shape Assign shape to represent one variable
#' @param var_colour Assign colour to other variable
#' @param add_conc This argument is optional, TRUE to add concentration.
#' @param plate_title This argument is optional, add the title.
#'
#' @return Returns a plot that represents the microplate with the given layout.
#' The plot shows in the x-axis the number of columns and in the y-axis the
#' letter to identified the row.
#'
#'
#'
#' @importFrom rlang .data
#'
#' @export
#' @examples
#' file_path <- system.file("extdata", "test_spectraMax_layout_1.csv",
#'   package = "mpxtractor"
#' )
#'
#' # Data is store as a tibble
#' plot_plate <- plot_layout_file(
#'   file = file_path, var_shape = "basic", var_colour = "condition",
#'   plate_title = "My experiment"
#' )
#'
#' # Show the plot
#' plot_plate
#'
# Main function
plot_layout_dataframe <- function(platemap_df, var_shape, var_colour, add_conc=NULL, plate_title = NULL) {
  platemap_df <- dplyr::mutate(platemap_df,
    Row = as.numeric(match(
      toupper(substr(.data$Wells, 1, 1)),
      LETTERS
    )),
    Column = as.numeric(substr(.data$Wells, 2, 5))
  )

  NUMBER_FACTORS <- 6 # number of shapes available to use
  if (length(unique(platemap_df[[var_shape]])) > NUMBER_FACTORS) {
    stop("You must use attributes for var_shape with less than 7 different factors.")
  }

  title <- check_title(plate_title)
  plate <- generate_platemap(platemap_df, var_shape, var_colour)

  if (!is.null(add_conc) && add_conc) {
    check_col_concentration(platemap_df)
    concentration <- add_concentration(platemap_df)
    plate_with_conc <- plate + concentration + title
    return(plate_with_conc)
  }
  plate + title

}
