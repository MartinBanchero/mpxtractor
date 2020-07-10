#' Function specific to read output files (.txt) produced by multisccanGo readers.
#'
#' This function receive one output file from multiscanGO microplate reader and
#' generate a tibble dataframe.
#'
#' @param file The path to a proper .txt file formatted by the multiscanGO machine.
#' @param time_interval the time interval between measurements. This parameter is
#' needed only for files formatted by the multiscango machine output option table.
#' @param input_type specified if the input file of multiscan is in table or list format.
#'
#' @return Returns a tibble data frame for both multiscan formats, table or list.
#'  In the case of table format, returns four columns. The first column is
#' "Wells" this contain the names for each well (A01, A02..). The second column
#' represent "Reading" at which the measurements were done, The third column is
#' "Measurement", this is the value measured for each "Reading".
#' In the case of list format returns the same columns plus the column time in
#' hh:mm:ss, column name Abs is change to Measurement, and `Meas.time[s]` to
#' Meas.time.sec.
#'
#' @section \code{file} format:
#' To simplify the number of tasks that the user have to apply before load the
#' data in R, \code{read_multiscango_data()} receive as a .txt the file that the
#' multiscanGO reader return after the measurements. In this case the problem
#' related to different file formats, like .csv, .xlsx and versions is simplified
#' by using only .txt files.
#'
#' @importFrom rlang .data
#'
#'
#' @export
#' @examples
#' file_path <- system.file("extdata", "test_multiscango_data_1.txt",
#'   package = "mpxtractor"
#' )
#'
#' # Data is store as a tibble
#' data <- read_multiscango_data(
#'   file = file_path, time_interval = "2 min", input_type = "table"
#' )
#'
#' # Now data is tidy
#' head(data)
#'
#' # Main function
read_multiscango_data <- function(file, time_interval = NULL, input_type ) {
  check_one_file_provided(file)
  check_file_path(file)
  check_that_file_is_non_empty(file)
  check_read_multiscan_arguments(time_interval, input_type)

  if (!is.null(time_interval) && toupper(input_type) == toupper("table")) {
    df_result_tidy <- read_multiscango_table_output(file, time_interval)
    return(df_result_tidy)
    }
  else if (is.null(time_interval) && toupper(input_type) == toupper("list")) {
    df_result_tidy <- read_multiscan_list_output(file)
    return(df_result_tidy)
  }
}


check_read_multiscan_arguments <- function(time_interval, input_type) {
  if (is.null(time_interval) && toupper(input_type) != toupper("list")) {
stop("unrecognized list input_type.
Also, to read multiscan table formats, time_interval is needed")
  }
  if (!is.null(time_interval) && toupper(input_type) != toupper("table")) {
    stop("unrecognized table input_type")
  }
}
