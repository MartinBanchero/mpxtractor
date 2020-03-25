#' Function specific to read output files (.txt) produced by multisccanGo readers.
#'
#' This function recive one output file from multiscanGO microplate reader and
#' generate a tibble dataframe.
#'
#' @param file The path to a proper .txt file formatted by the multiscanGO machine.
#'
#' @return Returns a tibble data frame whith four columns. The first column is
#' "Wells" this containe the names for each well (A01, A02..). The second column
#' represent "Time" at wich the measurements were done, the format is in hh:mm:ss.
#' The third column is "Temperature", this is the temperature at which the
#' experiment was performed. The fourth column containe the measured values.
#'
#'
#' @section \code{file} format:
#' To simplify the number of tasks that the user have to apply before load the
#' data in R, \code{read_spectramax_data()} receive as a .txt the file that the
#' spectraMax reader return after the measurements. In this case the problem
#' related to different file formats, like .csv, .xlsx and versions is simplified
#' by using only .txt files.
#'
#'
#'
#'
#' @export
#' @examples
#' file_path <- system.file("extdata", "spectraMax_1stplate.txt",
#'                          package = "mpxtractor")
#'
#' # Data is store as a tibble
#' data <- read_spectramax_data(
#'   file = file_path
#' )
#'
#' # Now data is tidy
#' head(data)
#'
#' # Main function
read_multiscango_data <- function(file) {
  check_one_file_provided(file)
  check_file_path(file)
  check_that_file_is_non_empty(file)
  processed_file <- get_raw_file_clean_multiscango(file)
  df <- intermediate_df(processed_file)
  df_tmp <- add_col_names(df)
  final_df <- final_format_df(df_tmp)
  df_result <- set_well_ids(df_final_format)
  df_result_tidy <- tidyr::as_tibble(df_result)
  df_result_tidy
}

# Remove empty lines and two lines of the multiscanGO output file.
#
# Read the file line by line  removing empty lines and the first two lines of the
# file.
#
# Parameter is a raw_file A text vector with each element containing a tab-delimited
#
# Return a character vector in the object processed_file.
get_raw_file_clean_multiscango <- function(file) {
  raw_file <- readLines(file, warn = FALSE, encoding = "latin1")
  raw_file_clean <- raw_file[which(raw_file != "")] # Remove empty space
  processed_file <- raw_file_clean[-c(1, 2)] # Remove firs two lines
}

# Generate a dataframe from the processed file.
#
# This function first get the indexes for each line containing the string
# readings, then using read.table() get all the measurements for each reading.
# Calculate the lines between readings. Add the readings as column and the Well
# Row indicated by one letter.
# At the end transform the measurements to numeric.
#
intermediate_df <- function(processed_file) {
  idx <- grepl("Reading", processed_file)
  df <- read.table(text = processed_file[!idx])
  wd <- diff(c(which(idx), length(idx) + 1)) - 1
  # Assign reading to corresponding values
  df <- cbind(Reading = rep(processed_file[idx], wd), df)
  df <- cbind(Well_Row = rep(LETTERS[1:wd[1]], length(wd)), df)

  # Leave only the number of Readings from the string
  num_read <- gsub("\\tReading:", "\\1", df$Reading)

    df$Reading <- as.numeric(num_read)
  return(df_intermediate)
}

# Add numbers as colname to the columns with measurements.
#
# The argument is a dataframe.
# Return dataframe with the name of columns indicating the number of well col.
add_col_names <- function(df_intermediate) {
  df2 <- df_intermediate[c(-1, -2)]
  colnames(df2) <- 1:ncol(df2)
  df_intermediate <- cbind(df_intermediate[, c(1, 2)], df2)
}

# Transform the dataframe to a more tidy dataframe.
#
# The argument is
# return a tidy dataframe

final_df <- function(df3) {
  df_tmp <- df3 %>%
    tidyr::gather(Well_Col, Measurement, -c(Well_Row, Reading)) %>%
    dplyr::group_by(Well_Col, Reading) %>%
    dplyr::mutate(rowID = seq_along(Well_Col)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Wells = interaction(Well_Row, Well_Col, sep = ""),
                  Well_Row = NULL,
                  Well_Col = NULL)
}


set_well_ids <- function(df_tmp) {
  wells <- df_tmp$Wells
  wellsname <- gsub("(^[A-Z])([0-9]$)", "\\10\\2", wells)
  df_tmp$Wells <- wellsname
  df_result <- df_tmp %>%
    dplyr::select(Wells, everything(), -rowID) %>%
    dplyr::arrange(Reading)
}
