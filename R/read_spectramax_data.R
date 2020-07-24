#' Function specific to read output files (.txt) produced by spectraMax readers.
#'
#' This function receive one output file from spectraMax microplate reader and
#' generate a tibble dataframe.
#'
#' @param file The path to a proper .txt file formatted by the spectraMax machine.
#'
#' @return Returns a tibble data frame with four columns. The first column is
#' "Wells" this contained the names for each well (A01, A02..). The second column
#' represent "Time" at which the measurements were done, the format is in hh:mm:ss.
#' The third column is "Temperature", this is the temperature at which the
#' experiment was performed. The fourth column contained the measured values.
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
#' file_path <- system.file("extdata", "test_spectramax_data_1.txt",
#' package = "mpxtractor")
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
read_spectramax_data <- function(file) {
  check_one_file_provided(file)
  check_file_path(file)
  check_that_file_is_non_empty(file)
  input_file_is_spectramax(file)
  clean_file <- get_raw_file_clean_spectramax(file)
  df <- cleanfile_to_df(clean_file)
  result_df <- std_format_df(df)

  result_df
}


# Remove empty lines and lines with multiple tabs.
#
# Read the file line by line, remove empty lines and lines with multiple tabs.
# Remove the last tab o each line.
#
# Parameter is a raw_file A text vector with each element containing a tab-delimited
#
# Return a character vector.
get_raw_file_clean_spectramax <- function(file) {
  raw_file <- readLines(file, warn = FALSE, encoding = "latin1")
  raw_file_clean <- grep(raw_file[which(raw_file != "")],
    pattern = ".*\\t\t\t.*",
    invert = TRUE, value = TRUE, useBytes = T
  )

  cleanfile <- trimws(raw_file_clean, which = "right", whitespace = "[\t]")
}

input_file_is_spectramax <- function(file) {
  raw_file <- readLines(file, warn = FALSE, encoding = "latin1")
  raw_file <- strsplit(raw_file[1], split = "\t")
  if (!raw_file[[1]][1] == "Time(hh:mm:ss)" && !raw_file[[1]][2] == "Temperature(\u00B0\u0043)") {
    stop("The input file is not an spectramax file.")
  }
}

# Recive a character vector that is transformed to a data frame.
#
# The character vector is trasnformed to a data frame using read.table(). The
# header is false to avoid encoding problems, to avoid factors the stringsAsFactors
# is false.
#
# Parameters is the cleanfile object which is a character vector.
#
# The function return a dataframe.
cleanfile_to_df <- function(cleanfile) {
  df <- utils::read.table(textConnection(cleanfile),
    header = FALSE, sep = "\t",
    stringsAsFactors = FALSE,
    colClasses = "character", comment.char = ""
  )
  names(df) <- as.character(unlist(df[1, ]))

  return(df[-1, ])
}


# Transform the dataframe to a tidy data frame.
#
# First the measurements are transformed to numeric. Follow by gather the
# measurements and the well id. Then modify the well names adding zeros between
# the letter and the single number.This allow to sort the Well column. The last
# steps are, move the Well column to the first position and generate a tibble.
#
# Parameter is a dataframe object.
#
# This return a tidy tibble.
std_format_df <- function(df) {
  # Format the df
  well_ids <- 3:ncol(df)
  df[, well_ids] <- as.numeric(sub(",", ".", as.character(unlist(df[, well_ids]))))

  # Apply dplyr
  df <- tidyr::gather(df, key = "Wells", value = "Measurement", c(-1, -2))

  df$Wells <- gsub("(^[A-Z])([0-9]$)", "\\10\\2", df$Wells)

  df_measurements <- dplyr::select(df, .data$Wells, dplyr::everything())
  df_measurements <- format_time_spectra(df_measurements)

  df_result <- tidyr::as_tibble(df_measurements)
  colnames(df_result)[colnames(df_result) == "Temperature(\u00C2\u00B0\u0043)"] <- "Temperature"

  return(df_result)
}

# Transform the time format from spectramax to hh:mm:ss as character
#
# Using gsub to match each of the different time representation and transform to
# hh:mm:ss format
format_time_spectra <- function(df) {
  time <- df$`Time(hh:mm:ss)`
  time <- gsub("^(\\d{1}:\\d{2}$)", "\\00:0\\1", time)
  time <- gsub("^(\\d{2}:\\d{2}$)", "\\00:\\1", time)
  time <- gsub("^(\\d{1}:\\d{2}:\\d{2}$)", "\\0\\1", time)

  df$`Time(hh:mm:ss)` <- time
  colnames(df)[colnames(df) == "Time(hh:mm:ss)"] <- "Time"

  df
}
