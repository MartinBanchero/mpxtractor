#' Function specific to read output files (.txt) produced by multisccanGo readers.
#'
#' This function recive one output file from multiscanGO microplate reader and
#' generate a tibble dataframe.
#'
#' @param file The path to a proper .txt file formatted by the multiscanGO machine.
#' @param time_point the time interval at which the measurements are done in min.
#'
#' @return Returns a tibble data frame whith four columns. The first column is
#' "Wells" this contain the names for each well (A01, A02..). The second column
#' represent "Reading" at which the measurements were done, The third column is
#' "Measurement", this is the value measured for each "Reading".
#'
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
#'   file = file_path, time_point = "2.5 min"
#' )
#'
#' # Now data is tidy
#' head(data)
#'
#' # Main function
read_multiscango_data <- function(file, time_point) {
  check_one_file_provided(file)
  check_file_path(file)
  check_that_file_is_non_empty(file)
  check_time_point(time_point)
  input_file_is_multiscango(file)
  processed_file <- get_raw_file_clean_multiscango(file)
  df_tmp <- generate_df(processed_file)
  df_intermediate <- add_col_names(df_tmp)
  df_final_tmp <- generate_df_result(df_intermediate)
  df_result <- set_well_ids(df_final_tmp)
  df_result <- impute_rows_with_NA(df_result)
  df_result <- add_column_time(df_result, time_point)
  df_result_tidy <- tidyr::as_tibble(df_result)
  df_result_tidy
}


input_file_is_multiscango <- function(file) {
  raw_file <- readLines(file, warn = FALSE, encoding = "latin1")
  raw_file <- strsplit(raw_file[1], split = " ")
  if (!raw_file[[1]][1] == "Results") {
    stop("The input file is not a multiscango file.")
  }
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
generate_df <- function(processed_file) {
  idx <- grepl("Reading", processed_file)
  df_tmp <- utils::read.table(text = processed_file[!idx], fill = TRUE)
  wd <- diff(c(which(idx), length(idx) + 1)) - 1
  # Assign reading to corresponding values
  df_tmp <- cbind(Reading = rep(processed_file[idx], wd), df_tmp)
  df_tmp <- cbind(Well_Row = rep(LETTERS[1:wd[1]], length(wd)), df_tmp)

  # Leave only the number of Readings from the string
  num_read <- gsub("\\tReading:", "\\1", df_tmp[["Reading"]])

  df_tmp$Reading <- as.numeric(num_read)
  return(df_tmp)
}

# Add numbers as colname to the columns with measurements.
#
# The argument is a dataframe.
# Return dataframe with the name of columns indicating the number of well col.
add_col_names <- function(df_tmp) {
  df2 <- df_tmp[c(-1, -2)]
  colnames(df2) <- 1:ncol(df2)
  df_intermediate <- cbind(df_tmp[, c(1, 2)], df2)
  return(df_intermediate)
}

# Transform the dataframe to a more tidy dataframe.
#
# The argument is the dataframe output of the function add_col_names.
# return a tidy dataframe

generate_df_result <- function(df_intermediate) {
    Well_Col <- Measurement <- Well_Row <- Reading <- NULL
    df_final_tmp <- tidyr::gather(
    df_intermediate,
    Well_Col,
    Measurement,
    -c(Well_Row, Reading)
  )
  df_final_tmp <- dplyr::group_by(
    df_final_tmp,
    .data$Well_Col,
    .data$Reading
  )
  df_final_tmp <- dplyr::mutate(df_final_tmp,
    rowID = seq_along(.data$Well_Col)
  )
  df_final_tmp <- dplyr::ungroup(df_final_tmp)
  df_final_tmp <- dplyr::mutate(df_final_tmp,
    Wells = interaction(.data$Well_Row, .data$Well_Col, sep = ""),
    Well_Row = NULL,
    Well_Col = NULL
  )
  df_final_tmp
}

# Reformat the output of the function final_df()
#
#  This function reformat the wells id, put the column Wells in the first column
# amd arrange by the column Reading.
#
# The argument is the dataframe output of the function add_col_names.
# return a tidy dataframe

set_well_ids <- function(df_final_tmp) {
  wells <- df_final_tmp$Wells
  wellsname <- gsub("(^[A-Z])([0-9]$)", "\\10\\2", wells)
  df_final_tmp$Wells <- wellsname
  df_result <- dplyr::select(
    df_final_tmp,
    .data$Wells,
    dplyr::everything(),
    -.data$rowID
  )
  df_result <- dplyr::arrange(df_result, .data$Wells)
}

impute_rows_with_NA <- function(df_result) {
  if (any(is.na(df_result))) {
    NA_data <- dplyr::filter_all(df_result, dplyr::any_vars(is.na(.)))
    warning(paste(
      "Warning 1\n Well:", NA_data$Wells, "and Reading:", NA_data$Reading,
      "contain missing values.\n"
    ))
    df_result[["Measurement"]] <- imputeTS::na_ma(
      df_result[["Measurement"]],
      k = 1
    )
    warning("Warning 2\n The missing values are imputed by taking the mean
between the two elements surrounding the center.")
    return(df_result)
  }
  return(df_result)
}

check_time_point <- function(tp) {
  if (!any(grepl("min", tp))) {
    stop("time point should be in minutes")
  }
}

add_column_time <- function(df, time_point) {
  tp <- as.numeric(sub("\\min.*", "", time_point))
  time_point <- tp / 60 # to hours
  max_reading <- max(df$Reading)
  total_time <- (time_point * (max_reading - 1))

  df_result <- dplyr::group_by(df, .data$Wells)
  df_result <- dplyr::mutate(
    df_result,
    Time = seq(0, total_time, by = time_point)
  )
  df_result <- get_time_hhmmss(df_result)
  df_result[,c("Wells", "Time", "Reading", "Measurement")]

}
