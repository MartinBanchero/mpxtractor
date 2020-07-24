#' Function specific to read output files (.txt) from fluorStar readers machines.
#'
#' This function receive one output file from fluorStar microplate reader and
#' generate a tibble dataframe.
#'
#' @param file The path to a proper .txt file formatted by the fluorStar machine.
#'
#' @return Returns a tibble data frame with four columns. The first column is
#' "Wells" this containe the names for each well (A01, A02..). The second column
#' represent "Sample" which identified the wells, this part of the standard output
#' of fluorStar machines.
#' The third column is "Time", that represents the timestep at which the machine
#' measures. The fourth column contained the measured values. Depending on the experiment,
#' this can be fluorescence, absorbance between others.
#'
#' @importFrom rlang .data
#'
#' @export
#' @examples
#' file_path <- system.file("extdata", "test_fluorstar_fluorescence_data.txt",
#'   package = "mpxtractor"
#' )
#'
#' # Data is store as a tibble
#' data <- read_fluorstar_data(
#'   file = file_path
#' )
#'
#' # Now data is tidy
#' head(data)
#'
# Main function
read_fluorstar_data <- function(file) {
  check_one_file_provided(file)
  check_file_path(file)
  check_that_file_is_non_empty(file)
  input_file_is_fluorstar(file)
  clean_file <- get_raw_file_clean_fluorstar(file)
  df_result <- generate_format_df_fluorstar(clean_file)
  df_result <- add_wave_lenght_col(df_result)
  df_result <- format_time_fluorstar(df_result)
  df_result_tidy <- tidyr::as_tibble(df_result)
  df_result_tidy
}

# Check weather a file is fluorstar output file
#
# Read line by line and check if the first line containe the word user, this is
# used as a tag to identify the fluorstar file.
#
# the argument is the fluorstar raw file
# return the stop statment if is not a correct file, otherwise does not reurn anything
#
input_file_is_fluorstar <- function(file) {
  raw_file <- readLines(file, warn = FALSE, encoding = "latin1")
  raw_file <- strsplit(raw_file[1], split = ":")
  if (!raw_file[[1]][1] == "User") {
    stop("The input file is not a fluorstar file.")
  }
}



# Clean the raw file
#
# This function read the file line by line and return all the lines that NOT contain
# more than three tabs. This using grep() with invert = TRUE
#
# The argument is the raw file from fluorstar
#
# return clean file
get_raw_file_clean_fluorstar <- function(file) {
  raw_file <- readLines(file, encoding = "latin1")
  clean_file <- grep(
    x = raw_file,
    pattern = ".*\\t\t\t.*",
    invert = TRUE,
    value = TRUE,
    useBytes = T
  )
}

# Generate data frame from fluorstar data.
#
# The function extract samples and names into character vector. Reads the lines
# samples and names into a dataframe. Add names to samples. Gather time and
# measurement. Remove the word "Sample" from the column Sample.
#
# Argument is a character vector clean_file
#
# returns a data frame with Wells, Sample, Time and Measurement as columns

generate_format_df_fluorstar <- function(clean_file) {
  #define the global variables
  Well <- Col <- Well <- Row <- Wells <- NULL

  # Extract samples and names into a character vector.
  samples <- grep(x = clean_file, pattern = "*Sample*", value = T, useBytes = T)
  names <- grep(
    x = clean_file, pattern = "*Sample*", invert = T, value = T,
    useBytes = T
  )
  # Read the lines in samples and names into data frame
  df_samples <- utils::read.table(text = samples, sep = "\t")
  df_names <- utils::read.table(text = names, sep = "\t", stringsAsFactors = FALSE)
  # Remove \n
  col_names_df <- data.frame(lapply(df_names, function(x) {
    gsub("\\n", " ", x)
  }))
  # Add names to samples
  colnames(df_samples) <- as.character(unlist(col_names_df[1, ]))
  df_samples$`Well Col` <- sprintf("%02d", as.numeric(df_samples$`Well Col`))
  # Generate well name
  df_tmp <- tidyr::unite(df_samples,
    Wells,
    c(.data$`Well Row`, .data$`Well Col`),
    sep = ""
  )
  # Gather time and measurements
  df_result <- tidyr::gather(df_tmp,
    key = "Time",
    value = "Measurement",
    dplyr::starts_with("Raw")
  )
  # Remove the word Sample from the column.
  colnames(df_result)[2] <- "Sample"
  df_result$Sample <- gsub(".*e", "", df_result[["Sample"]])
  df_result
}

add_wave_lenght_col <- function(df_result){
  matches <- regmatches(
    df_result[["Time"]],
    gregexpr(
      "(?<=\\().+?(?=\\))",
      df_result[["Time"]],
      perl = TRUE
    )
  )
  df_result <- dplyr::mutate(df_result, wavelength = unlist(matches))
  df_result
}






# Transform the time column into time format hh:mm:ss as character.
#
# First remove letters, generate a dataframe with hour and minutes. Add 0 to complete
# the format. Set the time format and arrange the data frame by wells and time
#
# Argument is a dataframe object.
#
# This return a tidy tibble with proper time format.
format_time_fluorstar <- function(raw_fls_data) {
  raw_fls_data$Time <- sub(".*-", "", raw_fls_data[["Time"]])
  # Remove letters
  matches <- regmatches(
    raw_fls_data[["Time"]],
    gregexpr(
      "[[:digit:]]+",
      raw_fls_data[["Time"]]
    )
  )
  # create a data frame with hour and minute.
  time_df <- as.data.frame(do.call(rbind, matches))
  colnames(time_df) <- c("hour", "minute")

  time_df$hour <- stringr::str_pad(time_df[["hour"]], 2, pad = "0")
  time_df$minute <- stringr::str_pad(time_df[["minute"]], 2, pad = "0")

  # Set the time format and arrange the data frame by wells and time
  time_df <- dplyr::mutate(time_df,
    Time = paste(.data$hour, ":",
      .data$minute, ":00",
      sep = ""
    )
  )
  raw_fls_data$Time <- time_df[["Time"]]
  raw_fls_data <- dplyr::group_by(raw_fls_data, .data$Sample)
  raw_fls_data <- dplyr::arrange(
    raw_fls_data,
    .data$Wells,
    .data$Time
  )
}
