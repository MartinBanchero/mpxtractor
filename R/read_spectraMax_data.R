#' Function specific to read output files from spectraMax readers.
#'
#' @export

# Main function
read_spectraMax_data <- function(file)
{
  check_that_only_one_file_is_provided(file)
  check_file_path(file)
  check_that_file_is_non_empty(file)
  clean_file <- get_raw_file_clean_spectraMax(file)
  df <- cleanfile_to_df(clean_file)
  result_df <- std_format_df(df)
  result_df
}

get_raw_file_clean_spectraMax <- function(file)
{
  raw_file <- readLines(file, warn = FALSE, encoding = "latin1")
  raw_file_clean <- grep(raw_file[which(raw_file != "")], pattern = ".*\\t\t\t.*",
                         invert = TRUE, value = TRUE, useBytes = T)

  cleanfile <- trimws(raw_file_clean, which = "right", whitespace = "[\t]")
}

