read_spectraMax_file <- function(file) {
  check_that_only_one_file_is_provided(file)
  check_file_path(file)
  check_that_file_is_non_empty(file)
  clean_file <- get_raw_file_clean(file)
  df <- cleanfile_to_df(clean_file)
  result_df <- std_format_df(df)
  return(result_df)
}