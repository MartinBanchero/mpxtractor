#' Function specific to read output .txt files from fluorStar readers.
#'
#' \code{read_fluorstar_data} returns the data in the .txt as tibble
#' data frame
#'
#' @export
#' @examples
#' file_path <- system.file("extdata", "fluorstar_milkjuice_FL.txt",
#' package = "mpxtractor")
#'
#' # Data is store as a tibble
#' data <- read_fluorstar_data(
#'    file = file_path)
#'
#' # Now data is tidy
#' head(data)

# Main function
 read_fluorstar_data <- function(file){
  check_one_file_provided(file)
  check_file_path(file)
  check_that_file_is_non_empty(file)
  clean_file <- get_raw_file_clean_fluorstar(file)
  df_result <- generate_format_df_fluorstar(clean_file)
  df_result_tidy <- tidyr::as_tibble(df_result)
  df_result_tidy
}

# Clean the raw file
get_raw_file_clean_fluorstar <- function(file){
  raw_file <- readLines(file, encoding = 'latin1')
  clean_file <- grep( x = raw_file,
                      pattern = ".*\\t\t\t.*",
                      invert = TRUE,
                      value = TRUE,
                      useBytes = T)
}

# Generate df from fluorstar
generate_format_df_fluorstar <- function(clean_file) {
  samples <- grep(x = clean_file, pattern = '*Sample*', value = T, useBytes = T)
  names <-  grep(x = clean_file, pattern = '*Sample*', invert = T ,value = T,
                 useBytes = T)

  df_samples <- read.table(text = samples, sep = '\t')
  df_names <-  read.table(text = names, sep = '\t', stringsAsFactors = FALSE )

  col_names_df <- data.frame(lapply(df_names, function(x) {
    gsub("\\n", " ", x)
  }))

  colnames(df_samples) <- as.character(unlist(col_names_df[1,]))
  df_samples$`Well Col` <- sprintf("%02d", as.numeric(df_samples$`Well Col`))

  df_result <- df_samples %>%
                tidyr::unite(Wells, c(`Well Row`,`Well Col`), sep = '') %>%
                tidyr::gather(key = 'Time', value = 'Measurement', starts_with('Raw'))
}

