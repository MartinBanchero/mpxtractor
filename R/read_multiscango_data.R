#' Function specific to read output .txt files from multiscanGO readers.
#'
#' \code{read_multiscango_data} returns the data in the .txt as tibble
#' data frame
#'
#' @export
#' @examples
#' file_path <- system.file("extdata", "multiscanGO_1streading.txt",
#' package = "mpxtractor")
#'
#' # Data is store as a tibble
#' data <- read_multiscanGO_data(
#'    file = file_path)
#'
#' # Now data is tidy
#' head(data)

# Main function
read_multiscango_data <- function(file){
  check_one_file_provided(file)
  check_file_path(file)
  check_that_file_is_non_empty(file)
  clean_file <- get_raw_file_clean_multiscango(file)
  df <- format_df(clean_file)
  df_tmp <- add_header(df)
  df_final_format <- final_format_df(df_tmp)
  df_result <- set_well_ids(df_final_format)
  df_result_tidy <- tidyr::as_tibble(df_result)
  df_result_tidy
}

# get the raw file
get_raw_file_clean_multiscango <- function(file){
  raw_file <- readLines(file, warn = FALSE, encoding = "latin1")
  raw_file_clean <- raw_file[which(raw_file != "")]  #Remove empty space
  clean_file <- raw_file_clean[-c(1, 2)]  #Remove firs two lines
}

# format_df
format_df <- function(clean_file){
  #Get indx of readings
  idx <- grepl("Reading", clean_file)
  #get all the lines whith numbers
  df <- read.table(text = clean_file[!idx])
  #Calculate the number of lines between readings
  wd <- diff(c(which(idx), length(idx) + 1)) - 1
  #Assign reading to corresponding values
  df <- cbind(Reading = rep(clean_file[idx], wd), df)
  df <- cbind(Well_Row = rep(LETTERS[1:wd[1]], length(wd)), df)

  # Leave only the number of Readings
  num_read <- gsub("\\tReading:", "\\1", df$Reading)
  # Add column reading with the number of reading as numeric
  df$Reading <- as.numeric(num_read)
  return(df)
}
# Header
add_header <- function(df)
{
  df2 <- df[c(-1,-2)]
  colnames(df2) <- 1:ncol(df2)
  df3 <- cbind(df[, c(1,2)], df2)
}

final_format_df <- function(df3)
{
  df_tmp <- df3 %>% tidyr::gather(Well_Col, Measurement, -c(Well_Row, Reading)) %>%
    dplyr::group_by(Well_Col, Reading) %>%
    dplyr::mutate(rowID = seq_along(Well_Col)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Wells = interaction(Well_Row, Well_Col, sep = ""), Well_Row = NULL, Well_Col = NULL)
}

set_well_ids <- function(df_tmp)
{
  wells <- df_tmp$Wells
  wellsname <- gsub("(^[A-Z])([0-9]$)", "\\10\\2", wells)
  df_tmp$Wells <- wellsname
  df_result <- df_tmp %>% dplyr::select(Wells, everything(), -rowID) %>%
                dplyr::arrange(Reading)
}
