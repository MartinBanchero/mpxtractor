#' Function specific to read output .txt files from multiscanGO readers.
#'
#' \code{read_multiscanGO_data} returns the data in the .txt as tibble data frame
#'
#' @export
#' @examples
#' file_path <- system.file("extdata", "multiscanGO_1streading.txt", package = "mpxtractor")
#'
#' # Data is store as a tibble
#' data <- read_multiscanGO_data(
#'    file = file_path)
#'
#' # Now data is tidy
#' head(data)

# Main function
read_multiscanGO_data <- function(file)
{
  check_that_only_one_file_is_provided(file)
  check_file_path(file)
  check_that_file_is_non_empty(file)
  clean_file <- get_raw_file_clean_multiscanGO(file)
  df <- format_df(clean_file)
  df_tmp <- add_header(df)
  df_final_format <- final_format_df(df_tmp)
  df_result <- set_well_ids(df_final_format)
  df_result_tidy <- tidyr::as_tibble(df_result)
  df_result_tidy
}

# get the raw file
get_raw_file_clean_multiscanGO <- function(file)
{
  raw_file <- readLines(file, warn = FALSE, encoding = "latin1")
  raw_file_clean <- raw_file[which(raw_file != "")]  #Remove empty space
  clean_file <- raw_file_clean[-c(1, 2)]  #Remove firs to lines( I have to ask if we need this info)
}

# format_df
format_df <- function(clean_file)
{
  idx <- grepl("Reading", clean_file)  #Get indx of readings
  df <- read.table(text = clean_file[!idx])  #get all the lines whith numbers

  wd <- diff(c(which(idx), length(idx) + 1)) - 1  #Calculate the number of lines between readings

  df <- cbind(Reading = rep(clean_file[idx], wd), df)  #Assign reading to corresponding values
  num_read <- gsub("\\tReading:", "\\1", df$Reading)  # Leave only the number of Readings
  df$Reading <- as.numeric(num_read)  # Add column reading with the number of reading as numeric
  return(df)
}
# Header
add_header <- function(df)
{
  df2 <- df[-1]
  colnames(df2) <- LETTERS[1:ncol(df2)]
  df3 <- cbind(Reading = df[, 1], df2)
}

final_format_df <- function(df3)
{
  df_tmp <- df3 %>% tidyr::gather(Col, Measurement, -Reading) %>%
    dplyr::group_by(Reading, Col) %>%
    dplyr::mutate(rowID = seq_along(Col)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(name = interaction(Col, rowID, sep = ""), Col = NULL, rowID = NULL) %>%
    tidyr::spread(name, Measurement)
  df_tmp2 <- tidyr::gather(df_tmp, key = "Wells", value = "Measurement", -Reading)
}

set_well_ids <- function(df_tmp2)
{
  wells <- df_tmp2$Wells
  wellsname <- gsub("(^[A-Z])([0-9]$)", "\\10\\2", wells)
  df_tmp2$Wells <- wellsname
  df_result <- dplyr::select(df_tmp2, Wells, everything())  #Swap the well column to the first column
}
