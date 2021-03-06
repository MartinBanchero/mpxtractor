#' Transform the time from hh:mm:ss to hours or minutes
#'
#' Process column Time of df_data and transform the time in hh:mm:ss format to
#' hours or minutes.
#'
#' @param  df_data dataframe
#' @param time_format character value "hours" or "minutes" to indicate
#' whether the the format is in hours or in minutes.
#'
#' @return the input dataframe whit the column Time formatted.
#'
#'
#' @export
#'
#' @examples
#' #Load example dataframe
#' data_file <- system.file("extdata",
#' "test_spectramax_data_1.txt",
#' package = "mpxtractor")
#' df_spectramax_outdata_1 <- mpxtractor::read_spectramax_data(data_file)
#'
#' # format time
#' df <- mpxtractor::format_time(df_spectramax_outdata_1)
#'
#' # Time transformed
#' df

format_time <- function(df_data, time_format = NULL) {
  if (!is.null(time_format) && toupper(time_format) == toupper("hours")) {
    Time <- get_time_in_hh(df_data$Time)
    # Move column time to the front
    df_data$Time <- Time
    return(df_data)
  }
  # Transform time to minutes
  if (!is.null(time_format) && toupper(time_format) == toupper("minutes")) {
    Time <- get_time_in_mm(df_data$Time)
    # Replace column time with the transformed time.
    df_data$Time <- Time
    return(df_data)
    if (!is.null(time_format) && toupper(time_format) != toupper("minutes") ||
        toupper(time_format) != toupper("hours")) {
      stop("The time unit is not recognized. The default unit is in hours, if you want
      the convertion in minutes you should give time = minutes.")
    }
  }
}


# Time in hs
get_time_in_hh <- function(time.col) {
  sapply(strsplit(time.col, ":"), function(x) {
    x <- as.numeric(x)
    x[1] + x[2] / 60 + x[3] / 3600
  })
}

# Time in minutes
get_time_in_mm <- function(time.col) {
  sapply(strsplit(time.col, ":"), function(x) {
    x <- as.numeric(x)
    x[1] * 60 + x[2] + x[3] / 60
  })
}



get_time_hhmmss <- function(df) {
  h <- df$Time %/% 1
  m <- ((df$Time %% 1) * 60) %/% 1
  s <- round((((df$Time %% 1) * 60) %% 1) * 60, 0)

  time_first_format <- paste(h, m, s, sep = ":")

  time_hh <- gsub("^(\\d{1}:\\d{1,}:\\d{1,}$)", "\\0\\1", time_first_format)
  time_mm <- gsub("^(\\d{2}:)(\\d{1}:)", "\\10\\2", time_hh)
  time_hhmmss <- gsub("^(\\d{2}:\\d{2}:)(\\d{1})$", "\\10\\2", time_mm)
  df$Time <- time_hhmmss
  df
}
