#' Transform the time from hh:mm:ss to hours or minutes
#'
#' Process column Time of df_data and transform the time in hh:mm:ss format to
#' hours or minutes.
#'
#' @param  df_data dataframe
#' @param time_format which is a character to indicate
#' whether the the format is in hours or in minutes.
#'
#' @return the input dataframe whit the column Time formatted.
#'
#'
#' @export
#'
#' @examples
#' #Load example dataframe
#' data(df_spectramax_outdata_1)
#'
#' # format time
#' df <- mpxtractor::format_time(df_spectramax_outdata_1)
#'
#' # Time transformed
#' df

format_time <- function(df_data, time_format = NULL) {
  if (!"Time" %in% colnames(df_data)) {
    df_data$Time <- df_data[["Reading"]]
    warning("There is no Time attribute in multiscanGO data, the same dataframe
            is return.")
    return(df_data)
  }
  if (is.null(time_format) || toupper(time_format) == toupper("hours")) {
    Time <- get_time_in_hh(df_data$Time)
    # Move column time to the front
    df_data$Time <- Time
    return(df_data)
  }
  # Transform time to minutes
  if (toupper(time_format) == toupper("minutes")) {
    Time <- get_time_in_mm(df_data$Time)
    # Replace column time with the transformed time.
    df_data$Time <- Time
    return(df_data)
    if (!is.null(time_format) && toupper(time_format) != toupper("minutes") &&
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
