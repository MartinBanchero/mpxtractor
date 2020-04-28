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

# Transform the time to hours or minutes
format_time <- function(df_data, time = NULL) {
  if (!"Time" %in% colnames(df_data)) {
    df_data$Time <- df_data[["Reading"]]
    return(df_data)
  }
  if (is.null(time) || toupper(time) == toupper("hours")) {
    Time <- get_time_in_hh(df_data$Time)
    # Move column time to the front
    df_data$Time <- Time
    return(df_data)
  }
  # Transform time to minutes
  if (toupper(time) == toupper("minutes")) {
    Time <- get_time_in_mm(df_data$Time)
    # Replace column time with the transformed time.
    df_data$Time <- Time
    return(df_data)
    if (!is.null(time) && toupper(time) != toupper("minutes") &&
        toupper(time) != toupper("hours")) {
      stop("The time unit is not recognized. The default unit is in hours, if you want
      the convertion in minutes you should give time = minutes.")
    }
  }
}
