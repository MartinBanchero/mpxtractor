#' Function to compute growth rates
#'
#' This function takes a dataframe with the raw data and the information from the
#' layout file and calculate growth rate (mu) using Savitzky and Golay filter(savgol).
#' Here the first derivative is given in the parameter m. And plynomial degree p = 1
#'
#' @param df_data Is a dataframe that combines data files with layout files
#' @param var_gr This the attribute to be used to calculate growth rates
#' @param ws is the windowsize in hours
#' @param ts is the time step in minutes
#'
#'
#' @return Returns the input dataframe with the column conaining the growth rates.
#'
#'
#' @section Warning:
#' Note that the time should be a time series, if the time series is broken the
#' growth rates are not calculated and the process stops.
#'
#'
#' @export
#'
#' @examples
#' file_path <- system.file("extdata", "test_gr_spectramax_data.txt",
#'   package = "mpxtractor"
#' )
#'
#' # Data is store as a tibble
#' df_data <- read_spectramax_data(file = file_path)
#' df_gr_data <- mpxtractor::compute_growth_rates(
#'   df_data = df_data,
#'   var_gr = "Measurement",
#'   ws = "2hs", ts = "2min"
#' )
#' # Main function
compute_growth_rates <- function(df_data, var_gr, ws, ts) {
  df_data <- check_Na_Inf_in_var_gr(df_data, var_gr)
  df_data <- format_time(df_data)
  df_data <- check_time_series(df_data)
  df_data_gr <- get_growth_rates(df_data, var_gr, ws, ts)
  return(df_data_gr)
}

# calculate growth rates using Savitzky and Golay filter
#
# This function first check the arguments and set the parameters to use in sgolayfilt()
# and generate a dataframe similar to the input but with the column growth_rates.
#
# parameters df_data that contains the raw data, var_gr is the attribute to use to apply
# the savgol filter, ws the windowsize in minutes and the ts timestep in minutes.
#
# return a dataframe with the raw data and one more column with growth_rates.
#
get_growth_rates <- function(df_data, var_gr, ws, ts) {
  check_ws_ts(ws, ts)
  ws <- as.numeric(sub("\\hs.*", "", ws))
  ts <- as.numeric(sub("\\min.*", "", ts))
  df_data <- dplyr::mutate(df_data, Diff_time = Time[2] - Time[1])
  timestep <- ts / 60 # hs
  windowlength <- (ws / df_data$Diff_time[2])
  windowlength <- ifelse(windowlength %% 2 == 0, windowlength + 1, windowlength)

  # Calculate growth rate (mu) using Savitzky and Golay filter(savgol).
  df_data <- dplyr::group_by(df_data, Wells)
  growth_rate <- dplyr::group_map(df_data, ~ signal::sgolayfilt(.x[[var_gr]],
    p = 1,
    n = windowlength,
    m = 1,
    ts = timestep
  ))

  df_data$growth_rate <- as.vector(unlist(growth_rate))
  df_data <- dplyr::select(df_data, -c(Diff, Diff_time))
  return(df_data)
}

check_ws_ts <- function(ws, ts) {
  if (!any(grepl("hs", ws))) {
    stop("windowsize(ws) should be in hours")
  }
  if (!any(grepl("min", ts))) {
    stop("timestep(ts) should be in minutes")
  }
}


check_Na_Inf_in_var_gr <- function(df_data, var_gr) {
  if (any(is.na(df_data[[var_gr]]))) {
    df_data[[var_gr]] <- imputeTS::na_ma(df_data[[var_gr]], k = 1)
    warning("The NAs present in `var_gr` are imputed by taking the mean between
    the two elements surrounding the center.\n")

    return(df_data)
  }
  if (any(is.infinite(df_data[[var_gr]]))) {
    is.na(df_data[[var_gr]]) <- sapply(df_data[[var_gr]], is.infinite)
    df_data[[var_gr]] <- imputeTS::na_ma(df_data[[var_gr]], k = 1)
    warning("The -Inf/Inf present in var_gr are imputed by the mean between the
            two elements surrounding the center.\n")

    return(df_data)
  }
  else {
    return(df_data)
  }
}

# Function check sequence time for time series
check_time_series <- function(df_data) {
  df_tmp <- dplyr::group_by(df_data, Wells)
  df_tmp1 <- dplyr::mutate(df_tmp, Diff = round(Time - dplyr::lag(Time), digits = 3))
  df_tmp1[is.na(df_tmp1)] <- 0
  if (length(unique(df_tmp1[["Diff"]])) > 2) {
    stop("The sequence of time is not equally spaced, thus is not a time series")
  }
  df_tmp1
}
