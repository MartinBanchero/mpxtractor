# Combine spectra data with layout data
combine_data <- function(spectramax_data, layout_file) {
  sp_data <- mpxtractor::read_spectramax_data(spectramax_data)
  sp_data_layout <- mpxtractor::read_layout_files(sp_data,
    reader_type = "spectramax",
    layout_files = layout_file
  )
  return(sp_data_layout)
}

# Time in hs
get_time_in_hh <- function(time.col) {
  sapply(strsplit(time.col, ":"), function(x) {
    x <- as.numeric(x)
    if (length(x) <= 2) {
      x[1] / 60 + x[2] / 3600
    }
    else {
      x[1] + x[2] / 60 + x[3] / 3600
    }
  })
}

# Time in minutes
get_time_in_mm <- function(time.col) {
  sapply(strsplit(time.col, ":"), function(x) {
    x <- as.numeric(x)
    if (length(x) <= 2) {
      x[1] + x[2] / 60
    }
    else {
      x[1] * 60 + x[2] + x[3] / 60
    }
  })
}

format_time <- function(sp_data, time = NULL) {
  if (is.null(time)) {
    Time <- get_time_in_hh(sp_data$`Time(hh:mm:ss)`)
    # Move column time to the front
    sp_data <- cbind(Time, sp_data)
    # Clean the dataframe
    sp_tidy_data <- dplyr::select(
      sp_data,
      c(
        -`Time(hh:mm:ss)`,
        -`Temperature(°C)`,
        -Plate
      )
    )
    return(sp_tidy_data)
  }


  if (toupper(time) == toupper("minutes")) {
    Time <- get_time_in_mm(sp_data$`Time(hh:mm:ss)`)

    # Move column time to the front
    sp_data <- cbind(Time, sp_data)

    # Clean the dataframe
    sp_tidy_data <- dplyr::select(
      sp_data,
      c(
        -`Time(hh:mm:ss)`,
        -`Temperature(°C)`,
        -Plate
      )
    )
    return(sp_tidy_data)
  }

  if (!is.null(time) && toupper(time) != toupper("minutes")) {
    stop("The time unit is not recognized. The default unit is in hours, if you want
    the convertion in minutes you should give time = minutes.")
  }
}

# background correction
background_correction_impute <- function(sp_data_layout) {
  df_tmp_gr <- dplyr::group_by(sp_data_layout, Wells)
  df_tmp_gr <- dplyr::mutate(df_tmp_gr, min_measurement = min(Measurement))

  df_tmp_gr$bg_corrected <- (df_tmp_gr$Measurement - df_tmp_gr$min_measurement)
  # log transform of bg corrected measurements
  df_tmp_gr$log_measurement <- log(df_tmp_gr$bg_corrected)
  # replace -inf by NA
  is.na(df_tmp_gr$log_measurement) <- sapply(df_tmp_gr$log_measurement, is.infinite)
  # impute data
  df_tmp_gr$log_measu_impute <- imputeTS::na_ma(df_tmp_gr$log_measurement, k = 1)

  return(df_tmp_gr)
}

calculate_growth_rate <- function(df_tmp_gr, windowsize) {
  df_tmp_gr <- dplyr::mutate(df_tmp_gr, Diff_time = Time[2] - Time[1])
  timestep <- windowsize / 60 # hs
  windowlength <- (windowsize / df_tmp_gr$Diff_time[2])

  if ((windowlength %% 2) == 0) {
    windowlength <- windowlength + 1
  }
  # Calculate growth rate (mu) using Savitzky and Golay filter(savgol).
  df_tmp_gr <- dplyr::group_by(df_tmp_gr, Wells)
  mu <- dplyr::group_map(df_tmp_gr, ~ sgolayfilt(.x$log_measu_impute,
    p = 1,
    n = windowlength,
    m = 1,
    ts = timestep
  ))

  df_tmp_gr$mu <- as.vector(unlist(mu))
  return(df_tmp_gr)
}
