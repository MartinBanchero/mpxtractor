
calculate_growth_rates <- function(df_data, var_gr, windowsize) {
  df_data <- dplyr::mutate(df_data, Diff_time = Time[2] - Time[1])
  timestep <- windowsize / 60 # hs
  windowlength <- (windowsize / df_data$Diff_time[2])

  if ((windowlength %% 2) == 0) {
    windowlength <- windowlength + 1
  }
  # Calculate growth rate (mu) using Savitzky and Golay filter(savgol).
  df_data <- dplyr::group_by(df_data, Wells)
  mu <- dplyr::group_map(df_data, ~ signal::sgolayfilt(.x[[var_gr]],
    p = 1,
    n = windowlength,
    m = 1,
    ts = timestep
  ))

  df_data$mu <- as.vector(unlist(mu))
  return(df_data)
}
