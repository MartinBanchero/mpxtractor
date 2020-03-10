std_format_df <- function(df){    # Format the df
  well_ids <- 3:ncol(df)
  df[, well_ids] <- as.numeric(sub(",", ".", as.character(unlist(df[, well_ids]))))

  # Apply dplyr
  df <- dplyr::gather(df,
               key = 'Wells',
               value = 'Measurement', c(-1,-2))

  df$Wells <- gsub('(^[A-Z])([0-9]$)', '\\10\\2', df$Wells)

  df_measurements <- dplyr::select(df, Wells, everything()) #Swap the well column to the first column
  df_measurements
}
