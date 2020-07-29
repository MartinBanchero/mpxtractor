#' Function specific to read output files (.txt) from multiscanGO readers machines
#'  for output format list.
#'
#' This function receive one output file from multiscango plate reader and
#' generate a tibble dataframe.
#'
#' @param file The path to a proper .txt file formatted by the multiscango machine
#'  for the option list.
#'
#' @return Returns a tibble data frame with all the columns present in the file,
#' in addition to this one column that contained time in hh:mm:ss format.
#'
#' @importFrom rlang .data
#' @importFrom stats median
#' @importFrom utils type.convert
#'
read_multiscan_list_output <- function(file) {
  raw_lines <- readLines(file)
  header_fields <- c(
    "Plate", "Well", "Group", "Type", "Sample", "Wavelength",
    "Reading", "Abs", "Meas. Time"
  )

  headers <- get_headers(raw_lines[1], header_fields)
  header_flags <- set_headers_flags(headers, header_fields)

  df_data <- data.frame(txt = as.character(raw_lines[2:length(raw_lines)]))
  df_data$txt <- as.character(df_data$txt)

  df_data <- as.data.frame(
    apply(df_data, 2, process_record, header_flags = header_flags)
  )
  names(df_data) <- headers

  df_data <- dplyr::mutate_all(
    .tbl = tidyr::as_tibble(df_data),
    type.convert, as.is = TRUE
  )
  df_data <- dplyr::rename(
    df_data,
    Wells = .data$Well,
    Measurement = .data$Abs,
    Meas.time.sec = .data$`Meas. Time`
  )
  df_data <- get_time_interval(df_data)
  df_data <- convert_seconds_to_hhmmss(df_data)
  return(df_data)
}

# get the attribute in header
# first_record: First record from text file
get_headers <- function(first_record, header_fields) {
  coltype <- c()
  positions <- stringr::str_locate_all(
    first_record,
    paste(header_fields,
      collapse = "|"
    )
  )
  # check available att from list header_fields
  lst_headers_allow <- unlist(
    lapply(as.list(1:dim(positions[[1]])[1]), function(x) {
      p <- positions[[1]][x[1], ]
      substr(first_record, p[1], p[2])
    })
  )
  return(lst_headers_allow)
}

# Check if the attribute is present in all possible attributes
set_headers_flags <- function(he, header_fields) {
  header_flags <- c()
  for (h in header_fields) {
    if (h %in% he) {
      header_flags[h] <- TRUE
    } else {
      header_flags[h] <- FALSE
    }
  }

  return(header_flags)
}



# parsing functions for each possible attribute
parse_plate <- function(txt) {
  return(stringr::str_extract(txt, pattern = "^Plate\\s[0-9]+"))
}

parse_wells <- function(txt) {
  return(stringr::str_extract(txt, pattern = "[A-Z]{1}[0-9]{2}"))
}

parse_group <- function(txt) {
  return(stringr::str_trim(stringr::str_extract(txt, pattern = "\\s[A-Z][a-z]*\\s")))
}

parse_type <- function(txt) {
  option1 <- stringr::str_trim(stringr::str_extract(
    txt,
    pattern = "\\s[A-Z][a-z]*(\\s+)([0-9]|[A-Z][a-z]*\\_[A-Z][a-z]*)\\s"
  ))
  if (!is.na(option1[1])) {
    return(option1)
  } else {
    option2 <- stringr::str_trim(stringr::str_extract_all(
      txt,
      pattern = "\\sUnknown*\\s"
    ))
    return(option2)
  }
}

parse_sample <- function(txt) {
  return(stringr::str_trim(
    stringr::str_extract(
      txt,
      pattern = "\\s[0-9]+\\/[0-9]+\\s"
    )
  ))
}

parse_wavelenght <- function(txt) {
  return(stringr::str_trim(
    stringr::str_extract(
      txt,
      pattern = "\\s[0-9]{3}\\s"
    )
  ))
}

parse_reading <- function(txt) {
  return(stringr::str_trim(
    stringr::str_extract(
      txt,
      pattern = "\\s{3}[0-9]{1}\\s"
    )
  ))
}

parse_abs <- function(txt) {
  return(stringr::str_trim(
    stringr::str_extract(
      txt,
      pattern = "\\s[0-9]{1}\\.[0-9]{2,4}\\s"
    )
  ))
}

parse_measure_sec <- function(txt) {
  return(stringr::str_trim(
    stringr::str_extract(
      stringr::str_trim(txt),
      pattern = "\\s[0-9]{1,6}\\.[0-9]{3,4}$"
    )
  ))
}


process_record <- function(rec, header_flags) {
  rec <- stringr::str_trim(rec)
  df <- data.frame(
    if (header_flags["Plate"]) parse_plate(rec) else NA,
    if (header_flags["Well"]) parse_wells(rec) else NA,
    if (header_flags["Group"]) parse_group(rec) else NA,
    if (header_flags["Type"]) parse_type(rec) else NA,
    if (header_flags["Sample"]) parse_sample(rec) else NA,
    if (header_flags["Wavelength"]) parse_wavelenght(rec) else NA,
    if (header_flags["Reading"]) parse_reading(rec) else NA,
    if (header_flags["Abs"]) parse_abs(rec) else NA,
    if (header_flags["Meas. Time"]) parse_measure_sec(rec) else NA
  )
  df <- df[, colSums(is.na(df)) != nrow(df)] # delete columnas with NA
  return(df)
}

# Function to generate the time interval from the column Meas.time.sec.
# The diff value for the time attribute is calculated by taking the median.
get_time_interval <- function(df_result) {
  df_result <- dplyr::group_by(df_result, .data$Wells)
  df_result <- dplyr::mutate(
    df_result,
    Diff = .data$Meas.time.sec - dplyr::lag(.data$Meas.time.sec, default = .data$Meas.time.sec[1])
  )
  df_result <- dplyr::group_by(df_result, .data$Wells)
  df_result <- dplyr::mutate(df_result, interval = median(.data$Diff, na.rm = TRUE))
warning("The unique time interval is calculated by taking the median of Diff time by group.")
  return(df_result)
}

# Add column time in format hh:mm:ss from seconds
convert_seconds_to_hhmmss <- function(df) {
  measures_per_well <- dplyr::count(df)
  total_time <- (df[["interval"]] * (max(measures_per_well[, 2]) - 1))
  total_time <- max(unique(total_time)) # to check  unique return
  df_result <- dplyr::group_by(df, .data$Wells)
  df_result <- dplyr::mutate(
    df_result,
    Time = seq(0, total_time, by = max(unique(df[["interval"]])))
  )

  df_result[["Time"]] <- round(df_result[["Time"]],digits = 2)
  df_result[["Time"]] <- as.character(hms::as_hms(df_result[["Time"]] ) )
  df_result <- dplyr::select(df_result, -c(.data$Diff, .data$interval))

  df_result <- dplyr::relocate(
    df_result,
    .data$Wells,
    .data$Time,
    .data$Measurement,
    .before = .data$Plate
  )
  return(df_result)
}
