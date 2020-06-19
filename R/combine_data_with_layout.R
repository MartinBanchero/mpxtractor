#' Function specific to read layout files .csv
#'
#' This function recive as argument a dataframe (data) which is given for
#' one of the functions(read_spectramax_data, read_multiscango_data,
#' read_fluorstar_data). Also, the type of machine(spectramax, multiscango or fluorstar)
#' and the path to the layout files. This function can take many layout files as
#' argument.
#'
#' \code{combine_data_with_layout} returns a dataframe with the data in
#' the raw files and in the layout files.
#'
#' @param df_data dataframe tidy
#' @param reader_type string with the name of one the reader machines
#' @param dir_layout_files specified the ddirectory in which gather the files
#' @param layout_file_pattern specified pattern to match the file names
#' @param layout_files name of layout files
#' @param layout_file_name optional argument to assign name to plates
#'
#'
#'
#' @export
#' @examples
#' file_path <- system.file("extdata", "test_spectraMax_layout_1.csv",
#' package = "mpxtractor")
#'
#' data(df_spectramax_outdata_1)
#'
#' # Data is store as a tibble
#' data_layout <- combine_data_with_layout(
#'  df_spectramax_outdata_1,
#'  reader_type = "spectramax",
#'  layout_files = file_path
#' )
#'
#' # Now data is tidy
#' head(data_layout)

combine_data_with_layout <- function(df_data, reader_type , dir_layout_files = NULL,
                                     layout_file_pattern = NULL,
                                     layout_files = NULL,
                                     layout_file_name = NULL) {
  if (!is.data.frame(df_data)) stop("df_data should be a dataframe")

  check_reader(reader_type)

  files_layout <- get_input_read_multifiles(
    folder = dir_layout_files,
    pattern = layout_file_pattern,
    filebyname = layout_files
  )

  lapply(files_layout, check_file_path_layout)
  if (is.null(layout_file_name)) {
    layout_file_name <- generate_plate_names_layout(files_layout)
  }
  if (length(files_layout) != length(layout_file_name)) {
    stop("files and plate_names must have the same length.")
  }
  list_of_data_frames <- Map(f = function(file, plate_name) {
    tryCatch(expr = {
      p <- add_plate_file(df_data, file, well_ids_column = "Wells")
      p$layout_file <- plate_name
      p
    }, error = function(e) {
      e <- paste0("Error in file '", plate_name, "': ", e$message)
      stop(e, call. = FALSE)
    })
  }, files_layout, layout_file_name)
  ## ADD functions to combine each type
  df_result <- reader_df_format(list_of_data_frames, reader_type)
  df_result
}


join_multiscango_and_layout <- function(list_of_data_frames) {
  result <- purrr::reduce(list_of_data_frames,
    dplyr::full_join,
    by = c("Wells", "Reading", "Measurement")
  )

  rownames(result) <- NULL
  return(result)
}


join_spectramax_and_layout <- function(list_of_data_frames) {
  result <- purrr::reduce(list_of_data_frames,
    dplyr::full_join,
    by = c(
      "Wells",
      "Time",
      "Temperature",
      "Measurement"
    )
  )

  rownames(result) <- NULL
  return(result)
}

join_fluorstar_and_layout <- function(list_of_data_frames) {
  result <- purrr::reduce(list_of_data_frames,
    dplyr::full_join,
    by = c(
      "Wells",
      "Sample",
      "Time",
      "Measurement"
    )
  )
  rownames(result) <- NULL
  return(result)
}

# Receive the name of the machine and call the specific function to read the
# data.
reader_df_format <- function(list_of_data_frames, reader_type) {
  if (toupper(reader_type) == toupper("spectramax")) {
    dfr <- join_spectramax_and_layout(list_of_data_frames) # One function for each machine
    if (!is.null(.data$plate_filename)) {
      dfr_time <- format_time(dfr)# Time to hs to avoid time to be sort alphabetically
      dfr_time <- dplyr::group_by(dfr_time, .data$Wells, .data$plate_filename)
      dfr_time <- dplyr::arrange(dfr_time, .data$Time, .by_group = TRUE)
      dfr_time <- get_time_hhmmss(dfr_time)
      return(dfr_time)
    }
    return(dfr)
  }
  if (toupper(reader_type) == toupper("multiscango")) {
    dfr <- join_multiscango_and_layout(list_of_data_frames) # One function for each machine
    if (!is.null(.data$plate_filename)) {
      dfr <- dplyr::group_by(dfr, .data$Wells, .data$plate_filename)
      dfr <- dplyr::arrange(dfr, .data$Reading, .by_group = TRUE)
      return(dfr)
    }
    return(dfr)
  }
  if (toupper(reader_type) == toupper("fluorStar")) {
    dfr <- join_fluorstar_and_layout(list_of_data_frames) # One function for each machine
    if (!is.null(.data$plate_filename)) {#If there is more than one data file in the df
      dfr_time <- format_time(dfr)# Time to hs to avoid time to be sort alphabetically
      dfr_time <- dplyr::group_by(dfr_time, .data$Wells, .data$plate_filename)
      dfr_time <- dplyr::arrange(dfr_time, .data$Time, .by_group = TRUE)
      dfr_time <- get_time_hhmmss(dfr_time)
      return(dfr_time)
    }
    return(dfr)
  }
}

# check the reader type from which the dataframe was crated
check_reader <- function(reader_type, time_point) {
  if (is.null(reader_type)) {
    stop("Sorry, one reader type must to be specified.")
  }
  if (toupper(reader_type) != toupper("spectramax") && toupper(reader_type) !=
      toupper("multiscango") && toupper(reader_type) != toupper("fluorstar")) {
    stop("Sorry,
      the micro-plate readers must to be spectramax, multiscango or fluorstar.")
  }
}
