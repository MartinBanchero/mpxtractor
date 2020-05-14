#' Function specific to read layout files .csv .
#'
#' This function recive as argument a dataframe (data) which is given for
#' one of the functions(read_spectramax_data, read_multiscango_data,
#' read_fluorstar_data). Also, the type of machine(spectramax, multiscango or fluorstar)
#' and the path to the layout files. This function can take many layout files as
#' argument.
#'
#' \code{read_layout_files} returns the data in the .csv as tibble
#' data frame
#'
#' @export
#' @examples
#' file_path <- system.file("extdata", "spectraMax_layout_plate1.csv",
#' package = "mpxtractor")
#'
#' # Data is store as a tibble
#' data_layout <- read_layout_files(data, reader_type ="spectramax"
#'    layout_files = file_path)
#'
#' # Now data is tidy
#' head(data)

combine_data_with_layout <- function(df_data, reader_type = NULL, dir_lyout_files = NULL,
                                     layout_file_pattern = NULL,
                                     layout_files = NULL,
                                     plate_names = NULL) {
  if (!is.data.frame(df_data)) stop("df_data should be a dataframe")
  check_type_of_reader(reader_type)

  files_layout <- get_input_read_multifiles(
    folder = dir_lyout_files,
    pattern = layout_file_pattern,
    filebyname = layout_files
  )

  lapply(files_layout, check_file_path_layout)
  if (is.null(plate_names)) {
    plate_names <- generate_plate_names_layout(files_layout)
  }
  if (length(files_layout) != length(plate_names)) {
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
  }, files_layout, plate_names)
  ## ADD functiones to combine each type
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
    return(dfr)
  }
  if (toupper(reader_type) == toupper("multiscango")) {
    dfr <- join_multiscango_and_layout(list_of_data_frames) # One function for each machine
    return(dfr)
  }
  if (toupper(reader_type) == toupper("fluorStar")) {
    dfr <- join_fluorstar_and_layout(list_of_data_frames) # One function for each machine
    return(dfr)
  }
}
