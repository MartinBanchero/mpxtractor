#'Function specific to read layout files .csv .
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
#' data <- read_layout_files(
#'    file = file_path)
#'
#' # Now data is tidy
#' head(data)

read_layout_files <- function(data, reader_type = NULL, dir_lyout_files = NULL,
                              layout_file_pattern = NULL,
                              layout_files = NULL,
                              plate_names = NULL) {#Cheack how to wrapp the param
  files_layout <- get_input_read_multifiles( folder = dir_lyout_files,
                                             pattern = layout_file_pattern,
                                             filebyname = layout_files)
  check_type_of_reader(reader_type)
  lapply(files_layout, check_file_path_layout)
  if (is.null(plate_names)) {
    plate_names <- generate_plate_names_layout(files_layout)
  }
  if (length(files_layout) != length(plate_names)) {
    stop("files and plate_names must have the same length.")
  }
  list_of_data_frames <- Map(f = function(file, plate_name) {
    tryCatch(expr = {
      p <- plater::add_plate(data, file, well_ids_column = "Wells")
      p$Plate <- plate_name
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


join_multiscango_and_layout <- function(list_of_data_frames){
  result <- purrr::reduce(list_of_data_frames,
                          full_join,
                          by = c("Wells","Readings", "Measurement"))
  rownames(result) <- NULL
  df_result <- result %>% dplyr::select_("Plate", everything())
  return(df_result)
}

join_spectramax_and_layout <- function(list_of_data_frames){
  print("Hi spec")
}

join_fluorstar_and_layout <- function(list_of_data_frames){
  print("Hi flu")
}

# Receive the name of the machine and call the specific function to read the
# data.
reader_df_format <- function(list_of_data_frames, reader_type) {
  if (toupper(reader_type) == toupper("spectramax")) {
    dfr <- join_spectramax_and_layout(list_of_data_frames)  #One function for each machine
    return(dfr)
  }
  if (toupper(reader_type) == toupper("multiscango")) {
    dfr <- join_multiscango_and_layout(list_of_data_frames) #One function for each machine
    return(dfr)
  }
  if (toupper(reader_type) == toupper("fluorStar")) {
    dfr <- join_fluorstar_and_layout(list_of_data_frames) #One function for each machine
    return(dfr)
  }
}
