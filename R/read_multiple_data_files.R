#' Multiple plates files
#'
#' This function can recive as argument multiple raw data files and join them.
#'
#' @param reader_type to specified the type of reader machine
#' @param dirFiles to specified the directory
#' @param file_pattern pattern to match filenames
#' @param filesname name of the files to be read
#' @param plate_names to add the name of the plate
#' @param time_interval used only if the input is multiscango
#'  \code{read_multiple_data_files} returns a dataframe with the data in
#' the raw files as dataframe.
#'
#'
#' @export
#' @examples
#' file_path <- system.file(
#'  "extdata",
#'  c("test_spectramax_data_1.txt", "test_spectramax_data_2.tx"),
#'  package = "mpxtractor"
#' )
#'
#' data_multiple_spectramax <- mpxtractor::read_multiple_data_files(
#' reader_type = "spectramax",
#' filesname = file_path
#' )
#'
#'
read_multiple_data_files <- function(reader_type,
                                    time_interval = NULL, dirFiles = NULL,
                                    file_pattern = NULL,
                                    filesname = NULL,
                                    plate_names = NULL) {
  check_type_of_reader(reader_type, time_interval)
  files <- get_input_read_multifiles(
    folder = dirFiles, pattern = file_pattern,
    filebyname = filesname
  )

  lapply(files, check_file_path)
  if (is.null(plate_names)) {
    plate_names <- generate_plate_names_data_files(files)
  }
  if (length(files) != length(plate_names)) {
    stop("files and plate_names must have the same length.")
  }
  list_of_data_frames <- Map(f = function(file, plate_name) {
    tryCatch(expr = {
      p <- type_of_reader(file, reader_type, time_interval)
      p$plate_filename <- plate_name
      p
    }, error = function(e) {
      e <- paste0("Error in file '", plate_name, "': ", e$message)
      stop(e, call. = FALSE)
    })
  }, files, plate_names)
  result <- dplyr::bind_rows(list_of_data_frames)
  rownames(result) <- NULL
  result <- dplyr::select_(result, "Wells", ~ dplyr::everything())
  result <- dplyr::group_by(result, .data$Wells)
  #result <- dplyr::arrange(result, .data$plate_filename)
  result <- dplyr::ungroup(result)

  result
}


# List the files present in the given folder and if pattern is given look for
# files with that pattern
get_files <- function(folder, pattern = NULL) {
  if (is.null(pattern)) {
    lst_files <- list.files(path = folder, recursive = TRUE, full.names = TRUE)
    return(lst_files)
  }
  lst_files <- list.files(
    path = folder, pattern = pattern, recursive = TRUE,
    full.names = TRUE
  )
  return(lst_files)
}


# Receive the name of the machine and call the specific function to read the
# data.
type_of_reader <- function(file, reader_type, time_point) {
  if (toupper(reader_type) == toupper("spectramax")) {
    p <- read_spectramax_data(file)  #One function for each machine
    return(p)
  }
  if (toupper(reader_type) == toupper("multiscango") && !is.null(time_point)) {
    p <- read_multiscango_data(file, time_point) #One function for each machine
    return(p)
  }
  if (toupper(reader_type) == toupper("fluorStar")) {
    p <- read_fluorstar_data(file) #One function for each machine
    return(p)
  }
}

