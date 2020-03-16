#' Multiple plates files
#'
#' @export
read_multiple_plates <- function(reader_type = NULL,
                                 dirFiles = NULL,
                                 file_pattern = NULL,
                                 filesname = NULL,
                                 plate_names = NULL){

  files <- get_input_read_multifiles(folder = dirFiles,
                                     pattern = file_pattern,
                                     filebyname = filesname)
  lapply(files, check_file_path)
  if (is.null(plate_names))  {
    plate_names <- generate_plate_names(files)
  }
  if (length(files) != length(plate_names)) {
    stop("files and plate_names must have the same length.")
  }
  if (is.null(reader_type)) {
    stop("Sorry, one reader type must to be specified.")
  }
  if (toupper(reader_type) != toupper("spectramax") && toupper(reader_type) !=
      toupper("multiscango") && toupper(reader_type) != toupper("fluorstar")) {
    stop("Sorry,
    the micro-plate readers must to be spectramax, multiscango or fluorstar.")
  }
  list_of_data_frames <- Map(f = function(file, plate_name) {
    tryCatch(expr = {
      p <- type_of_reader(file, reader_type)
      p$Plate <- plate_name
      p
    }, error = function(e) {
      e <- paste0("Error in file '", plate_name, "': ", e$message)
      stop(e, call. = FALSE)
    })
  }, files, plate_names)
  result <- dplyr::bind_rows(list_of_data_frames)
  rownames(result) <- NULL
  result <- dplyr::select_(result, "Plate", ~dplyr::everything())
  result
}

# Get input to give to the function that read multiple files @return filenames
# string
get_input_read_multifiles <- function(folder = NULL, pattern = NULL, filebyname = NULL) {
  if (is.null(folder) && !is.null(pattern) && is.null(filebyname)) {
    stop(paste0("Sorry, dirFiles must to be given."))
  }
  if (!is.null(folder) && !is.null(pattern) && !is.null(filebyname)) {
    stop(paste0("Sorry, filebyname cannnot be used with dirFiles or pattern"))
  }
  if (!is.null(folder)) {
    lst_files <- do.call(get_files, list(folder, pattern))
    return(lst_files)
  }
  if (!is.null(filebyname)) {
    lst_files <- filebyname
    return(lst_files)
  }

}


# List the files present in the given folder and if pattern is given look for
# files with that pattern
get_files <- function(folder, pattern = NULL) {
  if (is.null(pattern)) {
    lst_files <- list.files(path = folder, recursive = TRUE, full.names = TRUE)
    return(lst_files)
  }
  lst_files <- list.files(path = folder, pattern = pattern, recursive = TRUE,
                          full.names = TRUE)
  return(lst_files)
}


# Receive the name of the machine and call the specific function to read the
# data.
type_of_reader <- function(file, reader_type) {
  if (toupper(reader_type) == toupper("spectramax")) {
    p <- read_spectramax_data(file)  #One function for each machine
    return(p)
  }
  if (toupper(reader_type) == toupper("multiscango")) {
    p <- read_multiscango_data(file) #One function for each machine
    return(p)
  }
  if (toupper(reader_type) == toupper("fluorStar")) {
    # p <- read_fluorstar_data(file) #One function for each machine
    print("fluorStar")
  }

}
