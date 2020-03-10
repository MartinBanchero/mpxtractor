read_multiple_plates <- function(dirFiles = NULL , file_pattern = NULL,
                                 filesname = NULL,
                                 plate_names = NULL)
{
  files <- get_input_read_multifiles(folder = dirFiles,
                                     pattern = file_pattern,
                                     filebyname = filesname)
  lapply(files, check_file_path)
  if (is.null(plate_names)) {
    plate_names <- generate_plate_names(files)
  }
  if (length(files) != length(plate_names)) {
    stop("files and plate_names must have the same length.")
  }
  list_of_data_frames <- Map(f = function(file, plate_name) {
    tryCatch(expr = {
      p <- read_spectraMax_data(file)#One function for each machine
      p$Plate <- plate_name
      p
    }, error = function(e) {
      e <- paste0("Error in file '", plate_name, "': ",
                  e$message)
      stop(e, call. = FALSE)
    })
  }, files, plate_names)
  result <- dplyr::bind_rows(list_of_data_frames)
  rownames(result) <- NULL
  result <- dplyr::select_(result, "Plate", ~dplyr::everything())
  result
}
