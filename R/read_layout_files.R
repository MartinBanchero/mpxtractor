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

read_layout_files <- function(data,dir_lyout_files = NULL,
                                layout_file_pattern = NULL,
                                layout_files = NULL,
                                plate_names = NULL){

  layout_files <- get_input_read_multifiles(folder = dir_lyout_files,
                                     pattern = layout_file_pattern,
                                     filebyname = layout_files)
  lapply(layout_files, check_file_path)
  if (is.null(plate_names))  {
    plate_names <- generate_plate_names(layout_files)
  }
  if (length(layout_files) != length(plate_names)) {
    stop("files and plate_names must have the same length.")
  }
  list_of_data_frames <- Map(f = function(layout_file, plate_name) {
    tryCatch(expr = {
      p <- plater::add_plate(data, layout_file, "well_ids_column")
      p$Plate <- plate_name
      p
    }, error = function(e) {
      e <- paste0("Error in file '", plate_name, "': ", e$message)
      stop(e, call. = FALSE)
    })
  }, layout_files, plate_names)
  result <- dplyr::bind_rows(list_of_data_frames)
  rownames(result) <- NULL
  result <- dplyr::select_(result, "Plate", ~dplyr::everything())
  result
}
