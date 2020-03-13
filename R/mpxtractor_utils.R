#' Check that one file is provided.
#'
#' @return Error if more than one file is given.
#' @seealso \code{\link{plater}}
#' @noRd
check_one_file_provided <- function(file) {
  if (length(file) > 1) {
    stop(paste0("Sorry, only one file should be provided, but you provided ",
                "multiple. Maybe you wanted read_plates()?"), call. = FALSE)
  }

}

#' Check the file path.
#'
#' @seealso \code{\link{plater}}
#' @return Error if the file is null or if the file does not exist.
#' Then check if the extension is .txt
#' @noRd
check_file_path <- function(file) {
  if (is.null(file) || !file.exists(file)) {
    stop(paste0("Sorry, can't find your file '", file, "'."),
         call. = FALSE)
  }
  if (!(grepl("[Tt][Xx][Tt]$", file))) {
    stop(paste0("Sorry, '", file, "'doesn't have a proper txt file extension."),
         call. = FALSE)
  }
}

#' Check that file is not empty
#'
#' @seealso \code{\link{plater}}
#' @return Error if the file is empty.
#' @noRd
check_that_file_is_non_empty <- function(file) {

  if (length(readLines(file)) == 0) {
    stop(paste0("Sorry, '", file, "' is empty and must not be."),
         call. = FALSE)
  }
}

#' Generate plate names
#' @return the names of the files
#' @seealso \code{\link{plater}}
#' @noRd
generate_plate_names <- function(files) {
  files <- regmatches(files, regexpr("[^/\\\\]*.[Tt][Xx][Tt]$", files))
  gsub(".[Tt][Xx][Tt]$", "", files)
}
