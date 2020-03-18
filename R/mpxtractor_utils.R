#' Check that one file is provided.
#'
#' @return Error if more than one file is given.
#' @seealso \code{\link{plater}}
#' @noRd
check_one_file_provided <- function(file) {
  if (length(file) > 1) {
    stop(paste0(
      "Sorry, only one file should be provided, but you provided ",
      "multiple. Maybe you wanted read_plates()?"
    ), call. = FALSE)
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
      call. = FALSE
    )
  }
  if (!(grepl("[Tt][Xx][Tt]$", file))) {
    stop(paste0("Sorry, '", file, "'doesn't have a proper txt file extension."),
      call. = FALSE
    )
  }
}

#' Check the file path and extension of layout file.
#'
#' @seealso \code{\link{plater}}
#' @return Error if the file is null or if the file does not exist.
#' Then check if the extension is .csv
#' @noRd
check_file_path_layout <- function(file) {
  if (is.null(file) || !file.exists(file)) {
    stop(paste0("Sorry, can't find your file '", file, "'."),
      call. = FALSE
    )
  }
  if (!(grepl("[Cc][Ss][Vv]$", file))) {
    stop(paste0("Sorry, '", file, "'doesn't have a proper .csv file extension."),
      call. = FALSE
    )
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
      call. = FALSE
    )
  }
}

#' Generate plate names
#' @return the names of the files
#' @seealso \code{\link{plater}}
#' @noRd
generate_plate_names_layout <- function(files) {
  files <- regmatches(files, regexpr("[^/\\\\]*.[Cc][Ss][Vv]$", files))
  gsub(".[Cc][Ss][Vv]$", "", files)
}


#' Get input to give to the function that read multiple files.
#'
#' @return filenames
#' @noRd

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

check_type_of_reader <- function(reader_type) {
  if (is.null(reader_type)) {
    stop("Sorry, one reader type must to be specified.")
  }
  if (toupper(reader_type) != toupper("spectramax") && toupper(reader_type) !=
    toupper("multiscango") && toupper(reader_type) != toupper("fluorstar")) {
    stop("Sorry,
      the micro-plate readers must to be spectramax, multiscango or fluorstar.")
  }
}


