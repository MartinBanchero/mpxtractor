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

generate_plate_names_layout <- function(files) {
  files <- regmatches(files, regexpr("[^/\\\\]*.[Cc][Ss][Vv]$", files))
  gsub(".[Cc][Ss][Vv]$", "", files)
}

# Generate plate names of data files
#
# Return the names of the files to use as a plate name.
#
generate_plate_names_data_files <- function(files) {
  files <- regmatches(files, regexpr("[^/\\\\]*.[Tt][Xx][Tt]$", files))
  gsub(".[Tt][Xx][Tt]$", "", files)
}


# Get input to give to the function read_multiple_files().
#
# This function can take different arguments. Can retrive the files given in one
# folder, or matching a pattern, or pass the directly the names useing filebyname.
#
# This return a list of file paths.
get_input_read_multifiles <- function(folder = NULL,
                                      pattern = NULL,
                                      filebyname = NULL) {
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

# Verified from which microplate reader the files are.
#
# Recive as argument one string with the name of the microplate reader type.
#
# Return an error if there is no match.
check_type_of_reader <- function(reader_type, time_interval, input_type) {
  if (is.null(reader_type)) {
    stop("Sorry, one reader type must to be specified.")
  }
  if (toupper(reader_type) != toupper("spectramax") && toupper(reader_type) !=
    toupper("multiscango") && toupper(reader_type) != toupper("fluorstar")) {
    stop("Sorry,
      the micro-plate readers must be spectramax, multiscango or fluorstar.")
  }
  if (toupper(reader_type) == toupper("spectramax") && !is.null(input_type)) {
    stop("Sorry,
      the input_type argument is only valid for multiscango analysis.")
  }
  if (toupper(reader_type) == toupper("fluorstar") && !is.null(input_type)) {
    stop("Sorry,
      the input_type argument is only valid for multiscango analysis.")
  }

  if (toupper(reader_type) == toupper("multiscango") && is.null(input_type)) {
    stop("Sorry, the input_type argument is missing")
  }
  if (toupper(reader_type) == toupper("multiscango") && !is.null(time_interval)) {

    check_time_interval(time_interval)
  }
}

check_one_file_provided <- function(file) {
  if (length(file) > 1) {
    stop(paste0(
      "Sorry, only one file should be provided, but you provided ",
      "multiple."
    ), call. = FALSE)
  }
}


check_layout_file_path <- function(file) {
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
