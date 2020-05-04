#' Function specific to read layout files (.csv).
#'
#' This function recive layout file properly formatted. This function is similar
#' to the function \code{read_plate()} from \cite{plater} package.
#'
#' @param file The path to a proper .csv file.
#'
#' @return Returns a tibble data frame whith different attributtes. The first column
#' is "Wells" this contain the names for each well (A01, A02..). The rest of the
#' attributes represent the different conditions.
#'
#' @section \code{file} format:
#'  The format that \code{read_layout_file()} receive is a .csv file previuosly
#'  formatted in a proper way. This mean that the separation between conditions is
#'  one empty line.
#'
#' @references
#' \insertRef{plater}{mpxtractor}
#'
#' @export
#' @examples
#' file_path <- system.file("extdata", "test_spectraMax_layout.csv", package = "mpxtractor")
#'
#' # Data is store as a tibble
#' data <- read_layout_file(
#'   file = file_path
#' )
#'
#' # Now data is tidy
#' head(data)
#'
#'
#'
#'
# Main function
read_layout_file <- function(file, well_ids_column = "Wells") {
  check_that_only_one_file_is_provided(file)
  check_file_path_layout(file)
  check_that_file_is_non_empty(file)
  check_well_ids_column_name(well_ids_column)
  plate_size <- guess_plate_size(file)
  raw_file_list <- get_list_of_plate_layouts(file, plate_size)
  result <- convert_all_layouts(raw_file_list, plate_size)
  result <- combine_list_to_dataframe(result)
  colnames(result)[colnames(result) == "wellIds"] <- well_ids_column
  class(result) <- c("tbl_df", "tbl", "data.frame")
  result
}




# Check that one file is provided.
#
# Return error if more than one file is given.
#
# This function is borrowed from plater package.
#

# Check if more than one file is provided
#
# Throws an error if multiple files are provided.
check_that_only_one_file_is_provided <- function(file) {
  if (length(file) > 1) {
    stop(paste0("Sorry, only one file should be provided, but you provided ",
                "multiple. Maybe you wanted read_plates()?"), call. = FALSE)
  }
}


# Check the file path.
#
# Return error if the file is null or if the file does not exist.
# Then check if the extension is .txt.
#
# This function is borrowed from plater package.
#
check_file_path <- function(file) {
  if (is.null(file) || !file.exists(file)) {
    stop(paste0("Sorry, can't find your file '", file, "'."),
         call. = FALSE
    )
  }
  if (!(grepl("[Tt][Xx][Tt]$", file))) {
    stop(paste0("Sorry, '", file, "'doesn't have a proper .txt file extension."),
         call. = FALSE
    )
  }
}


# Check that file is not empty
#
# Return error if the file is empty.
#
# This function is borrowed from plater package.
#
check_that_file_is_non_empty <- function(file) {
  if (length(readLines(file)) == 0) {
    stop(paste0("Sorry, '", file, "' is empty and must not be."),
         call. = FALSE
    )
  }
}

# Check if well_ids_column is a valid string
#
# Throws an error if well_ids_column is null or an empty string
check_well_ids_column_name <- function(well_ids_column) {
  if (is.null(well_ids_column) || well_ids_column == "") {
    stop("Sorry, well_ids_column must not be NULL or an empty string.",
         call. = FALSE)
  }
}


# Guess the plate size from the column labels.
#
# Throws an error if the largest column label is not a valid number of columns
# for a standard plate size.
#
# @param A plate .csv file
# @return the size of the plate based on the column labels.
guess_plate_size <- function(file) {
  first_line <- read_lines(file, n = 1)

  first_line_vector <- strsplit(first_line, ",")[[1]]

  # remove title field
  first_line_vector <- first_line_vector[-1]

  number_of_columns <- max(as.numeric(first_line_vector))

  get_plate_size_from_number_of_columns(number_of_columns)
}


# Read in the data and convert to a list of plate layouts.
#
# @param file Path to the plate file
# @param plate_size The number of wells in the plate. Must be 12, 24, 48, 96 or
#                   384. Default 96.
get_list_of_plate_layouts <- function(file, plate_size) {
  # read in file
  raw_file <- read_lines(file)

  # get list of data frames with new columns
  number_of_rows <- number_of_rows(plate_size)

  number_of_plates <- calculate_number_of_plates(raw_file, number_of_rows)

  raw_file_list <- lapply(1:number_of_plates, FUN =
                            function(plate) {
                              first_row <- (plate - 1) * (number_of_rows + 1) + plate
                              last_row <- first_row + number_of_rows
                              raw_file[first_row:last_row]
                            }
  )

  raw_file_list
}


# Wrapper around convert_plate_to_column that reports which layout generates an
# error to help users find it.
#
# Catches any errors thrown and prefaces the error message with
# "Error in layout #x" where x is the number of the layout generating the error.
#
# @param raw_file_list The list of containing plates from readLines
# @param plate_size The number of wells in the plate. Must be 12, 24, 48, 96 or
#                   384. Default 96.
# @return A list of two-column data frames of the same length as the input list.
convert_all_layouts <- function(raw_file_list, plate_size) {

  convert <- function(f, layout_number) {
    tryCatch(
      expr = convert_plate_to_column(f, plate_size),
      error = function(e) {
        e <- paste0("Error in layout #", layout_number, ": ",
                    e$message)
        stop(e, call. = FALSE)
      }
    )
  }

  Map(f = convert, raw_file_list, 1:length(raw_file_list))
}

# Merge together list of dataframes into a single dataframe.
#
# @throws error is any column name is a duplicate, other than "wellIds"
#
# @param result List, with each element containing a dataframe, with a column
#        named "wellIds"
# @return A dataframe merged together from the list elements. Wells are included
#         if they're non-missing in at least one of the data frames, otherwise
#         omitted.
combine_list_to_dataframe <- function(result) {
  if (length(result) == 1) {
    result <- result[[1]]
  } else {
    # ensure that plate names are unique
    result <- check_unique_plate_names(result)

    # combine result into one data frame
    result <- Reduce(function(x, y) merge(x, y, by = "wellIds", all = TRUE),
                     result)
  }

  # only return rows which have value for more than the well ID
  keep <- rowSums(!is.na(result)) > 1

  result[keep, ]
}

# Returns the size of a plate given the number of columns.
#
# @param columns 4, 6, 8, 12, 24
# @return the size of the plate or throws error if invalid number of columns.
# @examples get_plate_size_from_number_of_columns(12)
get_plate_size_from_number_of_columns <- function(columns) {
  n <- plate_dimensions("PlateSize", "Columns", columns)

  if (length(n) == 0) {
    stop(paste0("Could not guess plate size from number of columns. ",
                "Invalid number of columns: ", columns), call. = FALSE)
  }

  n
}


# Helper function to return rows/columns/plate size from another value
#
# @param get The type of value to get ("Columns", "Rows", or "plate_size")
# @param from The type of value being provided ("Columns", "Rows", or "PlateSize")
# @param value The value
#
# @return the corresponding value, or an empty vector if invalid data supplied
plate_dimensions <- function(get, from, value) {
  dimensions <- data.frame(
    Columns    = c(4, 6, 8, 12, 24),
    Rows      = c(3, 4, 6, 8, 16),
    PlateSize  = c(12, 24, 48, 96, 384))

  which_row <- which(dimensions[[from]] == value)

  dimensions[which_row, get]
}


# Returns the number of rows in a plate of a given size.
#
# @param plate_size 12, 24, 48, 96, or 384 wells
# @return The number of rows in a plate of a given size.
# @examples number_of_rows(96)
number_of_rows <- function(plate_size) {
  # stops if plate_size not 12, 24, 48, 96, or 384 wells
  return(plate_size / number_of_columns(plate_size))
}


# Returns the number of columns in a plate of a given size.
#
# @param plate_size 12, 24, 48, 96, or 384 wells
# @return The number of columns in a plate of a given size.
# @examples number_of_columns(96)
number_of_columns <- function(plate_size) {
  n <- plate_dimensions("Columns", "PlateSize", plate_size)

  if (length(n) == 0) {
    stop(paste0("Invalid plate_size: ", plate_size,
                ". Must be 12, 24, 48, 96 or 384."), call. = FALSE)
  }

  n
}

# Calculate the number of plates contained in the file.
#
# Throws an error if the number of elements in `raw_file` cannot be parsed to
# an integer number of plates.
#
# @param raw_file A text vector with each element containing a comma-delimited
# row
# @param number_of_rows The expected number of rows for the given plate size
#
# @return the number of plates in the file
calculate_number_of_plates <- function(raw_file, number_of_rows) {
  is_integer <- function(x) x %% 1 == 0

  result <- (length(raw_file) + 1) / (number_of_rows + 2)
  if (is_integer(result)) {
    return(result)
  } else {
    # file might end in blank line in which case we shouldn't add one
    result <- (length(raw_file)) / (number_of_rows + 2)
    if (raw_file[length(raw_file)] == "" || is_integer(result)) {
      return(result)
    } else {
      stop(paste0("File length is incorrect. Must be a multiple of the ",
                  "number of rows in the plate plus a header row for each ",
                  "plate and a blank row between plates."),
           call. = FALSE)
    }
  }
}
# Returns a character vector of well IDs (e.g. A01..B10..H12) of length 12,
#              24, 48, 96, or 384 wells.
#
# @param plate_size 12, 24, 48, 96, or 384 wells
# @return  A character vector of well IDs (e.g. A01..B05..H12) of length 12,
#              24, 48, 96, or 384
# @examples get_well_ids(96)
get_well_ids <- function(plate_size) {
  cols <- number_of_columns(plate_size) # stops if not 12, 24, 48, 96, 384
  rows <- number_of_rows(plate_size)

  wells <- vapply(formatC(1:cols, width = 2, flag = "0"),
                  FUN = function(i) paste(LETTERS[1:rows], i, sep = ""),
                  FUN.VALUE = rep("character", rows))
  wells <- as.vector(t(wells))
  return(wells)
}

# Make sure all plate names are unique.
#
# If each plate (column in the final data frame) doesn't have a unique name,
# append ".#" where "#" is the column's number, to the name.
#
# @param result list of two-column data frames, where the names of the second
# columns will be checked for duplication.
#
# @return result with any duplicated names replaced
check_unique_plate_names <- function(result) {
  # get plate names
  plate_names <- vapply(result, FUN = function(x) colnames(x)[2],
                        FUN.VALUE = "character")

  if(any(duplicated(plate_names))) {
    duplicates <- which(duplicated(plate_names))

    # replace duplicate column names with .n
    result <- lapply(1:length(result), FUN = function(n) {
      if (n %in% duplicates) {
        new_name <- paste0(colnames(result[[n]])[2], ".", n)
        colnames(result[[n]])[2] <- new_name
        result[[n]]
      } else {
        result[[n]]
      }
    })
  }
  return(result)
}



# Wrapper around base::readLines
# Doesn't warn if there's an incomplete final line for the file
# See github issue 17, where layouts created on Mac created warning
# https://github.com/ropenscilabs/plater/issues/17
read_lines <- function(file, n = -1L) {
  readLines(file, n = n, warn = FALSE)
}
