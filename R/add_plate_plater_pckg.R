add_plate_file <- function(data, file, well_ids_column) {
  if ("data.frame" %in% class(file) && class(data) == "character") {
    warning("file and class arguments to add_plate appear to be reversed.")
    temp <- data
    data <- file
    file <- temp
  }
  original_class <- class(data)
  to_add <- read_layout_file(file, "wellIds")
  check_well_ids_column_name(well_ids_column)
  validate_column_is_in_data(data, well_ids_column)
  missing_leading_zeroes <- are_leading_zeroes_missing(
    data,
    well_ids_column,
    guess_plate_size(file)
  )
  if (missing_leading_zeroes) {
    to_add$wellIds <- remove_leading_zeroes(to_add$wellIds)
  }
  if (!(all(to_add$wellIds %in% data[[well_ids_column]]))) {
    stop(wrong_wells_error_message(
      data, well_ids_column,
      to_add
    ))
  }
  result <- merge(data, to_add,
    by.x = well_ids_column, by.y = "wellIds",
    all.x = TRUE
  )
  user_order <- order(match(result[[well_ids_column]], data[[well_ids_column]]))
  result <- result[user_order, ]
  class(result) <- union(original_class, c(
    "tbl_df", "tbl",
    "data.frame"
  ))
  result
}

# Check if well_ids_column is a valid string
#
# Throws an error if well_ids_column is null or an empty string
check_well_ids_column_name <- function(well_ids_column) {
  if (is.null(well_ids_column) || well_ids_column == "") {
    stop("Sorry, well_ids_column must not be NULL or an empty string.",
      call. = FALSE
    )
  }
}

# Throws an error if col_name is not a column in data.
#
# @param data A data frame
# @param col_name The name of the column of well IDs
validate_column_is_in_data <- function(data, col_names) {
  if (any(!(col_names %in% colnames(data)))) {
    the_offender <- which(!(col_names %in% colnames(data)))
    the_offender <- col_names[the_offender]

    if (length(the_offender) > 1) {
      the_offender <- paste0(the_offender, collapse = ", ")
      the_offender <- paste0("are no columns named ", the_offender)
    } else {
      the_offender <- paste0("is no column named '", the_offender, "'")
    }

    stop(paste0("There ", the_offender, " in your data frame."),
      call. = FALSE
    )
  }
}

# Returns TRUE if leading zeroes are missing.
#
# Stops if some leading zeroes are missing and others not or if invalid well IDs
# are present.
#
# @param data The data frame containing the well IDs.
# @param well_ids_column The name of the column containing the well IDs.
# @return TRUE if leading zeroes are missing or FALSE if all leading zeroes are
# missing.
are_leading_zeroes_missing <- function(data, well_ids_column, plate_size) {
  if ((all(data[[well_ids_column]] %in% get_well_ids(plate_size)))) {
    return(FALSE)
  } else {
    if (!are_leading_zeroes_valid(data, well_ids_column, plate_size)) {
      # leading zeroes are invalid
      if (!(all(data[[well_ids_column]] %in%
        get_well_ids_without_leading_zeroes(plate_size)))) {
        # some missing leading zeroes, some not, give up
        stop("Invalid well IDs--some have leading zeroes and some don't.",
          call. = FALSE
        )
      } else {
        return(TRUE)
      }
    } else {
      # problem is not with leading zeroes
      stop("Some well IDs are invalid.", call. = FALSE)
    }
  }
}


# Returns wells with leading zeroes removed.
#
# @param wells A character vector of well IDs
# @return wells with leading zeroes removed (e.g. A1 rather than A01)
remove_leading_zeroes <- function(wells) {
  wells <- ifelse(substr(wells, 2, 2) == "0",
    paste0(substr(wells, 1, 1), substr(wells, 3, 3)),
    wells
  )
  return(wells)
}
