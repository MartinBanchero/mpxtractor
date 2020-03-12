# Check that one file is provided.
#
# @return Error if more than one file is given.
# @seealso \code{\link{plater}}
check_that_only_one_file_is_provided <- function(file){
  if (length(file) > 1) {
    stop(paste0("Sorry, only one file should be provided, but you provided ",
                "multiple. Maybe you wanted read_plates()?"), call. = FALSE)
  }

}

# Check the file path.
#
# @seealso \code{\link{plater}}
# @return Error if the file is null or if the file does not exist. Then check if
# the extension is .txt
#
check_file_path <- function(file){
  if (is.null(file) || !file.exists(file)) {
    stop(paste0("Sorry, can't find your file '", file, "'."),
         call. = FALSE)
  }
  if (!(grepl("[Tt][Xx][Tt]$", file))) {
    stop(paste0("Sorry, '", file, "' doesn't have a proper txt file extension."),
         call. = FALSE)
  }
}

# Check that file is not empty
#
# @seealso \code{\link{plater}}
# @return Error if the file is empty.
#
check_that_file_is_non_empty <- function(file)
{
  if (length(readLines(file)) == 0) {
    stop(paste0("Sorry, '", file, "' is empty and must not be."),
         call. = FALSE)
  }
}


