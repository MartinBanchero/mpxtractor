check_that_file_is_non_empty <- function(file) 
{
  if (length(readLines(file)) == 0) {
    stop(paste0("Sorry, '", file, "' is empty and must not be."), 
         call. = FALSE)
  }
}