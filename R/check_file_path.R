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