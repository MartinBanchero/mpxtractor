check_that_only_one_file_is_provided <- function(file){
  if (length(file) > 1) {
    stop(paste0("Sorry, only one file should be provided, but you provided ", 
                "multiple. Maybe you wanted read_plates()?"), call. = FALSE)
  }
  
}