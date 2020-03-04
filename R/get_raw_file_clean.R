get_raw_file_clean <- function(file){
  raw_file <- readLines(file, warn = FALSE, encoding = "latin1")
  raw_file_clean <- grep(raw_file[which(raw_file != "")],
                         pattern = ".*\\t\t\t.*",
                         invert = TRUE,
                         value = TRUE,
                         useBytes = T )
  
  cleanfile <- trimws(raw_file_clean, which = "right", whitespace = "[\t]")
}