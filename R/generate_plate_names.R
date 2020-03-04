generate_plate_names <- function(files){
  files <- regmatches(files, regexpr("[^/\\\\]*.[Tt][Xx][Tt]$", files))
  gsub(".[Tt][Xx][Tt]$", "", files)
}