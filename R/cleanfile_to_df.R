cleanfile_to_df <- function(cleanfile) {
  df <- read.table(textConnection(cleanfile), 
                   header = TRUE,
                   sep = '\t',
                   stringsAsFactors = FALSE, 
                   colClasses = 'character', 
                   comment.char = "")
  return(df)
}