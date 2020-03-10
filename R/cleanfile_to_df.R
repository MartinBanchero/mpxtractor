#'Make data frame using a clean file.
#'
#' \code{cleanfile_to_df} takes a clean file which is a string type and using
#' read.table generates a dataframe.
#' @param cleanfile A string
#' @return Dataframe
#' @example
#' cleanfile_to_df(cleanfile)
#' @seealso \code{\link[utils]{read.table}}

cleanfile_to_df <- function(cleanfile) {
  df <- utils::read.table(textConnection(cleanfile),
                   header = TRUE,
                   sep = '\t',
                   stringsAsFactors = FALSE,
                   colClasses = 'character',
                   comment.char = "")
  return(df)
}
