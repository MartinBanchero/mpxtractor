get_input_read_multifiles <- function(folder = NULL, pattern = NULL, filebyname = NULL)
{
  if (is.null(folder) && !is.null(pattern) && is.null(filebyname))  {
    stop(paste0("Sorry, dirFiles must to be given."))
  }
  if (!is.null(folder) &&  !is.null(pattern) && !is.null(filebyname) ) {
    stop(paste0("Sorry, files cannnot be used with dirFiles or pattern"))
  }
  if (!is.null(folder)) {
    lst_files <- do.call(get_files, list(folder, pattern))
    return(lst_files)
  }
  if (!is.null(filebyname)) {
    lst_files <- filebyname
    return(lst_files)
  }
}

get_files <- function(folder, pattern = NULL)
{
  if (is.null(pattern)) {
    lst_files <- list.files(path = folder,
                            recursive = TRUE,
                            full.names = TRUE)
    return(lst_files)
  }
  lst_files <- list.files(path = folder,
                          pattern = pattern,
                          recursive = TRUE,
                          full.names = TRUE)
  return(lst_files)
}
