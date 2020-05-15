#'
#' Measurements of od600 for a 16x24 micro plate produced by spectraMax reader.
#' The dataset contain the Measurement and other attributes for three different
#' timepoints. The dataset was group by wells and arranged by time. This in
#' order to provide an easy way to use time series. Both datasets
#' df_spectramax_outdata_1 and df_spectramax_outdata_2 have the same structure.
#'
#'
#'
#' @format A data frame with 1152 rows and 4 variables:
#' \describe{
#'   \item{Wells}{Well id}
#'   \item{Time}{Time step, in hh:mm:ss format}
#'   \item{Temperature}{Temparture, measured in °C }
#'   \item{Measurement}{OD600, optical density at 600 nm}
#' }
#' @source \url{http://www.teusinkbruggemanlab.nl/}
"df_spectramax_outdata_2"
