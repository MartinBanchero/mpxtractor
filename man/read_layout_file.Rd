% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_layout_file.R
\name{read_layout_file}
\alias{read_layout_file}
\title{Function specific to read layout files (.csv).}
\usage{
read_layout_file(file, well_ids_column = "Wells")
}
\arguments{
\item{file}{The path to a proper .csv file.}

\item{well_ids_column}{add the name to column well}
}
\value{
Returns a tibble data frame with different attributes The first column
is "Wells" this contain the names for each well (A01, A02..). The rest of the
attributes represent the different conditions.
}
\description{
This function recive layout file properly formatted. This function is similar
to the function \code{read_plate()} from \cite{plater} package.
}
\section{\code{file} format}{

 The format that \code{read_layout_file()} receive is a .csv file previously
 formatted in a proper way. This mean that the separation between conditions is
 one empty line.
}

\examples{
file_path <- system.file(
 "extdata",
 "test_layout_file.csv",
  package = "mpxtractor"
)

# Data is store as a tibble
data <- read_layout_file(
  file = file_path
)

# Now data is tidy
head(data)




}
\references{
\insertRef{plater}{mpxtractor}
}
