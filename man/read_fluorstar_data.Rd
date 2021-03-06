% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_fluorstar_data.R
\name{read_fluorstar_data}
\alias{read_fluorstar_data}
\title{Function specific to read output files (.txt) from fluorStar readers machines.}
\usage{
read_fluorstar_data(file)
}
\arguments{
\item{file}{The path to a proper .txt file formatted by the fluorStar machine.}
}
\value{
Returns a tibble data frame with four columns. The first column is
"Wells" this containe the names for each well (A01, A02..). The second column
represent "Sample" which identified the wells, this part of the standard output
of fluorStar machines.
The third column is "Time", that represents the timestep at which the machine
measures. The fourth column contained the measured values. Depending on the experiment,
this can be fluorescence, absorbance between others.
}
\description{
This function receive one output file from fluorStar microplate reader and
generate a tibble dataframe.
}
\examples{
file_path <- system.file("extdata", "test_fluorstar_fluorescence_data.txt",
  package = "mpxtractor"
)

# Data is store as a tibble
data <- read_fluorstar_data(
  file = file_path
)

# Now data is tidy
head(data)

}
