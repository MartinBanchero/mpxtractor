# mpxtractor <img src="man/logo_mpxtractor.png" width = 200, align="right">



The main function of `mpxtractor` is to provide a simple method that is able to process raw data 
from micro plate readers like SpectraMax, FluorStar and MultiscanGO and apply different methods to it.
This package generate tidy data frames, also combine this data with layout files and plot growth rates
over a microplate structre. This is a critical point in several research lines performing experiments 
using micro plate reader machines because of the time that takes to clean and sort the raw data into a
tidy format. Also, the features of this package allow to detect which wells present faulty data. 
This package main contribution is orientated to save time and get fast into data analysis.


## Installation 

```R
# The development version from GitHub:
# install.packages("mpxtractor")
devtools::install_github("MartinBanchero/mpxtractor")
```

## Usage

```R
```

## Learn more

To get started, first read `vignette("mpxtractor")`. Then read more about the specific package component that you want to apply.

* For data wrangling and layout files, read `vignette("wrangling_and_layout_functions")`.
* For plotting featurs, read `vignette("plotting_functions")`
