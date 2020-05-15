# mpxtractor <img src="man/figures/logo_mpxtractor.png" width = 200, align="right">

<p align="justified">
The main function of `mpxtractor` is to provide a simple method that is able to process raw data 
from micro plate readers like SpectraMax, FluorStar and MultiscanGO and apply different
methods to it. 
This package generate tidy data frames, this data can be combined with layout files and then growth rates
are calculated and plot over a microplate frame. This is a critical point in several research lines performing
experiments using micro plate reader machines mainly due to the time that takes to clean and sort the raw data into a
tidy format. Also, the features of this package allow to detect which wells present faulty data e.g wells
not filled correctly. Other feature of mpxtractor is that allow to plot the layout file over a microplate frame, this is very useful because is easy to visualize the layout, this is handy in the lab to set the experiment.
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
* For auxiliary functions read `vignette("Auxiliary_functions")`
