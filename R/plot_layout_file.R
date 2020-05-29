#' Function to plot layout files.
#'
#' This function recive one file which is the .csv layout file and generate a
#' plot that represents the designed layout of the microplate.
#'
#' @param file The path to a proper .csv layout file.
#' @param var_shape Assign shape to represent one variable
#' @param var_colour Assign colour to other variable
#' @param plate_title This argument is optional, add the title.
#'
#' @return Returns a plot that represents the microplate with the given layout.
#' The plot shows in the x-axis the number of columns and in the y-axis the
#' letter to identified the row.
#'
#'
#' @section \code{file} format:
#' Note that the .csv layout file needs to be formatted in the correct way. See
#' files in the examples below.
#'
#'
#' @importFrom rlang .data
#'
#' @export
#' @examples
#' file_path <- system.file("extdata", "test_spectraMax_layout_1.csv",
#'   package = "mpxtractor"
#' )
#'
#' # Data is store as a tibble
#' plot_plate <- plot_layout_file(
#'   file = file_path, var_shape = "basic", var_colour = "condition",
#'   plate_title = "My experiment"
#' )
#'
#' # Show the plot
#' plot_plate
#'
# Main function
plot_layout_file <- function(file, var_shape, var_colour, plate_title = NULL) {
  #defined <- rm(list = ls(pattern = "plate_title"))
  list_arg <- ls()
  defined <- list_arg[-2]
  passed <- names(as.list(match.call())[-1])
  if (any(!defined %in% passed)) {
    stop(paste("missing values for argument", paste(setdiff(defined, passed), collapse = ", ")))
  }

  NUMBER_FACTORS <- 6 # number of shapes available to use
  platemap_df <- platemap_for_ggplot(file)

   if (length(unique(platemap_df[[var_shape]])) > NUMBER_FACTORS) {
    stop("You must use attributes for var_shape with less than 7 different factors.")
  }

  generate_platemap(
    platemap_df,
    var_shape,
    var_colour,
    plate_title
  )
}

# Generate dataframe from layout file to make the plots.
#
# The layout file is read into a dataframe, after the attributes column and
# row are added. This is important to format the dimensions of the microplate.
#
# Parameters is the layout file with the correct format(check .csv files in extdata).
#
# The function return a dataframe.
platemap_for_ggplot <- function(file) {
  platemap_df <- read_layout_file(file, well_ids_column = "Wells")
  platemap_df <- dplyr::mutate(platemap_df,
    Row = as.numeric(match(
      toupper(substr(.data$Wells, 1, 1)),
      LETTERS
    )),
    Column = as.numeric(substr(.data$Wells, 2, 5))
  )
}

# Plot microplate with showing different conditions
#
# This function makes two plots, one is the plot that represents the background,
# which is the representation of the microplate and the second overlapping this
# are the conditions. For the first one the wells are empty circles, the conditions
# are plotted by shape and color.
#
#
#
# Parameters are a dataframe with the information of the layout file, var_shape
# which is the name of the variable represented by shape, var_color is the variable
# to be plotted by color. And as an option the plate_title
#
# The function return a plot
generate_platemap <- function(platemap_df, var_shape, var_colour, plate_title) {
  n_col <- length(unique(platemap_df$Column))
  n_row <- length(unique(platemap_df$Row))
  # Make the background plot
  ggplot2::ggplot(data = platemap_df, ggplot2::aes(x = .data$Column, y = .data$Row)) +
    ggplot2::geom_point(
      data = expand.grid(seq(1, n_col), seq(1, n_row)),
      ggplot2::aes(x = .data$Var1, y = .data$Var2),
      color = "grey39", fill = "white", shape = 21, size = 5
    ) +
    ggplot2::scale_y_reverse(breaks = seq(1, n_row), labels = LETTERS[1:n_row]) +
    ggplot2::scale_x_continuous(breaks = seq(1, n_col), position = "top") +
    ggplot2::labs(title = plate_title) +
    # Plot the variables over the background plot
    ggplot2::geom_point(ggplot2::aes_string(
      shape = var_shape, colour = var_colour,
    ), size = 3) +
    ggplot2::theme(
      legend.box = "vertical",
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = ggplot2::unit(0.001, "cm"),
      legend.key.width = ggplot2::unit(0.7, "cm"),
      plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm"),
      text = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        size = 1
      )
    ) +
    ggplot2::scale_colour_viridis_d()
}
