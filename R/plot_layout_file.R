#' Function to plot layout files.
#'
#' This function recive one file which is the .csv layout file and generate a
#' plot that represents the designed layout of the microplate.
#'
#' @param file The path to a proper .csv layout file.
#' @param var_shape Assign shape to represent one variable
#' @param var_colour Assign colour to other variable
#' @param name_plate_layout This argument is optional, add the title.
#'
#' @return Returns a plot that represents the microplate with the given layout.
#' The plot shows in the x-axis the number of columns and in the y-axis the
#' letter to identified the row.
#'
#'
#' @section \code{file} format:
#' Note that the .csv layout file needs to be formatted in the correct way. See
#' examples below.
#'
#'
#' @export
#' @examples
#' file_path <- system.file("extdata", "spectraMax_layout_plate1.csv",
#'   package = "mpxtractor"
#' )
#'
#' # Data is store as a tibble
#' plot_plate <- plot_layout_file(
#'   file = file_path, var_shape = "basic", var_colour = "condition",
#'   name_plate_layout = "My experiment"
#' )
#'
#' # Show the plot
#' plot_plate
#'
#' # Main function
plot_layout_file <- function(file, var_shape,
                            var_colour,
                            name_plate_layout = NULL) {
  platemap_df <- platemap_for_ggplot(file)
  generate_platemap(
    platemap_df,
    var_shape,
    var_colour,
    name_plate_layout
  )
}

platemap_for_ggplot <- function(file) {
  platemap_df <- read_layout_file(file, well_ids_column = "Wells")
  platemap_df <- dplyr::mutate(platemap_df,
    Row = as.numeric(match(
      toupper(substr(Wells, 1, 1)),
      LETTERS
    )),
    Column = as.numeric(substr(Wells, 2, 5))
  )
}


generate_platemap <- function(platemap_df,
                              var_shape,
                              var_colour,
                              name_plate_layout) {
  n_col <- length(unique(platemap_df$Column))
  n_row <- length(unique(platemap_df$Row))

  ggplot2::ggplot(data = platemap_df, ggplot2::aes(x = Column, y = Row)) +
    ggplot2::geom_point(
      data = expand.grid(seq(1, n_col), seq(1, n_row)),
      ggplot2::aes(x = Var1, y = Var2),
      color = "grey39", fill = "white", shape = 21, size = 5
    ) +
    ggplot2::scale_y_reverse(breaks = seq(1, n_row), labels = LETTERS[1:n_row]) +
    ggplot2::scale_x_continuous(breaks = seq(1, n_col), position = "top") +
    ggplot2::labs(title = name_plate_layout) +
    ggplot2::geom_point(ggplot2::aes_string(
      shape = var_shape, colour = var_colour,
    ), size = 3) +
    ggplot2::theme(
      legend.box = "horizontal",
      plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm"),
      text = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        size = 1
      )
    ) +
    ggplot2::scale_colour_viridis_d()
}
