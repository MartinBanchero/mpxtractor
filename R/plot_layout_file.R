#' Function plot the layout file given in a correct format.
#'
#' This function recive one file which is the .csv layout file and generate a
#' a plot that represents the designed layout of the microplate.
#'
#' @param file The path to a proper .csv layout file.
#'
#' @return Returns a plot that represents the microplate with the given layout.The
#' plot shows in the x-axis the number of columns and in the y-axis the letter for
#' identified with the row. The variables are distinguish by shape and by colour.
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
#' package = "mpxtractor")
#'
#' # Data is store as a tibble
#' plot_plate <- plot_layout_file(
#'    file = file_path)
#'
#' # Show the plot
#' plot_plate

# Main function
plot_lyout_file <- function(file, var_shape, var_colour, name_plate_layout ) {
 platemap_df <- platemap_for_ggplot(file)
 generate_platemap(platemap_df, var_shape, var_colour, name_plate_layout )
}




platemap_for_ggplot <- function(file) {
  platemap_df <- read_plate_file(file, well_ids_column = "Wells")
  platemap_df <- dplyr::mutate(platemap_df,
                            Row = as.numeric(match(toupper(substr(Wells, 1, 1)),
                                                   LETTERS)),
                            Column = as.numeric(substr(Wells, 2, 5)))
}


generate_platemap <- function(platemap_df, var_shape, var_colour, name_plate_layout){
 n_col <- length(unique(platemap_df$Column))
 n_row <- length(unique(platemap_df$Row))


 ggplot2::ggplot(data = platemap_df, aes(x = Column, y = Row)) +
 ggplot2::geom_point(data = expand.grid(seq(1, n_col ),
                                seq(1, n_row)),
             aes(x = Var1, y = Var2),
             color = "grey39", fill = "white", shape = 21, size = 5) +

 ggplot2::scale_y_reverse(breaks = seq(1, n_row), labels = LETTERS[1:n_row]) +

 ggplot2::scale_x_continuous(breaks = seq(1, n_col), position = "top") +
 ggplot2::labs(title = name_plate_layout) +

 ggplot2::geom_point(aes_string(shape = var_shape, colour = var_colour), size = 3) +

 ggplot2::theme(legend.box = "horizontal",
        plot.margin = unit(c(1,1,1,1), "cm"),
        text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
 ggplot2::scale_colour_viridis_d()
}
