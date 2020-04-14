#' Function to plot growth rates from spectramax data.
#'
#' This function recive two files which are the layout file .csv and the spectramax
#' data file .txt.
#' plot that represents the growth rates into the microplate frame.
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
#' package = "mpxtractor")
#'
#' # Data is store as a tibble
#' plot_plate <- plot_layout_file(
#'    file = file_path, var_shape = "basic", var_colour = "condition",
#'    name_plate_layout = "My experiment" )
#'
#' # Show the plot
#' plot_plate


# Main function
microplate_spectramax_gr_plot <- function(spectramax_data,
                            layout_file,
                            time = NULL,
                            exp_title = NULL,
                            windowsize,
                            var_to_col) {

  sp_data_layout <- combine_data(spectramax_data, layout_file)
  sp_data_layout <- format_time(sp_data_layout, time)
  sp_data_layout <- background_correction_impute(sp_data_layout)
  sp_data_layout <- calculate_growth_rate(sp_data_layout, windowsize)
  df_sub_plots_well <- generate_subplots_by_well(sp_data_layout)
  df_sub_plots_well <- subplots_with_coordinates(df_sub_plots_well)
  df_sub_plots_well <- subplots_annotated(df_sub_plots_well)
  color_con <- color_by_condition(sp_data_layout, var_to_col)
  microplate_with_plots <- plot_gr_in_plate(df_sub_plots_well)

  save_gr_microplate(microplate_with_plots)
}

generate_subplots_by_well <- function(sp_data_layout) {
  sp_data_layout$condition_fc <- factor(sp_data_layout$condition,
    levels = unique(sp_data_layout$condition)
  )
  # generte color scale
  colors_fact <- rev(viridis(length(unique(sp_data_layout$condition))))
  names(colors_fact) <- levels(sp_data_layout$condition_fc)
  colScale <- scale_colour_manual(name = "condition_fc", values = colors_fact)

  # generate subplots
  sp_data_layout <- dplyr::group_by(sp_data_layout, Wells)
  df_sub_plots_well <- do(
    subplots = ggplot2::ggplot(sp_data_layout, aes(x = Time, y = mu)) +
      ggplot2::geom_line(aes(colour = condition_fc), size = 1) +
      colScale +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "none",
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
      )
  )
}


cut_into_coordinates <- function(variable, ngroups) {
  seq_all <- seq(min(variable) - 0.5, max(variable) + 0.5, by = 1)
  cut(variable,
    breaks = seq_all,
    labels = paste(seq_all[-(ngroups + 1)], seq_all[-1], sep = ","),
    include.lowest = TRUE
  )
}

subplots_with_coordinates <- function(df_sub_plots_well) {
  # Add column and row to the table sub_subplots
  df_sub_plots_well <- dplyr::mutate(df_sub_plots_well,
    Row = as.numeric(match(
      toupper(substr(Wells, 1, 1)),
      LETTERS
    )),
    Column = as.numeric(substr(Wells, 2, 5))
  )

  df_sub_plots_well$Row <- rev(df_sub_plots_well$Row)

  # Function cut_into_coordinates() to generate the coordinates x-y for each subplot.
  df_sub_plots_well$group_x <- cut_into_coordinates(
    df_sub_plots_well$Column,
    length(unique(platemap$Column))
  )
  df_sub_plots_well$group_y <- cut_into_coordinates(
    df_sub_plots_well$Row,
    length(unique(platemap$Row))
  )

  df_sub_plots_well <- tidyr::separate(df_sub_plots_well, group_x,
    into = c("min_x", "max_x"),
    sep = ",", convert = TRUE
  )

  df_sub_plots_well <- tidyr::separate(df_sub_plots_well, group_y,
    into = c("min_y", "max_y"),
    sep = ",", convert = TRUE
  )
}


# Function grobfun() generate the graphical objects with the coordinates x-y to plot
#in the background plot.
grobfun <- function(min_x, max_x, min_y, max_y, subplots) {
  annotation_custom(ggplotGrob(subplots),
    xmin = min_x, ymin = min_y,
    xmax = max_x, ymax = max_y
  )
}


subplots_annotated <- function(df_sub_plots_well) {
  df_sub_plots_well <- dplyr::select(
    df_sub_plots_well,
    min_x, max_x, min_y,
    max_y, subplots
  )
  df_sub_plots_well <- dplyr::mutate(df_sub_plots_well, grobs = pmap(., grobfun))


  df_sub_plots_well <- dplyr::inner_join(df_sub_plots_well,
    unique(df_sub_plots_well[, c("Wells", "condition_fc")]),
    by = "Wells"
  )
}


color_by_condition <- function(sp_data_layout, var_to_col) {
  colors_fact <- rev(viridis(length(unique(sp_data_layout$var_to_col))))
  color_con <- stack(colors_fact)
}



plot_gr_in_plate <- function(df_sub_plots_well, color_con) {
  n_col <- length(unique(df_sub_plots_well$Column))
  n_row <- length(unique(df_sub_plots_well$Row))

  background_plot <- ggplot2::ggplot(
    data = df_sub_plots_well,
    aes(
      x = Column,
      y = Row,
      fill = condition_fc
    )
  ) +
    ggplot2::geom_blank() +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = element_rect("transparent")) +
    ggplot2::scale_x_continuous(breaks = seq(1, n_col), position = "top") +
    ggplot2::scale_y_continuous(
      breaks = seq(1, n_row),
      labels = LETTERS[n_row:1],
      position = "left"
    ) +
    ggplot2::geom_col(aes(Inf, Inf)) +
    ggplot2::scale_fill_manual(name = var_col, values = color_con$values) +
    ggplot2::labs(title = exp_title)

  allwells <- background_plot + allgrobs$grobs
}


save_gr_microplate <- function(allwells, path = NULL) {
  ggsave(
    path = path,
    plot = allwells,
    width = 30,
    height = 20,
    units = "cm"
  )
}

