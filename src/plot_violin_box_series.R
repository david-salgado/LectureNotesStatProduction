#' @title Violin-Boxplot Series Visualization for Target vs Frame Variables
#' @description Creates a combined violin and boxplot visualization for exploring the relationship between a categorical/discrete frame variable (x-axis) and a continuous variable (y-axis). Includes automatic viridis color scaling.
#'
#' @param data A data.table or data.frame containing the variables to plot.
#' @param x_var Character string specifying the categorical/discrete variable (will be converted to factor).
#' @param y_var Character string specifying the continuous target variable.
#' @param title Plot title (default includes variable names).
#' @param xlab X-axis label (defaults to x_var name).
#' @param ylab Y-axis label (defaults to y_var name).
#' @param color_palette Viridis color palette option (default: "plasma").
#' @param alpha Transparency level for violins (default: 0.8).
#' @param boxplot_width Width of boxplots (default: 0.1).
#'
#' @return A ggplot object showing the combined violin-boxplot visualization.
#'
#' @details
#' The function creates a visualization that combines:
#' \itemize{
#'   \item Violin plots showing the density distribution
#'   \item Boxplots showing quartiles (without outliers)
#'   \item Viridis color scale for better visual perception
#' }
#'
#' @examples
#' \dontrun{
#' # Using the Household Budget Survey data example
#' plot_violin_box_series(
#'   data = data_HBS_grTruth.dt,
#'   x_var = "hhsize",
#'   y_var = "exp_01"
#' )
#'
#' # Customized example
#' plot_violin_box_series(
#'   data = mtcars,
#'   x_var = "cyl",
#'   y_var = "mpg",
#'   title = "MPG Distribution by Cylinders",
#'   color_palette = "magma"
#' )
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_violin geom_boxplot labs theme_bw theme element_text
#' @importFrom viridis scale_fill_viridis_d
#' @export
plot_violin_box_series <- function(
    data,
    x_var,
    y_var,
    title = paste0("Target variable (", y_var, ") vs. Frame variable (", x_var, ")"),
    xlab = x_var,
    ylab = y_var,
    color_palette = "plasma",
    alpha = 0.8,
    boxplot_width = 0.1) {
  
  # Input validation
  stopifnot(
    is.data.frame(data) || data.table::is.data.table(data),
    x_var %in% names(data),
    y_var %in% names(data),
    is.character(title),
    is.character(color_palette),
    alpha > 0 && alpha <= 1,
    boxplot_width > 0
  )
  
  # Create plot
  p <- ggplot(data, aes(x = as.factor(get(x_var)), y = get(y_var), fill = as.factor(get(x_var)))) +
    geom_violin(alpha = alpha, trim = FALSE) +
    geom_boxplot(width = boxplot_width, fill = "white", outlier.shape = NA) +
    scale_fill_viridis_d(option = color_palette) +
    labs(
      title = title,
      x = paste("\n", xlab),
      y = paste(ylab, "\n"),
      fill = x_var
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.position = "none"
    )
  
  return(p)
}