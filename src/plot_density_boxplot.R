#' Distribution Visualization with density plot and boxplot
#'
#' Creates a combined plot showing:
#' - Traditional boxplot
#' - Density plot
#' - Faceting support
#'
#' @param data A `data.frame` or `data.table` containing the data to plot.
#' @param x Character string specifying the x-axis variable.
#' @param xlab Label for x-axis (default: "Categories").
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param facet_formula Formula for faceting (e.g., `~group`). Optional.
#'
#' @return A `ggplot` object that can be further customized.
#'
#' @details
#' This visualization combines multiple statistical representations:
#' \itemize{
#'   \item **Boxplot**: Showing medians and quartiles
#'   \item **Density plot**: Showing the distribution density
#' }
#' 
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' 
#' # Sample data
#' dt <- data.table(
#'   geo1 = factor(rep(LETTERS[1:3], each = 100)),
#'   diff_rel = rnorm(300),
#'   region = sample(c("North", "South"), 300, replace = TRUE)
#' )
#'
#' # Basic usage
#' plot_density_boxplot(
#'   data = dt,
#'   x = "geo1",
#'   title = "Distribution by category")
#'
#' # With faceting
#' plot_density_boxplot(
#'   data = dt,
#'   x = "geo1",
#'   facet_formula = ~region)
#' }
#'
#'
#' @importFrom ggplot2 ggplot aes geom_hline labs theme_bw theme element_text facet_grid
#' @export
plot_density_boxplot <- function(
    data, x, xlab, title, subtitle, facet_formula = NULL){
  
  stopifnot(
    is.data.frame(data),
    x %in% names(data)
  )
  
  p <- ggplot(data, aes(x = .data[[x]])) +
    geom_density(aes(y = after_stat(scaled)), fill = "skyblue") +  # 0-1 Scale
    geom_boxplot(aes(y = -0.1), width = 0.03) +
    scale_y_continuous(
      name = "Density (scaled)\n",
      breaks = c(-0.1, seq(0, 1, 0.2)),
      labels = c("", seq(0, 1, 0.2))
    ) +
    labs(title = title, subtitle = subtitle,
         x = xlab, y = "") +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5)
    )
  
  if (!is.null(facet_formula)) {
    p <- p + facet_grid(facet_formula, scales = "free")
  }
  
  return(p)
}
