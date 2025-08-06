#' Distribution Visualization with Half-Eye, Boxplots, and Points
#'
#' Creates a combined plot showing:
#' - Half-eye distribution (density + interval)
#' - Traditional boxplot
#' - Jittered points for raw data
#' - Reference line at y=0
#' - Faceting support
#'
#' @param data A `data.frame` or `data.table` containing the data to plot.
#' @param x Character string specifying the x-axis variable.
#' @param y Character string specifying the y-axis variable.
#' @param xlab Label for x-axis (default: "Categories").
#' @param ylab Label for y-axis (default: relative difference math expression).
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param facet_formula Formula for faceting (e.g., `~group`). Optional.
#'
#' @return A `ggplot` object that can be further customized.
#'
#' @details
#' This visualization combines multiple statistical representations:
#' \itemize{
#'   \item **Half-eye plot**: Density + credibility interval (using ggdist)
#'   \item **Boxplot**: Showing medians and quartiles
#'   \item **Points**: With jitter to avoid overplotting (using gghalves)
#' }
#' 
#' The horizontal reference line at y=0 provides a visual baseline.
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
#' plot_raincloud_relative_diff_popFraction(
#'   data = dt,
#'   x = "geo1",
#'   y = "diff_rel",
#'   title = "Distribution by category")
#'
#' # With faceting
#' plot_raincloud_relative_diff_popFraction(
#'   data = dt,
#'   x = "geo1",
#'   y = "diff_rel",
#'   facet_formula = ~region)
#' }
#'
#' @seealso
#' Key packages used internally:
#' - \code{\link[ggdist]{stat_halfeye}}
#' - \code{\link[gghalves]{geom_half_point}}
#' - \code{\link[ggplot2]{ggplot}}
#'
#' @importFrom ggplot2 ggplot aes geom_hline labs theme_bw theme element_text facet_grid
#' @importFrom ggdist stat_halfeye
#' @importFrom gghalves geom_half_point
#' @export
plot_raincloud_relative_diff_popFraction <- function(
    data, x, y, xlab, ylab, title, subtitle, facet_formula = NULL){

    stopifnot(
      is.data.frame(data),
      x %in% names(data),
      y %in% names(data)
    )
    
    p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
      geom_hline(yintercept = 0, color = "red") +
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.2, 
        point_colour = NA
      ) + 
      geom_boxplot(width = .15, outlier.shape = NA) +
      gghalves::geom_half_point(
        side = "l", 
        range_scale = .4, 
        alpha = .2
      ) +
      labs(
        x = xlab,
        y = ylab,
        title = title,
        subtitle = subtitle
      ) +
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
