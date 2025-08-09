#' @title Visualize Distribution with Density Plot of HT estimates
#' @description Creates a combined density plot and boxplot visualization for the 
#' HT estimates of a continuous variable, with reference lines showing key 
#' statistics (total, Q1, and Q3). The y-axis shows scaled density (0-1),
#' and values are automatically scaled to billions (10^9) on the x-axis.
#'
#' @param data A data.table or data.frame containing the target variable.
#' @param target_var Character string specifying the target continuous variable.
#' @param true_total Numeric value representing the true population total for reference line.
#' @param title Plot title (default includes target variable name).
#' @param xlab X-axis label (default shows units in billions).
#' @param color_palette Named list of colors for plot elements (default: list(fill = "skyblue", line = "red")).
#' @param line_size Numeric value for reference line thickness (default: 1).
#'
#' @return A ggplot object showing the combined density-boxplot visualization with reference lines.
#'
#' @details
#' The visualization combines:
#' \itemize{
#'   \item Density plot (scaled to 0-1)
#'   \item Horizontal boxplot at base
#'   \item Solid red line for true total
#'   \item Dashed red lines for Q1 and Q3 of estimates
#'   \item Automatic scaling of x-axis to billions (10^9 units)
#' }
#'
#' @examples
#' \dontrun{
#' # Using Household Budget Survey data
#' plot_violin_box_HTestimate(
#'   data = estim_HT.dt,
#'   target_var = "exp_01",
#'   true_total = sum(data_HBS_grTruth.dt$exp_01)
#' )
#'
#' # Customized example
#' plot_violin_box_HTestimate(
#'   data = income_data,
#'   target_var = "annual_income",
#'   true_total = 1e8,
#'   title = "Annual Income Distribution",
#'   color_palette = list(fill = "lightgreen", line = "blue")
#' )
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_density geom_boxplot geom_vline scale_y_continuous labs theme_bw theme element_text
#' @importFrom stats quantile
#' @export
plot_violin_box_HTestimate <- function(
    data,
    target_var,
    true_total,
    title = paste0("Distribution of target variable (", target_var, ")"),
    subtitle = "",
    xlab = "",
    color_palette = list(fill = "skyblue", line = "red"), line_size = 1,
    scale_factor = 1) {
  
  # Input validation
  stopifnot(
    is.data.frame(data) || data.table::is.data.table(data),
    target_var %in% names(data),
    is.numeric(true_total),
    is.list(color_palette),
    line_size > 0
  )
  
  # Calculate reference quantiles
  HTestimates <- data[[target_var]]
  q1 <- quantile(HTestimates, probs = 0.25) / scale_factor
  q3 <- quantile(HTestimates, probs = 0.75) / scale_factor
  scaled_true_total <- true_total / scale_factor
  
  # Create plot
  p <- ggplot(data, aes(x = get(target_var)/scale_factor)) +
    geom_density(aes(y = after_stat(scaled)), fill = color_palette$fill, alpha = 0.7) +
    geom_boxplot(aes(y = -0.1), width = 0.03, fill = "white", outlier.shape = NA) +
    geom_vline(xintercept = scaled_true_total, color = color_palette$line, linewidth = line_size) +
    geom_vline(xintercept = q3, color = color_palette$line, linetype = "dashed", linewidth = line_size) +
    geom_vline(xintercept = q1, color = color_palette$line, linetype = "dashed", linewidth = line_size) +
    scale_y_continuous(
      name = "Density (scaled)\n",
      breaks = c(-0.1, seq(0, 1, 0.2)),
      labels = c("", seq(0, 1, 0.2))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = ""
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5)
    )
  
  return(p)
}