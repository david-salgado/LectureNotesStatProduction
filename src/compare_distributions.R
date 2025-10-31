library(data.table)
library(ggplot2)

#' Compare Distributions Between dt1 and dt2 Data
#'
#' This function compares the distributions of a variable between dt1 and dt2 datasets
#' using appropriate statistical tests and visualizations based on variable type.
#'
#' @param dt1 A data.table containing the dt1 data
#' @param dt2 A data.table containing the dt2 data  
#' @param variable Character string specifying the variable name to compare
#'
#' @return A list containing appropriate statistics and visualizations based on variable type
#'
#' @examples
#' \dontrun{
#' # Para variables numéricas
#' set.seed(123)
#' dt1_num <- data.table(income = rnorm(800, 48000, 18000))
#' dt2_num <- data.table(income = rnorm(1000, 50000, 15000))
#' results_num <- compare_distributions(dt1_num, dt2_num, "income")
#'
#' # Para variables categóricas
#' dt1_cat <- data.table(category = sample(c("A", "B", "C"), 800, replace = TRUE))
#' dt2_cat <- data.table(category = sample(c("A", "B", "C"), 1000, replace = TRUE, 
#'                        prob = c(0.3, 0.4, 0.3)))
#' results_cat <- compare_distributions(dt1_cat, dt2_cat, "category")
#' }
#'
#' @import data.table
#' @import ggplot2
#' @importFrom stats chisq.test fisher.test shapiro.test t.test wilcox.test ks.test
#' @export
compare_distributions <- function(dt1, dt2, variable) {
  
  # Input validation
  if (!is.data.table(dt1)) stop("dt1 must be a data.table")
  if (!is.data.table(dt2)) stop("dt2 must be a data.table")
  if (!variable %in% names(dt1)) stop("Variable ", variable, " not found in dt1")
  if (!variable %in% names(dt2)) stop("Variable ", variable, " not found in dt2")
  
  # Determine variable type
  var_type <- class(dt1[[variable]])[1]
  if (var_type == "factor") var_type <- "character"
  
  # Extract vectors
  values_dt1 <- dt1[[variable]]
  values_dt2 <- dt2[[variable]]
  
  # Remove NA values with warning
  na_count_dt1 <- sum(is.na(values_dt1))
  na_count_dt2 <- sum(is.na(values_dt2))
  
  if (na_count_dt1 > 0) {
    
    warning("Removing ", na_count_dt1, " NA values from dt1")
    values_dt1 <- values_dt1[!is.na(values_dt1)]
    
  }
  if (na_count_dt2 > 0) {
    
    warning("Removing ", na_count_dt2, " NA values from dt2")
    values_dt2 <- values_dt2[!is.na(values_dt2)]
    
  }
  
  # Route to appropriate function based on variable type
  if (var_type %in% c("numeric", "integer")) {
    
    return(compare_numeric_distributions(values_dt1, values_dt2, variable))
    
  } else if (var_type %in% c("character", "factor")) {
    
    return(compare_categorical_distributions(values_dt1, values_dt2, variable))
    
  } else {
    
    stop("Unsupported variable type: ", var_type)
    
  }
}

# Function for numeric variables
compare_numeric_distributions <- function(values_dt1, values_dt2, variable) {
  
  # Create combined data for visualization
  combined_data <- rbind(
    data.table(value = values_dt1, group = "dt1"),
    data.table(value = values_dt2, group = "dt2")
  )
  setnames(combined_data, "value", variable)
  
  # Calculate descriptive statistics
  stats_dt1 <- data.table(
    dataset = "dt1",
    n = length(values_dt1),
    min = min(values_dt1),
    q10 = quantile(values_dt1, 0.10),
    q25 = quantile(values_dt1, 0.25),
    mean = mean(values_dt1),
    median = median(values_dt1),
    q75 = quantile(values_dt1, 0.75),
    q90 = quantile(values_dt1, 0.90),
    max = max(values_dt1),
    sd = sd(values_dt1),
    IQR = IQR(values_dt1)
  )
  
  stats_dt2 <- data.table(
    dataset = "dt2",
    n = length(values_dt2),
    min = min(values_dt2),
    q10 = quantile(values_dt2, 0.10),
    q25 = quantile(values_dt2, 0.25),
    mean = mean(values_dt2),
    median = median(values_dt2),
    q75 = quantile(values_dt2, 0.75),
    q90 = quantile(values_dt2, 0.90),
    max = max(values_dt2),
    sd = sd(values_dt2),
    IQR = IQR(values_dt2)
  )
  
  combined_stats <- rbind(stats_dt1, stats_dt2)
  
  # Normality tests
  #normality_dt1 <- shapiro.test(values_dt1)
  #normality_dt2 <- shapiro.test(values_dt2)
  
  # Select appropriate statistical test
  #if (normality_dt1$p.value > 0.05 && normality_dt2$p.value > 0.05) {
    
  #  comparison_test <- t.test(values_dt1, values_dt2)
  #  test_type <- "t-test"
    
  #} else {
    
  #  comparison_test <- wilcox.test(values_dt1, values_dt2)
  #  test_type <- "Wilcoxon rank-sum test"
  #}
  
  # Distance metrics calculations
  # Wasserstein distance
  wasserstein_dist <- transport::wasserstein1d(values_dt1, values_dt2)
  
  # Kolmogorov-Smirnov distance and test
  ks_test <- ks.test(values_dt1, values_dt2)
  ks_distance <- ks_test$statistic
  
  # Hellinger distance
  hellinger_dist <- calculate_hellinger_distance(values_dt1, values_dt2)
  
  # Jensen-Shannon divergence
  js_divergence <- calculate_js_divergence(values_dt1, values_dt2)
  
  
  # Create visualizations
  ecdf_plot <- create_ecdf_plot(combined_data, variable)
  density_plot <- create_density_plot(combined_data, variable)
  raincloud_plot <- create_raincloud_plot(combined_data, variable)
  
  return(list(
    variable_type = "numeric",
    statistics = combined_stats,
    #normality_tests = list(dt1 = normality_dt1, dt2 = normality_dt2),
    #comparison_test = comparison_test,
    #test_type = test_type,
    ks_test = ks_test,
    distance_metrics = list(
      hellinger_distance = hellinger_dist,
      js_divergence = js_divergence,
      ks_distance = ks_distance,
      wasserstein_distance = wasserstein_dist
    ),
    density_plot = density_plot,
    ecdf_plot = ecdf_plot,
    raincloud_plot = raincloud_plot,
    combined_data = combined_data
  ))
}

# Function for categorical variables
compare_categorical_distributions <- function(values_dt1, values_dt2, variable) {
  
  # Convert to factor if character
  if (is.character(values_dt1)) {
    
    all_levels <- unique(c(values_dt1, values_dt2))
    values_dt1 <- factor(values_dt1, levels = all_levels)
    values_dt2 <- factor(values_dt2, levels = all_levels)
    
  }
  
  # Create frequency tables
  freq_dt1 <- as.data.table(table(values_dt1), keep.rownames = FALSE)
  freq_dt2 <- as.data.table(table(values_dt2), keep.rownames = FALSE)
  
  setnames(freq_dt1, c("category", "count_dt1"))
  setnames(freq_dt2, c("category", "count_dt2"))
  
  # Merge frequency tables
  freq_combined <- merge(freq_dt1, freq_dt2, by = "category", all = TRUE)
  freq_combined[is.na(count_dt1), count_dt1 := 0]
  freq_combined[is.na(count_dt2), count_dt2 := 0]
  
  # Calculate proportions
  freq_combined[, prop_dt1 := count_dt1 / sum(count_dt1)]
  freq_combined[, prop_dt2 := count_dt2 / sum(count_dt2)]
  freq_combined[, diff_prop := prop_dt2 - prop_dt1]
  
  # Create contingency table for statistical tests
  cont_table <- table(
    c(values_dt1, values_dt2),
    c(rep("dt1", length(values_dt1)), rep("dt2", length(values_dt2)))
  )

  # Perform chi-squared test (with warning suppression for small expected counts)
  chi_test <- tryCatch({
    
    chisq.test(cont_table)
    
  }, warning = function(w) {
    
    warning("Chi-squared test may be inaccurate due to small expected counts")
    chisq.test(cont_table, simulate.p.value = TRUE)
    
  })
  
  # Fisher's exact test for small samples or many categories
  fisher_test <- tryCatch({
    
    fisher.test(cont_table, simulate.p.value = TRUE)
    
  }, error = function(e) {
    
    NULL
    
  })
  
  # Create combined data for visualization
  combined_data <- rbind(
    data.table(value = values_dt1, group = "dt1"),
    data.table(value = values_dt2, group = "dt2")
  )
  setnames(combined_data, "value", variable)
  
  # Create visualizations
  proportion_plot <- create_proportion_plot(freq_combined, variable)
  
  # Distance metrics calculations
  # Hellinger distance
  hellinger_dist <- calculate_hellinger_distance(values_dt1, values_dt2)
  
  # Jensen-Shannon divergence
  js_divergence <- calculate_js_divergence(values_dt1, values_dt2)
  
  return(list(
    variable_type = "categorical",
    frequency_table = freq_combined,
    contingency_table = cont_table,
    chi_squared_test = chi_test,
    fisher_test = fisher_test,
    distance_metrics = list(
      hellinger_distance = hellinger_dist,
      js_divergence = js_divergence
    ),
    proportion_plot = proportion_plot,
    combined_data = combined_data
  ))
}

# Visualization functions
create_ecdf_plot <- function(combined_data, variable) {
  
  ggplot(combined_data, aes(x = .data[[variable]], color = group)) +
    stat_ecdf(size = 1, alpha = 0.7) +
    labs(
      title = paste("Empirical CDF -", variable),
      x = variable,
      y = "Cumulative Probability",
      color = "Dataset"
    ) +
    theme_bw() +
    scale_color_manual(values = c("dt1" = "#E41A1C", "dt2" = "#377EB8"))
}

create_density_plot <- function(combined_data, variable) {
  ggplot(combined_data, aes(x = .data[[variable]], fill = group)) +
    geom_density(alpha = 0.5) +
    labs(
      title = paste("Density Plot -", variable),
      x = variable,
      y = "Density",
      fill = "Dataset"
    ) +
    theme_bw() +
    scale_fill_manual(values = c("dt1" = "#E41A1C", "dt2" = "#377EB8"))
}

create_raincloud_plot <- function(combined_data, variable){
  
  ggplot(combined_data, aes(x = group, y = .data[[variable]], fill = group, color = group)) +
    # 1. Individual points (rain) - A LA IZQUIERDA
    geom_point(
      size = 1.5,
      alpha = 0.2,
      position = position_jitter(
        seed = 1,
        width = 0.1,
        height = 0
      )
    ) +
    # 2. Boxplot - EN EL CENTRO (ligeramente a la derecha de los puntos)
    geom_boxplot(
      width = 0.15,
      alpha = 0.6,
      outlier.shape = NA,
      position = position_nudge(x = 0.2)  # Desplazado a la derecha
    ) +
    # 3. Density half-eye (cloud) - A LA DERECHA
    ggdist::stat_halfeye(
      adjust = 0.5,
      width = 0.5,
      .width = 0,
      justification = -0.2,  # Para que empiece desde el centro hacia la izquierda
      point_colour = NA,
      alpha = 0.4,
      position = position_nudge(x = 0.2)  # Desplazado más a la derecha
    ) +
    labs(
      title = paste("Raincloud Plot -", variable),
      subtitle = "dt1 vs dt2 data",
      x = "Group",
      y = variable,
      fill = "Dataset",
      color = "Dataset"
    ) +
    theme_bw() +
    scale_fill_manual(values = c("dt1" = "#E41A1C", "dt2" = "#377EB8")) +
    scale_color_manual(values = c("dt1" = "#E41A1C", "dt2" = "#377EB8")) +
    theme(legend.position = "top", 
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}
  
# Visualization functions for categorical variables
create_proportion_plot <- function(freq_combined, variable) {
  plot_data <- melt(freq_combined, 
                    id.vars = "category",
                    measure.vars = c("prop_dt1", "prop_dt2"),
                    variable.name = "dataset",
                    value.name = "proportion")
  
  plot_data[, dataset := ifelse(dataset == "prop_dt1", "dt1", "dt2")]
  
  ggplot(plot_data, aes(x = category, y = proportion, fill = dataset)) +
    geom_col(position = "dodge", alpha = 0.8) +
    labs(
      title = paste("Proportion Plot -", variable),
      x = variable,
      y = "Proportion",
      fill = "Dataset"
    ) +
    theme_bw() +
    scale_fill_manual(values = c("dt1" = "#E41A1C", "dt2" = "#377EB8")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5),
          plot.title = element_text(hjust = 0.5, face = 'bold'))
}

# Helper function to calculate Hellinger distance
calculate_hellinger_distance <- function(x, y, nbins = 1000) {
  
  # Determinar el tipo de variable
  x_type <- class(x)[1]
  y_type <- class(y)[1]
  
  # Si ambos son character o factor, tratarlos como categóricos
  if ((x_type %in% c("character", "factor") && y_type %in% c("character", "factor"))) {
    
    return(calculate_hellinger_categorical(x, y))
    
  } 
  # Si ambos son numéricos, usar el método original
  else if (x_type %in% c("numeric", "integer") && y_type %in% c("numeric", "integer")) {
    
    return(calculate_hellinger_numeric(x, y, nbins))
  
    }
  # Tipos mixtos - error
  else {
    
    stop("Variables x e y deben ser del mismo tipo (ambas numéricas o ambas categóricas)")
    
  }
}

# Función para variables numéricas
calculate_hellinger_numeric <- function(x, y, nbins = 1000) {
  # Create common range for both distributions
  min_val <- min(min(x), min(y))
  max_val <- max(max(x), max(y))
  
  # Create density estimates over common grid
  common_grid <- seq(min_val, max_val, length.out = nbins)
  
  dens_x <- density(x, from = min_val, to = max_val, n = nbins)$y
  dens_y <- density(y, from = min_val, to = max_val, n = nbins)$y
  
  # Normalize densities
  dens_x <- dens_x / sum(dens_x)
  dens_y <- dens_y / sum(dens_y)
  
  # Calculate Hellinger distance
  sqrt_x <- sqrt(dens_x)
  sqrt_y <- sqrt(dens_y)
  
  hellinger <- sqrt(1 - sum(sqrt_x * sqrt_y))
  
  return(hellinger)
}

# Función para variables categóricas
# Versión con manejo más robusto de categorías raras
calculate_hellinger_categorical <- function(x, y, epsilon = 1e-10) {
  
  # Convertir a factor si son character
  if (is.character(x)) x <- factor(x)
  if (is.character(y)) y <- factor(y)
  
  # Obtener todos los niveles únicos
  all_levels <- unique(c(levels(x), levels(y)))
  
  # Asegurar mismos niveles
  x <- factor(x, levels = all_levels)
  y <- factor(y, levels = all_levels)
  
  # Calcular probabilidades con suavizado de Laplace para evitar ceros
  freq_x <- (table(x) + epsilon) / (length(x) + length(all_levels) * epsilon)
  freq_y <- (table(y) + epsilon) / (length(y) + length(all_levels) * epsilon)
  
  # Asegurar orden
  freq_x <- freq_x[all_levels]
  freq_y <- freq_y[all_levels]
  
  # Reemplazar NAs por valor pequeño
  freq_x[is.na(freq_x)] <- epsilon
  freq_y[is.na(freq_y)] <- epsilon
  
  # Calcular Hellinger distance
  sum_sqrt <- sum(sqrt(freq_x * freq_y))
  hellinger <- sqrt(1 - sum_sqrt)
  
  return(hellinger)
}


# Helper function to calculate Jensen-Shannon divergence
calculate_js_divergence <- function(x, y, nbins = 1000) {
  
  # Determinar el tipo de variable
  x_type <- class(x)[1]
  y_type <- class(y)[1]
  
  # Si ambos son character o factor, tratarlos como categóricos
  if ((x_type %in% c("character", "factor") && y_type %in% c("character", "factor"))) {
    return(calculate_js_divergence_categorical(x, y))
  } 
  # Si ambos son numéricos, usar el método original
  else if (x_type %in% c("numeric", "integer") && y_type %in% c("numeric", "integer")) {
    return(calculate_js_divergence_numeric(x, y, nbins))
  }
  # Tipos mixtos - error
  else {
    stop("Variables x e y deben ser del mismo tipo (ambas numéricas o ambas categóricas)")
  }
}

# Función para variables numéricas (método original)
calculate_js_divergence_numeric <- function(x, y, nbins = 1000) {
  # Create common range for both distributions
  min_val <- min(min(x), min(y))
  max_val <- max(max(x), max(y))
  
  # Create density estimates over common grid
  common_grid <- seq(min_val, max_val, length.out = nbins)
  
  dens_x <- density(x, from = min_val, to = max_val, n = nbins)$y
  dens_y <- density(y, from = min_val, to = max_val, n = nbins)$y
  
  # Normalize densities
  dens_x <- dens_x / sum(dens_x)
  dens_y <- dens_y / sum(dens_y)
  
  # Avoid zeros for KL divergence calculation
  eps <- 1e-10
  dens_x <- pmax(dens_x, eps)
  dens_y <- pmax(dens_y, eps)
  
  # Calculate average distribution
  m <- 0.5 * (dens_x + dens_y)
  
  # Calculate KL divergences
  kl_x_m <- sum(dens_x * log(dens_x / m))
  kl_y_m <- sum(dens_y * log(dens_y / m))
  
  # Jensen-Shannon divergence
  js_divergence <- 0.5 * kl_x_m + 0.5 * kl_y_m
  
  return(js_divergence)
}

# Función para variables categóricas
# Versión con suavizado adaptativo
calculate_js_divergence_categorical <- function(x, y, alpha = 1e-6) {
  
  if (is.character(x)) x <- factor(x)
  if (is.character(y)) y <- factor(y)
  
  all_levels <- unique(c(levels(x), levels(y)))
  x <- factor(x, levels = all_levels)
  y <- factor(y, levels = all_levels)
  
  n_levels <- length(all_levels)
  n_x <- length(x)
  n_y <- length(y)
  
  # Suavizado adaptativo basado en el tamaño de muestra
  epsilon_x <- alpha / n_x
  epsilon_y <- alpha / n_y
  
  # Calcular distribuciones suavizadas
  p <- (table(x) + epsilon_x) / (n_x + n_levels * epsilon_x)
  q <- (table(y) + epsilon_y) / (n_y + n_levels * epsilon_y)
  
  p <- p[all_levels]
  q <- q[all_levels]
  p[is.na(p)] <- epsilon_x
  q[is.na(q)] <- epsilon_y
  
  # Distribución promedio
  m <- 0.5 * (p + q)
  
  # JS divergence
  kl_p_m <- sum(p * log(p / m))
  kl_q_m <- sum(q * log(q / m))
  js_divergence <- 0.5 * kl_p_m + 0.5 * kl_q_m
  
  return(js_divergence)
}




#' Generate Executive Summary of Distribution Comparison
#'
#' Provides a concise summary of the distribution comparison results.
#'
#' @param results Output from \code{compare_distributions} function
#' @param variable Character string of the variable name analyzed
#'
#' @return Prints a formatted summary to the console
#'
#' @export
executive_summary <- function(results, variable) {
  
  cat("=== EXECUTIVE SUMMARY ===\n")
  cat(paste("Variable analyzed:", variable, "\n"))
  cat(paste("Variable type:", results$variable_type, "\n\n"))
  
  if (results$variable_type == "numeric") {
    # Summary for numeric variables
    stats <- results$statistics
    mean_diff <- stats[dataset == "dt2", mean] - stats[dataset == "dt1", mean]
    median_diff <- stats[dataset == "dt2", median] - stats[dataset == "dt1", median]
    
    cat("Numeric Distribution Comparison:\n")
    cat(sprintf("Difference in means: %.4f\n", mean_diff))
    cat(sprintf("Difference in medians: %.4f\n", median_diff))
    cat(sprintf("Test used: %s\n", results$test_type))
    cat(sprintf("p-value: %.4f\n", results$comparison_test$p.value))
    cat(sprintf("KS test p-value: %.4f\n", results$ks_test$p.value))
    
  } else {
    
    # Summary for categorical variables
    cat("Categorical Distribution Comparison:\n")
    cat(sprintf("Chi-squared test p-value: %.4f\n", results$chi_squared_test$p.value))
    
    if (!is.null(results$fisher_test)) {
      cat(sprintf("Fisher's exact test p-value: %.4f\n", results$fisher_test$p.value))
    }
    
    # Show largest proportion differences
    freq_table <- results$frequency_table
    largest_diff <- freq_table[which.max(abs(diff_prop))]
    cat(sprintf("Largest proportion difference: %s (%.3f)\n", 
                largest_diff$category, largest_diff$diff_prop))
  }
  
  # Common summary elements
  if (results$variable_type == "numeric") {
    p_value <- results$comparison_test$p.value
  } else {
    p_value <- results$chi_squared_test$p.value
  }
  
  if (p_value < 0.05) {
    cat("\nCONCLUSION: Statistically significant difference between distributions\n")
  } else {
    cat("\nCONCLUSION: No statistically significant difference between distributions\n")
  }
  
  cat(sprintf("\nSample sizes - dt1: %d, dt2: %d\n", 
              length(unique(results$combined_data[group == "dt1"][[variable]])),
              length(unique(results$combined_data[group == "dt2"][[variable]]))))
}
