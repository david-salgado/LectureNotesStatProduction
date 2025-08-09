#' @title Generate All Possible Samples
#' @description Creates a matrix representing all possible samples (binary vectors) 
#' for a population of size N.
#' @param N Integer specifying population size (must be positive)
#' @return A matrix where each row represents a possible sample (excluding the empty sample)
#' @examples
#' generate_all_possible_samples(3)
generate_all_possible_samples <- function(N) {
  stopifnot(N > 0, is.numeric(N), N %% 1 == 0)
  samples <- as.matrix(expand.grid(lapply(1:N, function(i) c(0, 1))))
  colnames(samples) <- paste0('u', 1:N)
  samples <- samples[-1, ] # Remove empty sample
  rownames(samples) <- paste0("sample_", 1:nrow(samples))
  return(samples)
}

#' @title Define Sampling Design Probabilities
#' @description Assigns probabilities to each possible sample according to a predefined design.
#' @param samples Matrix of possible samples (from generate_all_possible_samples())
#' @param probs Vector of probabilities for each sample
#' @return Numeric vector of probabilities for each sample
#' @details The default implementation uses fixed probabilities that sum to 1.
#' For populations >3, probabilities are recycled with warning.
define_sampling_design <- function(samples, probs) {
  
  n_samples <- nrow(samples)
  stopifnot(n_samples == length(probs))
  
  names(probs) <- rownames(samples)
  stopifnot("Probabilities must be non-negative" = all(probs >= 0),
  "Probabilities must sum to 1" = abs(sum(probs) - 1) < 1e-9)
return(probs)
}

#' @title Simulate Sample Selection
#' @description Generates samples according to the specified sampling design.
#' @param samples Matrix of possible samples
#' @param probs Probability vector for each sample
#' @param n_iter Integer specifying number of samples to generate
#' @return Matrix of selected samples (n_iter rows × N columns)
simulate_sample_selection <- function(samples, probs, n_iter) {
  stopifnot(n_iter > 0, is.numeric(n_iter), n_iter %% 1 == 0)
  sample_indices <- sample(1:nrow(samples), size = n_iter, 
                           replace = TRUE, prob = probs)
  selected_samples <- samples[sample_indices, ]
  return(selected_samples)
}

#' @title Calculate Inclusion Probabilities
#' @description Computes first and second order inclusion probabilities.
#' @param samples Matrix of all possible samples
#' @param probs Probability vector for each sample
#' @param selected_samples Matrix of simulated samples
#' @param N Population size
#' @return List containing:
#' \itemize{
#'   \item pik_theoretical: Theoretical first-order inclusion probabilities
#'   \item pik_empirical: Empirical first-order inclusion probabilities
#'   \item pikl_theoretical: Theoretical second-order inclusion probabilities matrix
#'   \item pikl_empirical: Empirical second-order inclusion probabilities matrix
#' }
calculate_inclusion_probabilities <- function(samples, probs, selected_samples, N) {
  
  units_name <- paste0('u', 1:N)
  # First order probabilities
  pik_theoretical <- colSums(samples * probs)
  pik_empirical <- colMeans(selected_samples)
  names(pik_theoretical) <- names(pik_empirical) <- units_name
  
  # Second order probabilities
  unit_pairs <- as.data.table(expand.grid(k = 1:N, l = 1:N))[k < l]
  unit_pairs[, pikl := vapply(1:.N, function(i) {
    sum(samples[, k[i]] * samples[, l[i]] * probs)
  }, numeric(1))]
  
  # Theoretical matrix
  pikl_theoretical <- matrix(NA_real_, N, N)
  diag(pikl_theoretical) <- pik_theoretical
  for(i in 1:nrow(unit_pairs)) {
    k <- unit_pairs$k[i]
    l <- unit_pairs$l[i]
    pikl_theoretical[k, l] <- pikl_theoretical[l, k] <- unit_pairs$pikl[i]
  }
  dimnames(pikl_theoretical) <- list(units_name, units_name)
  
  # Empirical matrix
  pikl_empirical <- matrix(NA_real_, N, N)
  diag(pikl_empirical) <- pik_empirical
  for(k in 1:(N-1)) {
    for(l in (k+1):N) {
      pkl <- mean(selected_samples[,k] * selected_samples[,l])
      pikl_empirical[k,l] <- pikl_empirical[l,k] <- pkl
    }
  }
  dimnames(pikl_empirical) <- list(units_name, units_name)
  
  return(list(
    pik_theoretical = pik_theoretical,
    pik_empirical = pik_empirical,
    pikl_theoretical = pikl_theoretical,
    pikl_empirical = pikl_empirical
  ))
}

#' @title Analyze Sample Size Properties
#' @description Computes theoretical and empirical sample size statistics.
#' @param samples Matrix of all possible samples
#' @param probs Probability vector for each sample
#' @param selected_samples Matrix of simulated samples
#' @param inclusion_probs List from calculate_inclusion_probabilities()
#' @return List containing:
#' \itemize{
#'   \item size_mean: Theoretical and empirical mean sample sizes
#'   \item size_variance: Theoretical and empirical sample size variances
#'   \item Deltakl: Theoretical and empirical covariance matrices
#' }
analyze_sample_size_properties <- function(samples, probs, selected_samples, inclusion_probs) {
  sample_sizes <- rowSums(samples)
  
  # Mean calculations
  size_mean_theoretical <- sum(sample_sizes * probs)
  size_mean_empirical <- mean(rowSums(selected_samples))
  
  # Variance calculations
  size_var_theoretical <- sum(probs * (sample_sizes - size_mean_theoretical)^2)
  size_var_empirical <- var(rowSums(selected_samples))
  
  # Covariance matrices
  Deltakl_theoretical <- inclusion_probs$pikl_theoretical - 
    outer(inclusion_probs$pik_theoretical, inclusion_probs$pik_theoretical)
  
  Deltakl_empirical <- inclusion_probs$pikl_empirical - 
    outer(inclusion_probs$pik_empirical, inclusion_probs$pik_empirical)
  
  return(list(
    Deltakl = list(theoretical = Deltakl_theoretical, empirical = Deltakl_empirical),
    size_mean = c(theoretical = size_mean_theoretical, empirical = size_mean_empirical),
    size_variance = c(theoretical = size_var_theoretical, empirical = size_var_empirical)
  ))
}

#' @title Execute Complete Sampling Analysis
#' @description Main function that runs the full sampling analysis pipeline.
#' @param N Integer specifying population size (default = 3)
#' @param n_iter Integer specifying number of samples (default = 10000)
#' @return List containing all analysis results:
#' \itemize{
#'   \item possible_samples
#'   \item sample_probabilities
#'   \item simulated_samples
#'   \item inclusion_probabilities
#'   \item sample_size_properties
#' }
#' @examples
#' results <- run_sampling_analysis(N = 3, n_iter = 1000)
#' summary(results)
run_sampling_analysis <- function(probs, N = 3, n_iter = 10000) {
  # Input validation
  stopifnot(
    is.numeric(N), length(N) == 1, N > 0, N %% 1 == 0,
    is.numeric(n_iter), length(n_iter) == 1, n_iter > 0, n_iter %% 1 == 0,
    length(probs) == 2^N-1
  )
  
  # Generate components
  samples_possible <- generate_all_possible_samples(N)
  samples_prob <- define_sampling_design(samples_possible, probs)
  samples_iter <- simulate_sample_selection(samples_possible, samples_prob, n_iter)
  
  # Calculate probabilities
  inclusion_probs <- calculate_inclusion_probabilities(
    samples_possible, samples_prob, samples_iter, N
  )
  
  # Calculate properties
  size_props <- analyze_sample_size_properties(
    samples_possible, samples_prob, samples_iter, inclusion_probs
  )
  
  # Return consolidated results
  structure(
    list(
      parameters = list(N = N, n_iter = n_iter),
      possible_samples = samples_possible,
      sample_probabilities = samples_prob,
      simulated_samples = samples_iter,
      inclusion_probabilities = inclusion_probs,
      sample_size_properties = size_props
    ),
    class = "SamplingAnalysis"
  )
}

#' @title Print Sampling Analysis Summary
#' @description S3 method for summarizing SamplingAnalysis objects
#' @param x SamplingAnalysis object from run_sampling_analysis()
#' @param ... Additional arguments (unused)
#' @export
print.SamplingAnalysis <- function(x, ...) {
  cat("Sampling Design Analysis\n")
  cat("Population size (N):", x$parameters$N, "\n")
  cat("Iterations:", x$parameters$n_iter, "\n\n")
  
  cat("First-order Inclusion Probabilities:\n")
  print(data.frame(
    Unit = paste0("u", 1:x$parameters$N),
    Theoretical = x$inclusion_probabilities$pik_theoretical,
    Empirical = x$inclusion_probabilities$pik_empirical
  ))
  
  cat("\nSecond-order Inclusion Probabilities:\n")
  print(list(
    Theoretical = x$inclusion_probabilities$pikl_theoretical,
    Empirical = x$inclusion_probabilities$pikl_empirical
  ))
  
  cat("\nSample Size Properties:\n")
  print(data.frame(
    Statistic = c("Mean", "Variance"),
    Theoretical = c(x$sample_size_properties$size_mean["theoretical"],
                    x$sample_size_properties$size_variance["theoretical"]),
    Empirical = c(x$sample_size_properties$size_mean["empirical"],
                  x$sample_size_properties$size_variance["empirical"])
  ))
}