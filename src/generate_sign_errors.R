#' Introduce sign errors in numeric variables
#'
#' This function introduces sign errors (positive/negative inversion) into 
#' specified numeric variables to simulate data entry mistakes where the sign
#' of a value is incorrectly recorded.
#'
#' @param data Original dataframe (ground truth) containing the variables to modify
#' @param vars Character vector of numeric variable names to modify. These should
#'   be numeric variables where sign inversion makes conceptual sense (e.g., 
#'   financial amounts, balances, differences, etc.).
#' @param error_rate Overall error rate between 0 and 1. This represents the 
#'   proportion of values in each specified variable that will have their sign
#'   inverted. For example, 0.02 means 2% of values will have sign errors.
#' @param skip_zeros Logical indicating whether to skip zero values when applying
#'   sign errors (default: TRUE). Zero values remain unchanged since sign 
#'   inversion has no effect on zero.
#' @param skip_negative Logical indicating whether to skip already negative values
#'   (default: FALSE). If TRUE, only positive values will be considered for 
#'   sign inversion. If FALSE, both positive and negative values can be inverted.
#' @param custom_prob Optional named list of variable-specific error probabilities.
#'   If provided, overrides the global error_rate for specified variables.
#' @param seed Integer seed for random number generation to ensure reproducible 
#'   results. Set to NULL for non-reproducible random errors.
#'
#' @return A modified dataframe with sign errors introduced in the specified 
#'   variables. The original dataframe structure and variable types are preserved.
#'   Additionally, error metadata is attached as an attribute "error_metadata" 
#'   containing:
#'   \itemize{
#'     \item error_type: Type of errors applied ("sign_errors")
#'     \item error_rate: The error rate used (or custom probabilities)
#'     \item affected_vars: Variables that were modified
#'     \item error_log: Detailed record of all changes made
#'     \item timestamp: When the errors were generated
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic usage with default parameters
#' data_with_sign_errors <- generate_sign_errors(
#'   data = original_data,
#'   vars = c("exp_01", "exp_02", "tot_exp"),
#'   error_rate = 0.01
#' )
#' 
#' # Apply only to positive values (skip already negative values)
#' data_with_pos_sign_errors <- generate_sign_errors(
#'   data = original_data,
#'   vars = c("exp_01", "exp_02"),
#'   error_rate = 0.02,
#'   skip_negative = TRUE
#' )
#' 
#' # Custom error probabilities for different variables
#' custom_probs <- list(
#'   "exp_01" = 0.03,  # 3% error rate for food expenses
#'   "exp_02" = 0.01   # 1% error rate for alcohol/tobacco
#' )
#' 
#' data_with_custom_sign_errors <- generate_sign_errors(
#'   data = original_data,
#'   vars = c("exp_01", "exp_02", "exp_03"),
#'   error_rate = 0.02,  # Default for exp_03
#'   custom_prob = custom_probs
#' )
#' 
#' # Access error metadata
#' error_info <- attr(data_with_sign_errors, "error_metadata")
#' print(paste("Sign errors applied to", length(error_info$affected_vars), "variables"))
#' }

generate_sign_errors <- function(data, vars, error_rate = 0.01, 
                                 skip_zeros = TRUE, skip_negative = FALSE,
                                 custom_prob = NULL, seed = 123) {
  
  # Validate input parameters
  validate_error_parameters(error_rate, vars)
  
  if (!all(vars %in% names(data))) {
    stop("Some specified variables do not exist in the dataset")
  }
  
  # Check that specified variables are numeric
  non_numeric_vars <- vars[!sapply(data[, ..vars], is.numeric)]
  if (length(non_numeric_vars) > 0) {
    stop("The following variables are not numeric: ", 
         paste(non_numeric_vars, collapse = ", "))
  }
  
  set.seed(seed)
  data_with_errors <- copy(data)
  error_log <- list()
  
  # Apply sign errors to each specified variable
  for (var in vars) {
    message("Applying sign errors to variable: ", var)
    
    original_values <- data[[var]]
    
    # Use custom probability if specified, otherwise use global error_rate
    var_error_rate <- if (!is.null(custom_prob) && var %in% names(custom_prob)) {
      custom_prob[[var]]
    } else {
      error_rate
    }
    
    # Validate variable-specific error rate
    if (var_error_rate < 0 || var_error_rate > 1) {
      stop("Error rate for variable '", var, "' must be between 0 and 1")
    }
    
    # Identify candidate positions for sign inversion
    candidate_indices <- which(!is.na(original_values))
    
    # Apply filters based on parameters
    if (skip_zeros) {
      candidate_indices <- candidate_indices[original_values[candidate_indices] != 0]
    }
    
    if (skip_negative) {
      candidate_indices <- candidate_indices[original_values[candidate_indices] > 0]
    }
    
    # Select observations to modify
    n_errors <- round(length(candidate_indices) * var_error_rate)
    
    if (n_errors == 0) {
      message(" - No errors applied (error rate too low or no suitable values)")
      next
    }
    
    error_indices <- sample(candidate_indices, n_errors)
    
    modified_values <- original_values
    var_error_log <- list()
    
    for (idx in error_indices) {
      original_val <- original_values[idx]
      
      # Invert the sign
      modified_val <- -original_val
      
      modified_values[idx] <- modified_val
      
      # Log the error
      var_error_log[[as.character(idx)]] <- list(
        original = original_val,
        modified = modified_val,
        error_type = "sign_inversion"
      )
    }
    
    # Update the dataframe
    data_with_errors[[var]] <- modified_values
    
    error_log[[var]] <- var_error_log
    
    message(" - Errors applied: ", length(error_indices), 
            " (", round(length(error_indices)/length(candidate_indices)*100, 2), "% of eligible values)")
    
    # Show summary of changes
    if (length(error_indices) > 0) {
      original_vals <- original_values[error_indices]
      modified_vals <- modified_values[error_indices]
      pos_to_neg <- sum(original_vals > 0 & modified_vals < 0)
      neg_to_pos <- sum(original_vals < 0 & modified_vals > 0)
      zero_changes <- sum(original_vals == 0 & modified_vals == 0)
      
      message("   - Positive → Negative: ", pos_to_neg)
      message("   - Negative → Positive: ", neg_to_pos)
      if (zero_changes > 0) {
        message("   - Zero values (unchanged): ", zero_changes)
      }
    }
  }
  
  # Add error metadata as attribute
  attr(data_with_errors, "error_metadata") <- list(
    error_type = "sign_errors",
    error_rate = if (!is.null(custom_prob)) custom_prob else error_rate,
    affected_vars = vars,
    skip_zeros = skip_zeros,
    skip_negative = skip_negative,
    error_log = error_log,
    timestamp = Sys.time()
  )
  
  return(data_with_errors)
}

#' Validate error generation parameters
#'
#' Internal function to validate parameters for error generation functions.
#' Ensures that error rates are within valid bounds and that variable lists
#' are not empty.
#'
#' @param error_rate Numeric value between 0 and 1 representing the proportion
#'   of values to modify. Values outside this range will throw an error.
#' @param vars Character vector of variable names. An empty vector will
#'   throw an error since at least one variable must be specified for
#'   error generation.
#'
#' @return No return value. Called for side effects of parameter validation.
#' @keywords internal

validate_error_parameters <- function(error_rate, vars) {
  if (error_rate < 0 || error_rate > 1) {
    stop("Error rate must be between 0 and 1")
  }
  if (length(vars) == 0) {
    stop("At least one variable must be specified")
  }
}

#' Analyze potential sign error impact
#'
#' This helper function analyzes numeric variables to determine their suitability
#' for sign errors and provides statistics about potential impact.
#'
#' @param data Dataframe to analyze
#' @param vars Character vector of numeric variable names to analyze
#' @return A list with analysis results for each variable including:
#'   \itemize{
#'     \item n_positive: Number of positive values
#'     \item n_negative: Number of negative values  
#'     \item n_zero: Number of zero values
#'     \item mean_positive: Mean of positive values
#'     \item mean_negative: Mean of negative values
#'     \item potential_impact: Summary of potential changes
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Analyze variables before applying sign errors
#' analysis <- analyze_sign_error_impact(
#'   data = original_data,
#'   vars = c("exp_01", "exp_02", "tot_exp")
#' )
#' 
#' # View analysis for a specific variable
#' print(analysis$exp_01)
#' }

analyze_sign_error_impact <- function(data, vars) {
  
  # Validate input
  if (!all(vars %in% names(data))) {
    stop("Some specified variables do not exist in the dataset")
  }
  
  # Convert to regular dataframe if it's a data.table to avoid indexing issues
  if (data.table::is.data.table(data)) {
    data_df <- as.data.frame(data)
  } else {
    data_df <- data
  }
  
  # Check for non-numeric variables
  non_numeric_vars <- vars[!sapply(data_df[vars], is.numeric)]
  if (length(non_numeric_vars) > 0) {
    stop("The following variables are not numeric: ", 
         paste(non_numeric_vars, collapse = ", "))
  }
  
  analysis <- list()
  
  for (var in vars) {
    values <- data_df[[var]]
    non_na_values <- values[!is.na(values)]
    
    if (length(non_na_values) == 0) {
      analysis[[var]] <- list(
        n_positive = 0,
        n_negative = 0,
        n_zero = 0,
        mean_positive = NA,
        mean_negative = NA,
        potential_impact = "No non-NA values"
      )
      next
    }
    
    n_positive <- sum(non_na_values > 0)
    n_negative <- sum(non_na_values < 0)
    n_zero <- sum(non_na_values == 0)
    
    mean_positive <- if (n_positive > 0) mean(non_na_values[non_na_values > 0]) else NA
    mean_negative <- if (n_negative > 0) mean(non_na_values[non_na_values < 0]) else NA
    
    # Calculate potential impact
    potential_changes <- list(
      positive_to_negative = n_positive,
      negative_to_positive = n_negative,
      zero_unchanged = n_zero
    )
    
    analysis[[var]] <- list(
      n_positive = n_positive,
      n_negative = n_negative,
      n_zero = n_zero,
      mean_positive = mean_positive,
      mean_negative = mean_negative,
      potential_impact = potential_changes
    )
  }
  
  return(analysis)
}

#' Create custom probability mappings for sign errors
#'
#' This helper function creates structured custom probability mappings for use with 
#' generate_sign_errors().
#'
#' @param var_name Variable name
#' @param probability Error probability for the variable (between 0 and 1)
#' @return Structured list for custom_prob parameter
#' @export
#' @examples
#' \dontrun{
#' # Create custom probabilities for different variables
#' custom_probs <- create_custom_sign_probabilities(
#'   "exp_01", 
#'   0.03  # 3% error rate for food expenses
#' )
#' 
#' # Combine multiple custom probabilities
#' all_custom_probs <- c(
#'   create_custom_sign_probabilities("exp_01", 0.03),
#'   create_custom_sign_probabilities("exp_02", 0.01),
#'   create_custom_sign_probabilities("tot_exp", 0.005)
#' )
#' }

create_custom_sign_probabilities <- function(var_name, probability) {
  if (probability < 0 || probability > 1) {
    stop("Probability must be between 0 and 1")
  }
  return(stats::setNames(list(probability), var_name))
}