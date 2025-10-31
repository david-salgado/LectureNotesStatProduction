#' Generate typographical errors in categorical variables
#'
#' This function introduces various types of typographical errors into categorical 
#' variables (including binary variables) to simulate data entry mistakes commonly
#' found in real-world datasets. It supports multiple error generation mechanisms
#' and allows for fine-grained control over error types and rates.
#'
#' @param data Original dataframe (ground truth) containing the variables to modify
#' @param vars Character vector of categorical variable names to modify. These can 
#'   be character, factor, or numeric variables that will be treated as categorical
#'   for error generation purposes.
#' @param error_rate Overall error rate between 0 and 1. This represents the 
#'   proportion of values in each specified variable that will be modified. 
#'   For example, 0.05 means 5% of values will contain errors.
#' @param error_types Character vector specifying the types of errors to apply. 
#'   Available options are:
#'   \itemize{
#'     \item "typo": Simple typographical errors including character insertions, 
#'       substitutions, and deletions
#'     \item "transposition": Adjacent character swaps (e.g., "ab" becomes "ba")
#'     \item "keyboard": Errors based on QWERTY keyboard proximity (e.g., "a" becomes "s")
#'     \item "double_strike": Double key strike where a digit appears twice 
#'       (e.g., "123" becomes "1223" or "1123")
#'     \item "random": Randomly selects from all available error types
#'   }
#'   Multiple error types can be combined, and the probability distribution is 
#'   controlled by the typo_prob parameter.
#' @param typo_prob Probability value between 0 and 1 that controls the distribution
#'   of error types when multiple types are specified. Higher values favor "typo" 
#'   errors, while lower values distribute probability more evenly among other 
#'   specified error types. For example, with typo_prob = 0.6 and error_types = 
#'   c("typo", "transposition", "keyboard", "double_strike"), the probability 
#'   distribution would be: typo (60%), transposition (13.3%), keyboard (13.3%), 
#'   double_strike (13.3%).
#' @param max_typos Integer specifying the maximum number of character changes 
#'   to apply per value. For example, max_typos = 2 means each erroneous value 
#'   can have up to 2 characters modified. Higher values create more severe 
#'   distortions but may produce unrealistic results if set too high.
#' @param custom_typos Optional named list containing custom error mappings for 
#'   specific variables. Each list element should be named after a variable and 
#'   contain a named vector mapping original values to erroneous values. For example:
#'   list("urbrur" = c("1" = "2", "2" = "1")) would swap values 1 and 2 in the 
#'   "urbrur" variable. When custom mappings are provided for a variable, they 
#'   take precedence over automated error generation for matching original values.
#' @param seed Integer seed for random number generation to ensure reproducible 
#'   results. Set to NULL for non-reproducible random errors.
#'
#' @return A modified dataframe with typographical errors introduced in the 
#'   specified variables. The original dataframe structure and variable types 
#'   are preserved. Additionally, error metadata is attached as an attribute 
#'   "error_metadata" containing:
#'   \itemize{
#'     \item error_type: Type of errors applied ("typo_errors")
#'     \item error_rate: The error rate used
#'     \item affected_vars: Variables that were modified
#'     \item error_log: Detailed record of all changes made
#'     \item timestamp: When the errors were generated
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic usage with default parameters (now includes double_strike)
#' data_with_typos <- generate_typo_errors(
#'   data = original_data,
#'   vars = c("geo1", "urbrur", "relation"),
#'   error_rate = 0.03
#' )
#' 
#' # Focus on double strike errors for numeric codes
#' data_with_double_strikes <- generate_typo_errors(
#'   data = original_data,
#'   vars = c("ea", "geo1", "geo2"),
#'   error_rate = 0.02,
#'   error_types = c("double_strike"),
#'   typo_prob = 1.0  # Not used when only one error type
#' )
#' 
#' # Custom error types with specific probability distribution
#' data_with_controlled_errors <- generate_typo_errors(
#'   data = original_data,
#'   vars = c("hid", "relation"),
#'   error_rate = 0.02,
#'   error_types = c("keyboard", "double_strike"),
#'   typo_prob = 0.3  # 30% keyboard, 70% double_strike
#' )
#' }

generate_typo_errors <- function(data, vars, error_rate = 0.05, 
                                 error_types = c("typo", "transposition", "keyboard", "double_strike", "random"),
                                 typo_prob = 0.6, max_typos = 2, custom_typos = NULL,
                                 seed = 123) {
  
  # Validate input parameters
  validate_error_parameters(error_rate, vars)
  
  if (!all(vars %in% names(data))) {
    stop("Some specified variables do not exist in the dataset")
  }
  
  set.seed(seed)
  data_with_errors <- data
  error_log <- list()
  
  # QWERTY keyboard mapping for common proximity errors
  keyboard_map <- list(
    '1' = c('2', 'q'), '2' = c('1', '3', 'w', 'q'), '3' = c('2', '4', 'e', 'w'),
    '4' = c('3', '5', 'r', 'e'), '5' = c('4', '6', 't', 'r'), '6' = c('5', '7', 'y', 't'),
    '7' = c('6', '8', 'u', 'y'), '8' = c('7', '9', 'i', 'u'), '9' = c('8', '0', 'o', 'i'),
    '0' = c('9', 'p', 'o'),
    'q' = c('1', '2', 'w', 'a'), 'w' = c('2', '3', 'e', 's', 'a', 'q'), 
    'e' = c('3', '4', 'r', 'd', 's', 'w'), 'r' = c('4', '5', 't', 'f', 'd', 'e'),
    't' = c('5', '6', 'y', 'g', 'f', 'r'), 'y' = c('6', '7', 'u', 'h', 'g', 't'),
    'u' = c('7', '8', 'i', 'j', 'h', 'y'), 'i' = c('8', '9', 'o', 'k', 'j', 'u'),
    'o' = c('9', '0', 'p', 'l', 'k', 'i'), 'p' = c('0', 'l', 'o'),
    'a' = c('q', 'w', 's', 'z'), 's' = c('w', 'e', 'd', 'x', 'z', 'a'),
    'd' = c('e', 'r', 'f', 'c', 'x', 's'), 'f' = c('r', 't', 'g', 'v', 'c', 'd'),
    'g' = c('t', 'y', 'h', 'b', 'v', 'f'), 'h' = c('y', 'u', 'j', 'n', 'b', 'g'),
    'j' = c('u', 'i', 'k', 'm', 'n', 'h'), 'k' = c('i', 'o', 'l', 'm', 'j'),
    'l' = c('o', 'p', 'k'), 'z' = c('a', 's', 'x'), 'x' = c('s', 'd', 'c', 'z'),
    'c' = c('d', 'f', 'v', 'x'), 'v' = c('f', 'g', 'b', 'c'), 
    'b' = c('g', 'h', 'n', 'v'), 'n' = c('h', 'j', 'm', 'b'), 'm' = c('j', 'k', 'n')
  )
  
  # Apply simple typographical error
  apply_typo <- function(text) {
    # Ensure text is character and handle NA/empty values safely
    if (is.na(text) || is.null(text)) return(text)
    text <- as.character(text)
    if (nchar(text) == 0) return(text)
    
    text_chars <- strsplit(text, "")[[1]]
    n_typos <- sample(1:max_typos, 1)
    
    for (i in 1:n_typos) {
      pos <- sample(1:nchar(text), 1)
      current_char <- substr(text, pos, pos)
      
      # 50% probability of insertion, 30% substitution, 20% deletion
      error_type <- sample(c("insert", "substitute", "delete"), 1, 
                           prob = c(0.5, 0.3, 0.2))
      
      if (error_type == "insert") {
        new_char <- sample(letters, 1)
        text <- paste0(substr(text, 1, pos-1), new_char, substr(text, pos, nchar(text)))
      } else if (error_type == "substitute") {
        new_char <- sample(letters, 1)
        substr(text, pos, pos) <- new_char
      } else if (error_type == "delete" && nchar(text) > 1) {
        text <- paste0(substr(text, 1, pos-1), substr(text, pos+1, nchar(text)))
      }
    }
    return(text)
  }
  
  # Apply transposition error (adjacent character swap)
  apply_transposition <- function(text) {
    # Ensure text is character and handle NA/empty values safely
    if (is.na(text) || is.null(text)) return(text)
    text <- as.character(text)
    if (nchar(text) <= 1) return(text)
    
    text_chars <- strsplit(text, "")[[1]]
    pos <- sample(1:(length(text_chars)-1), 1)
    
    # Swap adjacent characters
    temp <- text_chars[pos]
    text_chars[pos] <- text_chars[pos + 1]
    text_chars[pos + 1] <- temp
    
    return(paste(text_chars, collapse = ""))
  }
  
  # Apply keyboard error (adjacent key press)
  apply_keyboard_error <- function(text) {
    # Ensure text is character and handle NA/empty values safely
    if (is.na(text) || is.null(text)) return(text)
    text <- as.character(text)
    if (nchar(text) == 0) return(text)
    
    text_chars <- strsplit(text, "")[[1]]
    pos <- sample(1:length(text_chars), 1)
    current_char <- tolower(text_chars[pos])
    
    if (current_char %in% names(keyboard_map)) {
      possible_errors <- keyboard_map[[current_char]]
      if (length(possible_errors) > 0) {
        new_char <- sample(possible_errors, 1)
        # Preserve original case
        if (grepl("[A-Z]", text_chars[pos])) {
          new_char <- toupper(new_char)
        }
        text_chars[pos] <- new_char
      }
    }
    
    return(paste(text_chars, collapse = ""))
  }
  
  # Apply double strike error (digit appears twice)
  apply_double_strike <- function(text) {
    # Ensure text is character and handle NA/empty values safely
    if (is.na(text) || is.null(text)) return(text)
    text <- as.character(text)
    if (nchar(text) == 0) return(text)
    
    text_chars <- strsplit(text, "")[[1]]
    
    # Find positions that contain digits (0-9)
    digit_positions <- which(grepl("[0-9]", text_chars))
    
    if (length(digit_positions) == 0) {
      # If no digits found, fall back to simple duplication of any character
      pos <- sample(1:length(text_chars), 1)
      current_char <- text_chars[pos]
    } else {
      # Prefer digits for double strike errors (more realistic for data entry)
      pos <- sample(digit_positions, 1)
      current_char <- text_chars[pos]
    }
    
    # Duplicate the character at the selected position
    text <- paste0(
      substr(text, 1, pos),
      current_char,
      substr(text, pos + 1, nchar(text))
    )
    
    return(text)
  }
  
  # Apply random error (randomly select from available error types)
  apply_random_error <- function(text) {
    # Create list of available error functions (excluding "random" itself)
    available_functions <- list()
    
    if ("typo" %in% error_types) available_functions$typo <- apply_typo
    if ("transposition" %in% error_types) available_functions$transposition <- apply_transposition
    if ("keyboard" %in% error_types) available_functions$keyboard <- apply_keyboard_error
    if ("double_strike" %in% error_types) available_functions$double_strike <- apply_double_strike
    
    # If no specific error types available, use typo as fallback
    if (length(available_functions) == 0) {
      return(apply_typo(text))
    }
    
    # Randomly select one of the available error functions
    selected_function <- sample(available_functions, 1)[[1]]
    return(selected_function(text))
  }
  
  # Apply errors to each specified variable
  for (var in vars) {
    message("Applying typographical errors to variable: ", var)
    
    # Convert all values to character for consistent processing
    # This handles numeric, factor, and character variables uniformly
    original_values <- as.character(data[[var]])
    
    # Select observations to modify
    n_errors <- round(length(original_values) * error_rate)
    if (n_errors == 0) {
      message(" - No errors applied (error rate too low for dataset size)")
      next
    }
    
    error_indices <- sample(seq_along(original_values), n_errors)
    
    modified_values <- original_values
    var_error_log <- list()
    
    for (idx in error_indices) {
      original_val <- original_values[idx]
      
      # Skip NA values and empty strings
      if (is.na(original_val) || original_val == "" || nchar(original_val) == 0) {
        next
      }
      
      # Apply custom error if specified and value matches
      if (!is.null(custom_typos) && var %in% names(custom_typos)) {
        custom_errors <- custom_typos[[var]]
        if (as.character(original_val) %in% names(custom_errors)) {
          modified_val <- custom_errors[[as.character(original_val)]]
          error_type_used <- "custom"
        } else {
          # Apply random error based on specified types
          error_type <- sample(error_types, 1, prob = c(typo_prob, 
                                                        rep((1-typo_prob)/(length(error_types)-1), 
                                                            length(error_types)-1)))
          modified_val <- switch(error_type,
                                 "typo" = apply_typo(original_val),
                                 "transposition" = apply_transposition(original_val),
                                 "keyboard" = apply_keyboard_error(original_val),
                                 "double_strike" = apply_double_strike(original_val),
                                 "random" = apply_random_error(original_val))
          error_type_used <- error_type
        }
      } else {
        # Apply random error based on specified types
        error_type <- sample(error_types, 1, prob = c(typo_prob, 
                                                      rep((1-typo_prob)/(length(error_types)-1), 
                                                          length(error_types)-1)))
        modified_val <- switch(error_type,
                               "typo" = apply_typo(original_val),
                               "transposition" = apply_transposition(original_val),
                               "keyboard" = apply_keyboard_error(original_val),
                               "double_strike" = apply_double_strike(original_val),
                               "random" = apply_random_error(original_val))
        error_type_used <- error_type
      }
      
      modified_values[idx] <- modified_val
      
      # Log the error
      var_error_log[[as.character(idx)]] <- list(
        original = original_val,
        modified = modified_val,
        error_type = error_type_used
      )
    }
    
    # Update the dataframe - convert back to original type if possible
    if (is.numeric(data[[var]])) {
      # Try to convert back to numeric, but keep as character if conversion fails
      converted <- suppressWarnings(as.numeric(modified_values))
      if (!any(is.na(converted))) {
        data_with_errors[[var]] <- converted
      } else {
        # If conversion fails (due to introduced letters), keep as character
        data_with_errors[[var]] <- modified_values
        warning("Variable '", var, "' converted to character due to introduced non-numeric errors")
      }
    } else if (is.factor(data[[var]])) {
      # For factors, convert to character with the new values
      data_with_errors[[var]] <- factor(modified_values)
    } else {
      # For character variables, keep as is
      data_with_errors[[var]] <- modified_values
    }
    
    error_log[[var]] <- var_error_log
    
    message(" - Errors applied: ", length(error_indices), 
            " (", round(length(error_indices)/length(original_values)*100, 2), "%)")
  }
  
  # Add error metadata as attribute
  attr(data_with_errors, "error_metadata") <- list(
    error_type = "typo_errors",
    error_rate = error_rate,
    affected_vars = vars,
    error_log = error_log,
    timestamp = Sys.time()
  )
  
  return(data_with_errors)
}
#' Create custom typo mappings for specific variables
#'
#' This helper function creates structured custom error mappings for use with 
#' generate_typo_errors(). It provides a convenient way to define specific
#' value transformations that should occur during error generation.
#'
#' @param var_name Character string specifying the variable name for which
#'   custom errors are being defined. This should match exactly the variable
#'   name in the dataset.
#' @param error_mappings Named vector or list mapping original values to 
#'   erroneous values. The names should be the original values and the 
#'   values should be the corresponding errors. For example:
#'   c("1" = "2", "2" = "1") would swap values 1 and 2. Only exact matches
#'   to the original values will be transformed; other values will undergo
#'   standard error generation.
#'
#' @return A structured list suitable for use as the custom_typos parameter
#'   in generate_typo_errors(). The returned list has one element named
#'   after var_name containing the error_mappings.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create custom errors for urban/rural variable
#' urban_rural_errors <- create_custom_typos(
#'   "urbrur", 
#'   c("1" = "2", "2" = "1")  # Swap 1 (rural) and 2 (urban)
#' )
#' 
#' # Create custom errors for relationship codes
#' relation_errors <- create_custom_typos(
#'   "relation",
#'   c("1" = "2", "2" = "3", "3" = "4")  # Shift codes by one position
#' )
#' 
#' # Use in error generation
#' custom_errors <- c(urban_rural_errors, relation_errors)
#' data_with_errors <- generate_typo_errors(
#'   data = my_data,
#'   vars = c("urbrur", "relation"),
#'   custom_typos = custom_errors
#' )
#' }
create_custom_typos <- function(var_name, error_mappings) {
  return(stats::setNames(list(error_mappings), var_name))
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