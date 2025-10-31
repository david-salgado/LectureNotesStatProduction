#' Introduce outliers into numerical columns using various mechanisms
#'
#' @param data A dataframe containing the original ground truth data.
#' @param numerical_columns A character vector specifying the names of numerical columns to contaminate.
#' @param mechanisms A character vector specifying the mechanism(s) to use for outlier generation.
#'   Options: "gaussian" (extreme values from normal distribution),
#'            "shift" (shift values by large amount),
#'            "replace_extreme" (replace with extreme percentiles).
#'   Can be a single mechanism or a vector of same length as numerical_columns. Default is "gaussian".
#' @param proportion The proportion of observations in each column to convert to outliers (0 to 1). Default is 0.02.
#' @param strength Multiplier controlling how extreme the outliers are. Higher values create more extreme outliers. Default is 3.
#' @param seed An optional seed for reproducibility.
#'
#' @return A new dataframe with outliers introduced in specified columns.
#'
#' @examples
#' \dontrun{
#' # Introduce gaussian outliers in age and income columns
#' data_with_outliers <- generate_outliers(
#'   data = original_data,
#'   numerical_columns = c("age", "income"),
#'   mechanisms = "gaussian",
#'   proportion = 0.03,
#'   strength = 4,
#'   seed = 123
#' )
#' 
#' # Use different mechanisms for different columns
#' data_with_outliers <- generate_outliers(
#'   data = original_data,
#'   numerical_columns = c("age", "income", "height"),
#'   mechanisms = c("gaussian", "shift", "replace_extreme"),
#'   proportion = 0.02,
#'   strength = 3,
#'   seed = 123
#' )
#' }
generate_outliers <- function(data, numerical_columns, mechanisms = "gaussian", proportion = 0.02, strength = 3, seed = NULL) {
  
  # Input validation
  if (!is.data.frame(data)) {
    
    stop("The 'data' argument must be a dataframe.")
    
  }
  data <- as.data.table(data)
  
  
  if (!all(numerical_columns %in% names(data))) {
    
    missing_cols <- numerical_columns[!numerical_columns %in% names(data)]
    stop(paste("The following specified columns do not exist in the dataframe:", paste(missing_cols, collapse = ", ")))
    
  }
  
  # Check if specified columns are numeric
  non_numeric_cols <- numerical_columns[!sapply(data[,..numerical_columns], function(x){is.numeric(x) | is.integer(x)})]
  if (length(non_numeric_cols) > 0) {
    
    stop(paste("The following specified columns are not numeric:", paste(non_numeric_cols, collapse = ", ")))
    
  }
  
  if (proportion < 0 || proportion > 1) {
    
    stop("The 'proportion' argument must be a value between 0 and 1.")
    
  }
  
  if (strength <= 0) {
    
    stop("The 'strength' argument must be a positive value.")
    
  }
  
  valid_mechanisms <- c("gaussian", "shift", "replace_extreme")
  if (!all(mechanisms %in% valid_mechanisms)) {
    
    invalid_mech <- mechanisms[!mechanisms %in% valid_mechanisms]
    stop(paste("Invalid mechanism(s):", paste(invalid_mech, collapse = ", "), 
               ". Valid options are:", paste(valid_mechanisms, collapse = ", ")))
    
  }
  
  # Handle mechanism vector: recycle if necessary
  if (length(mechanisms) == 1) {
    
    mechanisms <- rep(mechanisms, length(numerical_columns))
    
  } else if (length(mechanisms) != length(numerical_columns)) {
    
    stop("The 'mechanisms' vector must have length 1 or the same length as 'numerical_columns'.")
    
  }
  
  # Create a copy of the original data
  data_with_outliers <- copy(data)
    
  # Set seed for reproducibility if provided
  if (!is.null(seed)) set.seed(seed)
  
  # Apply outlier generation to each specified column
  for (i in seq_along(numerical_columns)) {
    
    col <- numerical_columns[i]
    mechanism <- mechanisms[i]
    
    # Calculate number of outliers to generate
    n_outliers <- max(1, round(nrow(data) * proportion))
    
    # Randomly select which rows to contaminate
    outlier_indices <- sample(seq_len(nrow(data)), n_outliers)
    
    # Generate outliers based on the specified mechanism
    if (mechanism == "gaussian") {
      
      # Extreme values from normal distribution based on column statistics
      col_mean <- mean(data_with_outliers[[col]], na.rm = TRUE)
      col_sd   <- sd(data_with_outliers[[col]], na.rm = TRUE)
      
      # Generate extreme values (both positive and negative extremes)
      extreme_values <- rnorm(
        n_outliers, 
        mean = col_mean, 
        sd = strength * col_sd
      )
      
      data_with_outliers[outlier_indices, col] <- extreme_values
      
    } 
    if (mechanism == "shift") {
      
      # Shift original values by a large amount
      col_range <- range(data_with_outliers[[col]], na.rm = TRUE)
      col_span  <- col_range[2] - col_range[1]
      
      # Determine shift direction randomly for each outlier
      directions <- sample(c(-1, 1), n_outliers, replace = TRUE)
      
      # Apply large shift (proportional to column range)
      shifts <- directions * strength * col_span
      data_with_outliers[outlier_indices, (col) := get(col) + shifts]
      
    }
    if (mechanism == "replace_extreme") {
      
      # Replace with extreme percentile values
      col_min <- min(data_with_outliers[[col]], na.rm = TRUE)
      col_max <- max(data_with_outliers[[col]], na.rm = TRUE)
      col_span <- col_max - col_min
      
      # Create extreme values beyond original range
      extreme_low  <- col_min - strength * 0.5 * col_span
      extreme_high <- col_max + strength * 0.5 * col_span
      
      # Assign extreme low or high values randomly
      extreme_values <- sample(c(extreme_low, extreme_high), 
                               n_outliers, replace = TRUE)
      
      data_with_outliers[outlier_indices, col] <- extreme_values
    }
  }
  
  return(data_with_outliers)
}