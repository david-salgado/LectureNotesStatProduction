#' @title Generate Missing Values in Data.Table
#' @description 
#' Generates missing values (NA) in a data.table according to specified mechanisms (MCAR, MAR, NMAR)
#' and mechanism types (linear, nonlinear, logistic, randomforest). Handles all common data types
#' (numeric, integer, character, logical, Date, factor) with appropriate NA types.
#'
#' @param complete_data.dt A data.table containing complete data (will not be modified).
#' @param target_vars Character vector specifying column names where missing values should be introduced.
#' @param auxiliary_vars Character vector of column names that influence missingness (for MAR/NMAR).
#' @param prop_missing Numeric vector (0-1) specifying missing proportion for each target variable. 
#'        Defaults to 0.1 for all variables.
#' @param mechanism Missing data mechanism: "MCAR" (default), "MAR", or "NMAR".
#' @param mechanism_type Type of MAR/NMAR mechanism: "linear", "nonlinear", "logistic", "randomforest".
#' @param mechanism_args List of additional arguments for the mechanism:
#' \itemize{
#'   \item For linear: 'weights' (numeric vector)
#'   \item For nonlinear: 'degree' (polynomial degree)
#'   \item For logistic: 'interaction' (logical)
#'   \item For randomforest: 'num.trees', 'mtry', 'importance'
#'   \item For NMAR: 'threshold' (numeric cutoff)
#' }
#' @param seed Optional random seed for reproducibility.
#'
#' @return A modified copy of the input data.table with missing values introduced.
#'
#' @details
#' \strong{Mechanisms:}
#' \itemize{
#'   \item \strong{MCAR}: Missing completely at random (no dependencies)
#'   \item \strong{MAR}: Missing at random (depends on other observed variables)
#'   \item \strong{NMAR}: Not missing at random (depends on unobserved values)
#' }
#'
#' \strong{Mechanism Types (for MAR/NMAR):}
#' \itemize{
#'   \item \strong{linear}: Linear relationship with auxiliary variables
#'   \item \strong{nonlinear}: Polynomial relationship (degree configurable)
#'   \item \strong{logistic}: Logistic regression model
#'   \item \strong{randomforest}: Random Forest probability model (via ranger)
#' }
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' 
#' # Sample data
#' data <- data.table(
#'   id = 1:1000,
#'   age = rnorm(1000, 50, 10),
#'   income = rlnorm(1000, 10, 0.5),
#'   status = sample(c("A", "B", "C"), 1000, replace = TRUE),
#'   date = as.Date("2020-01-01") + sample(0:365, 1000, replace = TRUE)
#' )
#'
#' # MCAR - 10% missing in age and income
#' mcar_data <- generate_missing_values(
#'   data,
#'   target_vars = c("age", "income"),
#'   prop_missing = c(0.1, 0.1),
#'   mechanism = "MCAR"
#' )
#'
#' # MAR linear - income missing depends on age
#' mar_linear <- generate_missing_values(
#'   data,
#'   target_vars = "income",
#'   auxiliary_vars = "age",
#'   prop_missing = 0.2,
#'   mechanism = "MAR",
#'   mechanism_type = "linear"
#' )
#'
#' # MAR logistic - status missing depends on age and income
#' mar_logistic <- generate_missing_values(
#'   data,
#'   target_vars = "status",
#'   auxiliary_vars = c("age", "income"),
#'   prop_missing = 0.15,
#'   mechanism = "MAR",
#'   mechanism_type = "logistic",
#'   mechanism_args = list(interaction = TRUE)
#' )
#'
#' # NMAR - income missing depends on its own values (>100000)
#' nmar_data <- generate_missing_values(
#'   data,
#'   target_vars = "income",
#'   prop_missing = 0.25,
#'   mechanism = "NMAR",
#'   mechanism_args = list(threshold = 100000)
#' )
#' }
#'
#' @importFrom data.table copy
#' @importFrom stats poly plogis rbinom runif glm binomial
#' @importFrom ranger ranger
#' @export
generate_missing_values <- function(
    complete_data.dt, 
    target_vars, 
    auxiliary_vars,
    prop_missing = rep(0.1, length(target_vars)),
    mechanism = c("MCAR", "MAR", "NMAR"), 
    mechanism_type = c("linear", "nonlinear", "logistic", "randomforest"),
    mechanism_args = NULL,
    seed = NULL) {
    
    if (!is.null(seed)) set.seed(seed)
    
    # Input validation
    stopifnot(
      data.table::is.data.table(complete_data.dt),
      all(target_vars %in% names(complete_data.dt)),
      #if (mechanism != "MCAR") all(auxiliary_vars %in% names(complete_data.dt)),
      length(target_vars) == length(prop_missing),
      all(prop_missing >= 0 & prop_missing <= 1),
      mechanism %in% c("MCAR", "MAR", "NMAR"),
      mechanism_type %in% c("linear", "nonlinear", "logistic", "randomforest")
    )

  # Create a copy to avoid modifying original data
  data_with_missing <- data.table::copy(complete_data.dt)
  n <- nrow(data_with_missing)
  
  # Generate missing indices based on mechanism
  if (mechanism == "MCAR") {
    for (i in seq_along(target_vars)) {
      
      var <- target_vars[i]
      cat(paste0("Generating missing values in variable ", var, "..."))
      p <- prop_missing[i]
      col_type <- class(data_with_missing[[var]])[1]  # Get primary class
      
      # Generate missing indices
      na_indices <- sample(1:n, size = round(n * p))
      
      # Assign type-specific NA
      if (col_type %in% c("integer", "factor")) {
        
        data_with_missing[na_indices, (var) := NA_integer_]
        
      }
      if (col_type == "numeric") {
        
        data_with_missing[na_indices, (var) := NA_real_]
        
      } 
      if (col_type == "character") {
        
        data_with_missing[na_indices, (var) := NA_character_]
        
      } 
      if (col_type == "logical") {
        
        data_with_missing[na_indices, (var) := NA]
        
      } 
      if (col_type == "Date") {
        
        data_with_missing[na_indices, (var) := as.Date(NA)]
        
      } 
      if (!col_type %in% c("integer", "factor", "numeric", "character", "logical", "Date")){
        
        warning(paste("Unsupported type for variable", var, "using default NA"))
        data_with_missing[na_indices, (var) := NA]
        
      }
    
      cat("ok.\n")
    }
  }
  
  if (mechanism == "MAR") {
    
    mechanism_type <- match.arg(mechanism_type)  
    
    # Base probability matrix
    prob_matrix <- matrix(prop_missing, nrow = n, ncol = length(target_vars), byrow = TRUE)
    
    # Mechanism implementations
    if (mechanism_type == "linear") {
      
      for (i in seq_along(target_vars)) {
        
        cat(paste0("Generating missing values for variable ", target_vars[i], "..."))
        aux_data <- as.matrix(complete_data.dt[, auxiliary_vars, with = FALSE])
        weights <- if (!is.null(mechanism_args$weights)) {
          
          mechanism_args$weights
          
        } else {
          
          rep(1, length(auxiliary_vars))
          
        }
        linear_pred <- aux_data %*% weights
        prob_matrix[, i] <- plogis(scale(linear_pred)) * prop_missing[i] * 2
        cat("ok.\n")
        
      }
      
      prob_matrix <- pmin(pmax(prob_matrix, 0.01), 0.99)
      
    }  
    if (mechanism_type == "nonlinear") {
      
      degree <- ifelse(is.null(mechanism_args$degree), 2, mechanism_args$degree)
      aux_data <- as.matrix(complete_data.dt[, auxiliary_vars, with = FALSE])
      
      for (i in seq_along(target_vars)) {
        
        cat(paste0("Generating missing values for variable ", target_vars[i], "..."))
        # Generate polynomial features for each target variable
        poly_terms <- poly(aux_data, degree = degree, raw = TRUE)
        
        # Create unique random coefficients for each target variable
        coefs <- matrix(runif(ncol(poly_terms)), ncol = 1)
                        
        # Calculate nonlinear effects
        effects <- poly_terms %*% coefs
                        
        # Scale to [0,1] and multiply by target-specific proportion
        prob_matrix[, i] <- plogis(scale(effects)) * prop_missing[i]
        cat("ok.\n")
        
      }
      
      prob_matrix <- pmin(pmax(prob_matrix, 0.01), 0.99)
    
    } 
    if (mechanism_type == "logistic") {
      
      for (i in seq_along(target_vars)) {
        
        cat(paste0("Generating missing values for variable ", target_vars[i], "..."))
        # Create synthetic binary outcome based on proportion
        synth_y <- rbinom(nrow(complete_data.dt), 1, prop_missing[i])
        
        # Prepare data for glm
        model_data <- cbind(
          data.table(y = synth_y), complete_data.dt[, auxiliary_vars, with = FALSE])
        
        # Build formula
        frm <- paste("y ~", paste(auxiliary_vars, collapse = " + "))
        if (!is.null(mechanism_args$interaction) && mechanism_args$interaction) {
          frm <- paste(frm, "+", paste(auxiliary_vars, collapse = ":"))
        }
        
        # Fit logistic model
        model <- glm(
          as.formula(frm),
          data = model_data,
          family = binomial()
        )
        
        # Predict probabilities
        prob_matrix[, i] <- predict(model, type = "response")
        
        cat("ok.\n")
      }
      
      prob_matrix <- pmin(pmax(prob_matrix, 0.01), 0.99)
      
    } 
    if (mechanism == "randomforest") {
      
      # Configuración de parámetros por defecto
      default_args <- list(
        num.trees = 100,
        mtry = floor(sqrt(length(auxiliary_vars))),
        importance = "none",
        probability = TRUE,
        classification = TRUE
      )
      mechanism_args <- modifyList(default_args, mechanism_args)
      
      for (i in seq_along(target_vars)) {
        
        cat(paste0("Generating missing values for variable ", target_vars[i], "..."))
        var <- target_vars[i]
        p   <- prop_missing[i]
        
        # 1. Generar variable objetivo sintética
        synth_y <- factor(rbinom(n, 1, p), levels = c(0, 1))
        
        # 2. Preparar datos para ranger
        model_data <- cbind(
          data.table(y = synth_y), complete_data.dt[, auxiliary_vars, with = FALSE])
        
        # 3. Entrenar modelo ranger
        rf_model <- ranger::ranger(
          formula     = y ~ .,
          data        = model_data,
          num.trees   = mechanism_args$num.trees,
          mtry        = mechanism_args$mtry,
          importance  = mechanism_args$importance,
          probability = TRUE,
          classification = TRUE
        )
        
        # 4. Predecir probabilidades (clase positiva)
        prob_matrix[, i] <- predict(rf_model, data = model_data)$predictions[, 2]
        
        cat("ok.\n")
      }
      
      prob_matrix <- pmin(pmax(prob_matrix, 0.01), 0.99)
      
    } 

    # Apply missing values with type preservation
    for (i in seq_along(target_vars)) {
      
      var <- target_vars[i]
      na_indices <- which(runif(n) < prob_matrix[, i])
      
      # Type-specific NA assignment
      col_type <- class(data_with_missing[[var]])[1]
      na_value <- switch(
        col_type,
        "integer" = NA_integer_,
        "numeric" = NA_real_,
        "character" = NA_character_,
        "logical" = NA,
        "factor" = factor(NA, levels = levels(data_with_missing[[var]])),
        "Date" = as.Date(NA),
        NA)
      
      data_with_missing[na_indices, (var) := na_value]
    }

  }
  
  if (mechanism == "NMAR") {
      
      # Not missing at random (depends on target var itself)
      prob_missing <- ifelse(data_with_missing[[var]] > threshold, 
                             prop_missing * 3,  # Much higher probability above threshold
                             prop_missing)      # Base probability below threshold
      na_indices <- which(runif(n) < prob_missing)
      
    }
  
  return(data_with_missing)
}