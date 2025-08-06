#' Calculate Relative Differences Between Sample and Frame Proportions
#'
#' @param variable Character specifying the grouping variable (e.g., "geo1", "geo2", "urbrur")
#' @param sampling_method Character specifying sampling method ("srswor", "syswor", "poisson")
#' @param frame_data data.table containing the full frame data
#' @param n_values Vector of sample sizes to evaluate
#' @param num_samples Number of samples to draw for each sample size
#' @param freq_frame_data data.table containing frame frequencies by variable
#' @param pik Inclusion or selection probabilities for the sampling method
#'
#' @return A data.table with relative differences between sample and frame proportions
#' @export
#'
#' @examples
#' \dontrun{
#' result <- calculate_relative_diff(
#'   variable = "urbrur",
#'   sampling_method = "syswor",
#'   frame_data = frame_household_perfect.dt,
#'   n_values = c(100, 200, 300),
#'   num_samples = 50,
#'   freq_frame_data = total_household_urbrur.dt
#' )
#' }
calculate_relative_diff <- function(variable, sampling_method, 
                                    frame_data, n_values, 
                                    num_samples, freq_frame_data, pik) {
  
  # Validate inputs
  stopifnot(
    is.character(variable),
    variable %in% names(frame_data),
    is.character(sampling_method),
    is.data.table(frame_data),
    is.data.table(freq_frame_data),
    variable %in% names(freq_frame_data),
    is.numeric(n_values),
    is.numeric(num_samples),
    num_samples > 0
  )
  
  # Get levels from frame data
  var_levels <- sort(as.integer(levels(frame_data[[variable]])))
  
  # Initialize results data.table
  result_dt <- data.table(
    var = factor(character(0), levels = var_levels),
    n = integer(0),
    iter = integer(0),
    diff_rel = numeric(0)
  )
  setnames(result_dt, "var", variable)
  
  # Calculate population size
  N <- nrow(frame_data)
  
  # Main sampling loop
  for (n in n_values) {
    cat(paste0("n= ", n, '...'))
    
    
    for (i in 1:num_samples) {
      # Apply sampling method
      if (sampling_method == "syswor") {
        pik <- rep(n / N, times = N)
        frame_data[, s := sampling::UPsystematic(pik)]
      } else if (sampling_method == "srswor") {
        frame_data[, s := sampling::srswor(n, N)]
      } else if (sampling_method == "poisson") {
        pik <- pik
        frame_data[, s:= sampling::UPpoisson(pik)] 
      } # Add other methods as needed
      
      # Calculate sample totals by variable
      sample_totals <- frame_data[
        , .(total_sample = sum(s)), by = variable][
          order(get(variable))]
      
      # Merge with frame totals and calculate differences
      aux_dt <- merge(
        freq_frame_data, sample_totals, by = variable)[
        , auxvar := total_sample / sum(total_sample)][
        , diff_rel := (auxvar - freq_frame) / freq_frame][
        , n := n][
        , iter := i][
        , .SD, .SDcols = c(variable, "n", "iter", "diff_rel")]
      
      result_dt <- rbindlist(list(result_dt, aux_dt))
    }
    cat(" ok.\n")
  }
  
  return(result_dt)
}