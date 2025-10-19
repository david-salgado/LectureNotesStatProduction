#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#                                                                              #
#                                                                              #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# Load packages ####
library(here)
library(sampling)
library(data.table)
library(ggplot2)
library(viridis)
library(hexbin)
library(GGally)
library(ggrain)


# Set relative paths ####
path_project <- here()
path_HBS_grTruth <- file.path(path_project, 'data', 'HBS', 'ground_truth')
path_HBS_samples <- file.path(path_project, 'data', 'HBS', 'samples')
path_HBS_estimators <- file.path(path_project, 'data', 'estimators', 'HBS')
path_src <- file.path(path_project, 'src')

# Load src functions ####
source(file.path(path_src, "generate_missing_values.R"))

# Set absolute filenames ####
data_HBS_household_grTruth_fn  <- 'data_HBS2023_household_grTruth.csv'
data_HBS_household_grTruth_fn  <- file.path(
  path_HBS_grTruth, data_HBS_household_grTruth_fn)

target_estim_nonResponse_imputation_HBS_fn <- file.path(
  path_HBS_estimators, "target_estim_nonResponse_imputation_HBS.dt.csv"
)

# Set parameters ####
sampling_fraction <- 0.05 
n_iter <- 1000

## Study variables ####
frame_vars      <- c("hid", "geo1", "geo2", "ea", "urbrur", "hhsize")
target_vars <- c("exp_01")
aux_vars <- c("tot_exp", "pc_exp")
vars_classes <- 
  c('character', 'factor', 'factor', 'numeric', 'factor', 'integer',
    rep('numeric', length(target_vars)),
    rep('numeric', length(aux_vars))
  )
names(vars_classes) <- c(frame_vars, target_vars, aux_vars)
size <- "hhsize"

## Missing mechanism ####
MAR_target_var     <- target_vars
MAR_aux_vars       <- c("geo1", "urbrur", "hhsize")
MAR_prop_missing   <- rnorm(length(MAR_target_var), 0.3, 0.02)
MAR_mechanism_type <- "logistic"
MAR_mechanism_args <- list(interaction = TRUE)

# Read ground truth data ####
microdata_HBS_household_grTruth.dt <- fread(
  data_HBS_household_grTruth_fn, sep = ";")[
    , c(frame_vars, target_vars, aux_vars), with = FALSE]

for (i in seq_along(vars_classes)) {
  col <- names(microdata_HBS_household_grTruth.dt)[i]
  clase <- vars_classes[i]
  set(microdata_HBS_household_grTruth.dt, j = col, value = match.fun(paste0("as.", clase))(microdata_HBS_household_grTruth.dt[[col]]))
}

# Calculate household first-order inclusion probabilities ####
N <- nrow(microdata_HBS_household_grTruth.dt)
n <- round(sampling_fraction * N)
microdata_HBS_household_grTruth.dt[
  , pik := inclusionprobabilities(get(size), n)]

# Calculate estimates ####
aux_vars_population <- unlist(microdata_HBS_household_grTruth.dt[
  , lapply(.SD, sum), .SDcols = aux_vars])
aux_vars_population <- c(aux_vars_population, N)
names(aux_vars_population) <- c(aux_vars, "N")
microdata_HBS_household_grTruth.dt[, N := 1]


target_estim_nonResponse.lst <- lapply(1:n_iter, function(i){
  
  cat(paste0('iter= ', i, '...'))
  microdata_HBS_household_grTruth.dt[
    , s:= UPrandompivotal(pik)]
  
  # Generate missing values
  microdata_raw.dt <- generate_missing_values(
    complete_data.dt = microdata_HBS_household_grTruth.dt[s == 1],
    target_vars      = MAR_target_var,
    auxiliary_vars   = MAR_aux_vars,
    prop_missing     = MAR_prop_missing,
    nonresponse      = 'partial',
    mechanism        = 'MAR',  
    mechanism_type   = MAR_mechanism_type,
    mechanism_args   = MAR_mechanism_args
  )[, r := ifelse(s == 1 & !is.na(get(target_vars)), 1, 0)]
  
  microdata_HBS_household_grTruth.dt[
    s == 1, r := microdata_raw.dt$r][
    s == 0, r := 0]
  
  form <- paste(target_vars, paste0(c(aux_vars[1], size), collapse = " + "), sep = " ~ ")
  imputation_model <- lm(as.formula(form), microdata_HBS_household_grTruth.dt[r == 1])
  microdata_HBS_household_grTruth.dt[
    , target_imputed := predict(imputation_model, microdata_HBS_household_grTruth.dt)][
    , target_synthetic := ifelse(r == 1, get(target_vars), target_imputed)]
  
  # HT  
  target_yHT <- microdata_HBS_household_grTruth.dt[
    s == 1, HTestimator(target_synthetic, pik)]
  
  # Ratio - x1
  x1_HT <- microdata_HBS_household_grTruth.dt[
    s == 1, HTestimator(get(aux_vars[1]), pik)]
  target_yRat_x1 <- target_yHT / x1_HT * aux_vars_population[1]
  
  # Ratio - x2
  x2_HT <- microdata_HBS_household_grTruth.dt[
    s == 1, HTestimator(get(aux_vars[2]), pik)]
  target_yRat_x2 <- target_yHT / x2_HT * aux_vars_population[2]
  
  # GREG - x1
  N_HT <- microdata_HBS_household_grTruth.dt[
    s == 1, HTestimator(rep(1, .N), pik)]
  x1sq_HT <- microdata_HBS_household_grTruth.dt[
    s == 1, HTestimator(get(aux_vars[1]) * get(aux_vars[1]), pik)]
  x1y_HT <- microdata_HBS_household_grTruth.dt[
    s == 1, HTestimator(get(aux_vars[1]) * target_synthetic, pik)]
  matT_x1 <- matrix(c(N_HT, x1_HT, x1_HT, x1sq_HT), ncol = 2)
  beta_x1 <- solve(matT_x1, tol = 1e-25) %*% c(target_yHT, x1y_HT)
  microdata_HBS_household_grTruth.dt[
    , target_pred  := beta_x1[1] + beta_x1[2] * get(aux_vars[1])][
    , target_resid := target_synthetic - target_pred]
  target_yGREG_x1 <- 
    microdata_HBS_household_grTruth.dt[s == 1, HTestimator(target_resid, pik)] + 
    microdata_HBS_household_grTruth.dt[, sum(target_pred)]
  
  # GREG - x2
  x2sq_HT <- microdata_HBS_household_grTruth.dt[
    s == 1, HTestimator(get(aux_vars[2]) * get(aux_vars[2]), pik)]
  x2y_HT <- microdata_HBS_household_grTruth.dt[
    s == 1, HTestimator(get(aux_vars[2]) * target_synthetic, pik)]
  matT_x2 <- matrix(c(N_HT, x2_HT, x2_HT, x2sq_HT), ncol = 2)
  beta_x2 <- solve(matT_x2, tol = 1e-25) %*% c(target_yHT, x2y_HT)
  microdata_HBS_household_grTruth.dt[
    , target_pred  := beta_x2[1] + beta_x2[2] * get(aux_vars[2])][
    , target_resid := target_imputed - target_pred]
  target_yGREG_x2 <- 
    microdata_HBS_household_grTruth.dt[s == 1, HTestimator(target_resid, pik)] + 
    microdata_HBS_household_grTruth.dt[, sum(target_pred)]
  
  # GREG - x1x2
  x1x2_HT <- microdata_HBS_household_grTruth.dt[
    s == 1, HTestimator(get(aux_vars[1]) * get(aux_vars[2]), pik)]
  
  matT_x1x2 <- matrix(c(
    N_HT,  x1_HT,   x2_HT, 
    x1_HT, x1sq_HT, x1x2_HT,
    x2_HT, x1x2_HT, x2sq_HT), ncol = 3)
  beta_x1x2 <- solve(matT_x1x2, tol = 1e-25) %*% c(target_yHT, x1y_HT, x2y_HT)
  microdata_HBS_household_grTruth.dt[
    , target_pred  := beta_x1x2[1] + beta_x1x2[2] * get(aux_vars[1]) + beta_x1x2[3] * get(aux_vars[2])][
      , target_resid := target_synthetic - target_pred]
  target_yGREG_x1x2 <- 
    microdata_HBS_household_grTruth.dt[s == 1, HTestimator(target_resid, pik)] + 
    microdata_HBS_household_grTruth.dt[, sum(target_pred)]
  
  microdata_HBS_household_grTruth.dt[
    , target_pred := NULL][
    , target_resid := NULL]
  
  # calib - linear - x1
  pik_sample <- microdata_HBS_household_grTruth.dt[s == 1, pik] 
  aux_vars_calib <- c(aux_vars[1], "N")
  gCalib <-  calib(
    Xs = microdata_HBS_household_grTruth.dt[s == 1, ..aux_vars_calib], 
    d  = 1/pik_sample, 
    total = aux_vars_population[c(1, 3)], method = 'linear')
  wCalib <- gCalib / pik_sample
  target_yCalib_x1 <- sum(microdata_HBS_household_grTruth.dt[s == 1, target_synthetic] * wCalib)
  
  # calib - linear - x2
  pik_sample <- microdata_HBS_household_grTruth.dt[s == 1, pik] 
  aux_vars_calib <- c(aux_vars[2], "N")
  gCalib <-  calib(
    Xs = microdata_HBS_household_grTruth.dt[s == 1, ..aux_vars_calib], 
    d  = 1/pik_sample, 
    total = aux_vars_population[c(2, 3)], method = 'linear')
  wCalib <- gCalib / pik_sample
  target_yCalib_x2 <- sum(microdata_HBS_household_grTruth.dt[s == 1, target_synthetic] * wCalib)
  
  # calib - linear - x1x2
  pik_sample <- microdata_HBS_household_grTruth.dt[s == 1, pik] 
  aux_vars_calib <- c(aux_vars, "N")
  gCalib <-  calib(
    Xs = microdata_HBS_household_grTruth.dt[s == 1, ..aux_vars_calib], 
    d  = 1/pik_sample, 
    total = aux_vars_population, method = 'linear')
  wCalib <- gCalib / pik_sample
  target_yCalib_x1x2 <- sum(microdata_HBS_household_grTruth.dt[s == 1, target_synthetic] * wCalib)
  
  cat(paste0('ok.\n'))
  
  output <- c(HT = target_yHT,
              Ratio_x1 = target_yRat_x1, 
              Ratio_x2 = target_yRat_x2, 
              GREG_x1 = target_yGREG_x1, 
              GREG_x2 = target_yGREG_x2, 
              GREG_x1x2 = target_yGREG_x1x2,
              calib_x1 = target_yCalib_x1,
              calib_x2 = target_yCalib_x2,
              calib_x1x2 = target_yCalib_x1x2)
  return(output)
})


target_estim_nonResponse.dt <- as.data.table(Reduce(rbind, target_estim_nonResponse.lst))[
  , sample := 1:n_iter]

fwrite(
  target_estim_nonResponse.dt, 
  file = target_estim_nonResponse_imputation_HBS_fn , 
  sep = ";"
)

# Plot estimates ####
target_population <- microdata_HBS_household_grTruth.dt[, sum(get(target_vars))]
target_estim_nonResponse_long.dt <- melt(target_estim_nonResponse.dt, id.vars = 'sample')
target_estim_nonResponse_long.dt[
  , relError := (value - target_population) / target_population]

ggplot(target_estim_nonResponse_long.dt, aes(x = variable, y = relError * 100, fill = variable)) +
  ggrain::geom_rain(
    alpha = 0.3,
    #rain.side = "f1x1",  # 'f1x1', 'r1x1', 'f1x2', etc.
    point.args = list(size = 1.5, alpha = 0.5),
    boxplot.args = list(width = 0.1, alpha = 0.8),
    violin.args = list(scale = "width", alpha = 0.4)
  ) +
  geom_hline(yintercept = 0, color = "red") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "",
       x = "",
       y = "Relative Error (%)") +
  theme_minimal() +
  theme(legend.position = 'top', legend.title = element_blank())

data.dt <- target_estim_nonResponse_long.dt[
  variable %chin% c('Ratio_x1', 'GREG_x1','GREG_x1x2', 'calib_x1', 'calib_x1x2')]
ggplot(data.dt, aes(x = variable, y = relError * 100, fill = variable)) +
  ggrain::geom_rain(
    alpha = 0.3,
    #rain.side = "f1x1",  # 'f1x1', 'r1x1', 'f1x2', etc.
    point.args = list(size = 1.5, alpha = 0.5),
    boxplot.args = list(width = 0.1, alpha = 0.8),
    violin.args = list(scale = "width", alpha = 0.4)
  ) +
  geom_hline(yintercept = 0, color = "red") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "",
       x = "",
       y = "Relative Error (%)") +
  theme_minimal() +
  theme(legend.position = 'top', legend.title = element_blank())
