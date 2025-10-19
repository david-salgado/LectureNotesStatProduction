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
library(naniar)
library(skimr)
library(hexbin)
library(GGally)
library(ggrain)


# Set relative paths ####
path_project <- here()
path_HBS_grTruth    <- file.path(path_project, 'data', 'HBS', 'ground_truth')
path_HBS_samples    <- file.path(path_project, 'data', 'HBS', 'samples')
path_HBS_estimators <- file.path(path_project, 'data', 'estimators', 'HBS')
path_src <- file.path(path_project, 'src')

# Set absolute filenames ####
data_HBS_household_grTruth_fn  <- 'data_HBS2023_household_grTruth.csv'
data_HBS_household_grTruth_fn  <- file.path(
  path_HBS_grTruth, data_HBS_household_grTruth_fn
)

microdata_HBS_household_sample_fn <- "microdata_HBS_household_sample.csv"
microdata_HBS_household_sample_fn <- file.path(
  path_HBS_samples, microdata_HBS_household_sample_fn
)

microdata_HBS_household_raw_fn <- "microdata_HBS_household_raw.csv"
microdata_HBS_household_raw_fn <- file.path(path_HBS_samples, microdata_HBS_household_raw_fn)

target_estim_fullResp_HBS_fn <- file.path(
  path_HBS_estimators, "target_estim_fullResp_HBS.dt.csv"
)

# Set parameters ####
sampling_fraction_household <- 0.01 

n_iter <- 1000


## Household variables ####
frame_vars   <- c("hid", "geo1", "geo2", "ea", "urbrur", "hhsize")
target_vars  <- c("exp_01")
aux_vars     <- c("tot_exp", "pc_exp")
vars_classes <- 
  c('character', 'factor', 'factor', 'numeric', 'factor', 'integer',
  rep('numeric', length(target_vars)),
  rep('numeric', length(aux_vars))
)
names(vars_classes) <- c(frame_vars, target_vars, aux_vars)


# Read HBS ground truth data at household level ####
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
n <- round(sampling_fraction_household * N)
microdata_HBS_household_grTruth.dt[
  , pik := inclusionprobabilities(hhsize, n)]

# Plot variable density and correlation ####
study_vars <- c(target_vars, aux_vars, 'pik')

hex_plot <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_hex(bins = 135, color = NA, alpha = 0.9) +
    scale_fill_viridis_c(option = "plasma",
                         trans = "log10",
                         name = "Count\n(log10)") +
    theme_bw()
}

density_plot <- function(data, mapping, ...){
  ggplot(data, mapping, ...) +
  geom_density(aes(y = after_stat(scaled)), fill = "skyblue") + 
  geom_boxplot(aes(y = -0.1), width = 0.03) +
  scale_y_continuous(
    name = "Density (scaled)\n",
    breaks = c(-0.1, seq(0, 1, 0.2)),
    labels = c("", seq(0, 1, 0.2))) +
    theme_bw()
}

ggpairs(microdata_HBS_household_grTruth.dt[, ..study_vars],
        lower = list(continuous = hex_plot),
        diag = list(continuous = density_plot),
        upper = list(continuous = "cor"))  +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Calculate estimates ####
aux_vars_population <- c()
for (aux_var in aux_vars){
  aux_vars_population <- c(
    aux_vars_population, microdata_HBS_household_grTruth.dt[, sum(get(aux_var))]
  )
}
names(aux_vars_population) <- aux_vars
aux_vars_population <- c(aux_vars_population, N)
names(aux_vars_population) <- c(aux_vars, "N")
microdata_HBS_household_grTruth.dt[, N := 1]

target_estim_fullResponse.lst <- lapply(1:n_iter, function(i){
  
  cat(paste0('iter= ', i, '...'))
  microdata_HBS_household_grTruth.dt[
    , s:= UPrandompivotal(pik)]
  
  # HT  
  target_yHT <- microdata_HBS_household_grTruth.dt[
    s == 1, HTestimator(get(target_vars), pik)]
  
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
    s == 1, HTestimator(get(aux_vars[1]) * get(target_vars[1]), pik)]
  matT_x1 <- matrix(c(N_HT, x1_HT, x1_HT, x1sq_HT), ncol = 2)
  beta_x1 <- solve(matT_x1) %*% c(target_yHT, x1y_HT)
  microdata_HBS_household_grTruth.dt[
    , target_pred  := beta_x1[1] + beta_x1[2] * get(aux_vars[1])][
      , target_resid := get(target_vars) - target_pred]
  target_yGREG_x1 <- 
    microdata_HBS_household_grTruth.dt[s == 1, HTestimator(target_resid, pik)] + 
    microdata_HBS_household_grTruth.dt[, sum(target_pred)]
  
  # GREG - x2
  x2sq_HT <- microdata_HBS_household_grTruth.dt[
    s == 1, HTestimator(get(aux_vars[2]) * get(aux_vars[2]), pik)]
  x2y_HT <- microdata_HBS_household_grTruth.dt[
    s == 1, HTestimator(get(aux_vars[2]) * get(target_vars), pik)]
  matT_x2 <- matrix(c(N_HT, x2_HT, x2_HT, x2sq_HT), ncol = 2)
  beta_x2 <- solve(matT_x2) %*% c(target_yHT, x2y_HT)
  microdata_HBS_household_grTruth.dt[
    , target_pred  := beta_x2[1] + beta_x2[2] * get(aux_vars[2])][
      , target_resid := get(target_vars) - target_pred]
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
  beta_x1x2 <- solve(matT_x1x2) %*% c(target_yHT, x1y_HT, x2y_HT)
  microdata_HBS_household_grTruth.dt[
    , target_pred  := beta_x1x2[1] + beta_x1x2[2] * get(aux_vars[1]) + beta_x1x2[3] * get(aux_vars[2])][
      , target_resid := get(target_vars) - target_pred]
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
  target_yCalib_x1 <- sum(microdata_HBS_household_grTruth.dt[s == 1, get(target_vars)] * wCalib)
  
  # calib - linear - x2
  pik_sample <- microdata_HBS_household_grTruth.dt[s == 1, pik] 
  aux_vars_calib <- c(aux_vars[2], "N")
  gCalib <-  calib(
    Xs = microdata_HBS_household_grTruth.dt[s == 1, ..aux_vars_calib], 
    d  = 1/pik_sample, 
    total = aux_vars_population[c(2, 3)], method = 'linear')
  wCalib <- gCalib / pik_sample
  target_yCalib_x2 <- sum(microdata_HBS_household_grTruth.dt[s == 1, get(target_vars)] * wCalib)
  
  # calib - linear - x1x2
  pik_sample <- microdata_HBS_household_grTruth.dt[s == 1, pik] 
  aux_vars_calib <- c(aux_vars, "N")
  gCalib <-  calib(
    Xs = microdata_HBS_household_grTruth.dt[s == 1, ..aux_vars_calib], 
    d  = 1/pik_sample, 
    total = aux_vars_population, method = 'linear')
  wCalib <- gCalib / pik_sample
  target_yCalib_x1x2 <- sum(microdata_HBS_household_grTruth.dt[s == 1, get(target_vars)] * wCalib)
  
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

target_estim_fullResponse.dt <- as.data.table(Reduce(rbind, target_estim_fullResponse.lst))[
  , sample := 1:n_iter]

fwrite(
  target_estim_fullResponse.dt, file = target_estim_fullResp_HBS_fn, sep = ";"
)

# Plot estimates ####
target_population <- microdata_HBS_household_grTruth.dt[, sum(get(target_vars))]
target_estim_fullResponse_long.dt <- melt(target_estim_fullResponse.dt, id.vars = 'sample')
target_estim_fullResponse_long.dt[
  , relError := (value - target_population) / target_population]

ggplot(target_estim_fullResponse_long.dt, aes(x = variable, y = relError * 100, fill = variable)) +
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

data.dt <- target_estim_fullResponse_long.dt[
  variable %chin% c('HT', 'Ratio_x1', 'GREG_x1', 'GREG_x1x2', 'calib_x1', 'calib_x1x2')
]
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

