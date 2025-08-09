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

# Set relative paths ####
path_project <- here()
path_HBS_grTruth <- file.path(path_project, 'data', 'HBS', 'ground_truth')
path_samples <- file.path(path_project, 'data', 'samples')
path_src <- file.path(path_project, 'src')

# Load src functions ####
source(file.path(path_src, "plot_density_boxplot.R"))
source(file.path(path_src, "plot_violin_box_series.R"))
source(file.path(path_src, "plot_violin_box_HTestimate.R"))

# Set absolute filenames ####
data_HBS_grTruth_fn  <- 'data_HBS2023_household_grTruth.csv'
data_HBS_grTruth_fn  <- file.path(path_HBS_grTruth, data_HBS_grTruth_fn)

estim_HT_fn  <- 'estim_HT.csv'
estim_HT_fn  <- file.path(path_samples, estim_HT_fn)

# Set parameters ####
sampling_fraction <- 0.01

n_iter <- 1000

# Read HBS ground truth data ####
data_HBS_grTruth.dt <- fread(
  data_HBS_grTruth_fn, sep = ";")

# Calculate first-order inclusion probabilities ####
n <- round(sampling_fraction * nrow(data_HBS_grTruth.dt))
data_HBS_grTruth.dt[
  , pik := inclusionprobabilities(hhsize, n)]

# Plot target variable density ####
## Total expenditure ####
plot_density_boxplot(
  data = data_HBS_grTruth.dt, 
  x = "tot_exp",
  xlab = "\nTotal household expenditure (tot_exp)",
  title = "Distribution of target variable (tot_exp)",
  subtitle = "")

## Food and beverage expenditure ####
plot_density_boxplot(
  data = data_HBS_grTruth.dt, 
  x = "exp_01",
  xlab = "\nFood and beverage household expenditure (exp_01)",
  title = "Distribution of target variable (exp_01)",
  subtitle = "")

## Transport expenditure ####
plot_density_boxplot(
  data = data_HBS_grTruth.dt, 
  x = "exp_07",
  xlab = "\nTransport household expenditure (exp_07)",
  title = "Distribution of target variable (exp_07)",
  subtitle = "")

## Education expenditure ####
plot_density_boxplot(
  data = data_HBS_grTruth.dt, 
  x = "exp_10",
  xlab = "\nEducation household expenditure (exp_10)",
  title = "Distribution of target variable (exp_10)",
  subtitle = "")

# Plot target variable vs frame variable (size) ####
## Total expenditure ####
plot_violin_box_series(
   data = data_HBS_grTruth.dt,
   x_var = "hhsize",
   y_var = "tot_exp",
   xlab = "Household size (hhsize)",
   ylab = "Total household expenditure (tot_exp)"
)

## Food and beverage expenditure ####
plot_violin_box_series(
  data = data_HBS_grTruth.dt,
  x_var = "hhsize",
  y_var = "exp_01",
  xlab = "Household size (hhsize)",
  ylab = "Food and beverage household expenditure (exp_01)"
)

## Transport expenditure ####
plot_violin_box_series(
  data = data_HBS_grTruth.dt,
  x_var = "hhsize",
  y_var = "exp_07",
  xlab = "Household size (hhsize)",
  ylab = "Transport household expenditure (exp_07)"
)

## Education expenditure ####
plot_violin_box_series(
  data = data_HBS_grTruth.dt,
  x_var = "hhsize",
  y_var = "exp_10",
  xlab = "Household size (hhsize)",
  ylab = "Education household expenditure (exp_10)"
)

# Calculate HT estimates for all samples ####
## Total expenditure ####
totexp_estim_HT <- sapply(1:n_iter, function(i){
  
  cat(paste0('iter= ', i, '...'))
  output <- data_HBS_grTruth.dt[
    , s:= UPrandompivotal(pik)][
      s == 1, HTestimator(tot_exp, pik)]
  cat(paste0('ok.\n'))
  return(output)
})

## Food and beverage expenditure ####
exp01_estim_HT <- sapply(1:n_iter, function(i){
  
  cat(paste0('iter= ', i, '...'))
  output <- data_HBS_grTruth.dt[
    , s:= UPrandompivotal(pik)][
    s == 1, HTestimator(exp_01, pik)]
  cat(paste0('ok.\n'))
  return(output)
})

## Transport expenditure ####
exp07_estim_HT <- sapply(1:n_iter, function(i){
  
  cat(paste0('iter= ', i, '...'))
  output <- data_HBS_grTruth.dt[
    , s:= UPrandompivotal(pik)][
      s == 1, HTestimator(exp_07, pik)]
  cat(paste0('ok.\n'))
  return(output)
})

## Education expenditure ####
exp10_estim_HT <- sapply(1:n_iter, function(i){
  
  cat(paste0('iter= ', i, '...'))
  output <- data_HBS_grTruth.dt[
    , s:= UPrandompivotal(pik)][
      s == 1, HTestimator(exp_10, pik)]
  cat(paste0('ok.\n'))
  return(output)
})


estim_HT.dt <- data.table(
  iter = 1:n_iter, 
  tot_exp= totexp_estim_HT,
  exp_01 = exp01_estim_HT,
  exp_07 = exp07_estim_HT,
  exp_10 = exp10_estim_HT)

fwrite(estim_HT.dt, file = estim_HT_fn, sep = ";")

# Plot HT estimates ####
## Total expenditure ####
plot_violin_box_HTestimate(
  data = estim_HT.dt,
  target_var = "tot_exp",
  true_total = sum(data_HBS_grTruth.dt$tot_exp),
  title = "Distribution of HT estimates",
  subtitle = "Target variable (tot_exp)",
  xlab = bquote("HT estimates of total household expenditure ("*10^9*")"),
  scale_factor = 1e9
)

## Food and beverages expenditure ####
plot_violin_box_HTestimate(
 data = estim_HT.dt,
 target_var = "exp_01",
 true_total = sum(data_HBS_grTruth.dt$exp_01),
 title = "Distribution of HT estimates",
 subtitle = "Target variable (exp_01)",
 xlab = bquote("HT estimates of food and beverages household expenditure ("*10^9*")"),
 scale_factor = 1e9
)
 
## Transport expenditure ####
plot_violin_box_HTestimate(
  data = estim_HT.dt,
  target_var = "exp_07",
  true_total = sum(data_HBS_grTruth.dt$exp_07),
  title = "Distribution of HT estimates",
  subtitle = "Target variable (exp_07)",
  xlab = bquote("HT estimates of food and beverages household expenditure ("*10^9*")"),
  scale_factor = 1e9
)

## Education expenditure ####
plot_violin_box_HTestimate(
  data = estim_HT.dt,
  target_var = "exp_10",
  true_total = sum(data_HBS_grTruth.dt$exp_10),
  title = "Distribution of HT estimates",
  subtitle = "Target variable (exp_10)",
  xlab = bquote("HT estimates of food and beverages household expenditure ("*10^9*")"),
  scale_factor = 1e9
)
