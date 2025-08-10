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

# Set relative paths ####
path_project <- here()
path_HBS_grTruth <- file.path(path_project, 'data', 'HBS', 'ground_truth')
path_samples <- file.path(path_project, 'data', 'samples')
path_src <- file.path(path_project, 'src')

# Load src functions ####
source(file.path(path_src, "generate_missing_values.R"))

# Set absolute filenames ####
data_HBS_household_grTruth_fn  <- 'data_HBS2023_household_grTruth.csv'
data_HBS_household_grTruth_fn  <- file.path(
  path_HBS_grTruth, data_HBS_household_grTruth_fn)

data_HBS_individual_grTruth_fn  <- 'data_HBS2023_individual_grTruth.csv'
data_HBS_individual_grTruth_fn  <- file.path(
  path_HBS_grTruth, data_HBS_individual_grTruth_fn)

microdata_HBS_household_raw_fn <- "microdata_HBS_household_raw.csv"
microdata_HBS_household_raw_fn <- file.path(path_samples, microdata_HBS_household_raw_fn)

microdata_HBS_individual_raw_fn <- "microdata_HBS_individual_raw.csv"
microdata_HBS_individual_raw_fn <- file.path(path_samples, microdata_HBS_individual_raw_fn)


# Set parameters ####
## Sampling fractions ####
sampling_fraction_district <- 12/61 # At least one district (total 61) per province (geo1 10) + 2 of margin
sampling_fraction_houselhold <- 0.01 # Same for all districts

## Household variables ####
frame_vars_household      <- c("hid", "geo1", "geo2", "ea", "urbrur", "hhsize")
target_vars_household.lst <- list(
  dwelling    = c("statocc", "rooms", "bedrooms", "floor", "walls", "roof"),
  water       = c("water", "toilet"),
  energy      = c("electricity", "cook_fuel"),
  assets      = c("car", "bicycle", "motorcycle", "refrigerator", "tv", "radio", "bank"),
  expenditure = c("exp_01", "exp_02", "exp_03", "exp_04", "exp_05", "exp_06", 
                  "exp_07", "exp_08", "exp_09", "exp_10", "exp_11", "exp_12")
)
vars_classes_household <- c('character', 'factor', 'factor', 'numeric', 'factor', 'integer',
                  'factor', 'integer', 'integer', 'factor', 'factor', 'factor',
                  'factor', 'factor',
                  'factor', 'factor',
                  rep('factor', length(target_vars_household.lst$assets)),
                  rep('numeric', length(target_vars_household.lst$expenditure)))
names(vars_classes_household) <- c(frame_vars_household, unlist(target_vars_household.lst))

## Individual variables ####
frame_vars_individual      <- c("hid", "idno")
target_vars_individual.lst <- list(
  demographics = c("relation", "sex", "age", "age_month"),
  employment   = c("occupation", "industry"),
  migration    = c("migrate_recent"),
  disabilities = c("disability")
)
vars_classes_individual <- c('character', 'factor',
                            'factor', 'factor', 'integer', 'integer',
                            'factor', 'factor',
                            'factor',
                            'factor')
names(vars_classes_individual) <- c(frame_vars_individual, unlist(target_vars_individual.lst))

## Missing mechanism - household ####
MAR_group1_target_vars_household    <- unlist(
  target_vars_household.lst[c('dwelling', 'water', 'energy', 'assets')]
)
MAR_group1_aux_vars_household       <- c("geo1", "urbrur")
MAR_group1_prop_missing_household   <- rnorm(
  length(MAR_group1_target_vars_household), 0.1, 0.02
)
MAR_group1_mechanism_type_household <- "logistic"
MAR_group1_mechanism_args_household <- list(interaction = TRUE)

MAR_group2_target_vars_household    <- unlist(target_vars_household.lst['expenditure'])
MAR_group2_aux_vars_household       <- c("geo1", "urbrur", "hhsize")
MAR_group2_prop_missing_household   <- rnorm(length(MAR_group2_target_vars_household), 0.5, 0.1)
MAR_group2_mechanism_type_household <- "randomforest"
MAR_group2_mechanism_args_household <- list(
  num.trees = 50,
  mtry = floor(sqrt(length(MAR_group2_aux_vars_household))),
  importance = "none",
  probability = TRUE,
  classification = TRUE
)

## Missing mechanism - individual ####
MCAR_group1_target_vars_individual    <- unlist(
  target_vars_individual.lst["demographics"]
)
MCAR_group1_prop_missing_individual   <- rnorm(
  length(MAR_group1_target_vars_individual), 0.02, 0.001
)

MAR_group2_target_vars_individual    <- unlist(
  target_vars_individual.lst[c("employment", "migration", "disabilities")]
)
MAR_group2_aux_vars_individual       <- c("sex", "age")
MAR_group2_prop_missing_individual   <- rnorm(length(MAR_group2_target_vars_individual), 0.1, 0.05)
MAR_group2_mechanism_type_individual <- "randomforest"
MAR_group2_mechanism_args_individual <- list(
  num.trees = 50,
  mtry = floor(sqrt(length(MAR_group2_aux_vars_individual))),
  importance = "none",
  probability = TRUE,
  classification = TRUE
)

# Read HBS ground truth data at household level ####
## Household ####
microdata_HBS_household_grTruth.dt <- fread(
  data_HBS_household_grTruth_fn, sep = ";")[
  , c(frame_vars_household, unlist(target_vars_household.lst)), with = FALSE]

for (i in seq_along(vars_classes_household)) {
  col <- names(microdata_HBS_household_grTruth.dt)[i]
  clase <- vars_classes_household[i]
  set(microdata_HBS_household_grTruth.dt, j = col, value = match.fun(paste0("as.", clase))(microdata_HBS_household_grTruth.dt[[col]]))
}

## Individual ####
microdata_HBS_individual_grTruth.dt <- fread(
  data_HBS_individual_grTruth_fn, sep = ";")[
  , c(frame_vars_individual, unlist(target_vars_individual.lst)), with = FALSE]

for (i in seq_along(vars_classes_individual)) {
  col <- names(microdata_HBS_individual_grTruth.dt)[i]
  clase <- vars_classes_individual[i]
  set(microdata_HBS_individual_grTruth.dt, j = col, value = match.fun(paste0("as.", clase))(microdata_HBS_individual_grTruth.dt[[col]]))
}

# Calculate household first-order inclusion probabilities ####
NI <- length(unique(microdata_HBS_household_grTruth.dt$geo2))
nI <- round(sampling_fraction_district * NI)
microdata_HBS_household_grTruth.dt[
  , NIi := .N, by = "geo2"]

microdata_HBS_district.dt <- microdata_HBS_household_grTruth.dt[
  , c("geo2", "NIi"), with = FALSE]

microdata_HBS_district.dt <- microdata_HBS_district.dt[
  !duplicated(microdata_HBS_district.dt, by = 'geo2')][
  , piIi := inclusionprobabilities(NIi, nI)]

microdata_HBS_household_sample.dt <- microdata_HBS_household_grTruth.dt[
  microdata_HBS_district.dt, on = c('geo2', 'NIi')]

# Select district and household samples ####
microdata_HBS_district_sample.dt <- microdata_HBS_district.dt[
  , sI := UPrandompivotal(piIi)][
  sI == 1]

microdata_HBS_household_sample.dt <- microdata_HBS_household_sample.dt[
  microdata_HBS_district_sample.dt, on = c('geo2', 'NIi', 'piIi')][
  , nII := round(sampling_fraction_houselhold * NIi)]

microdata_HBS_district_sample.dt <- microdata_HBS_household_sample.dt[
  , c('geo2', 'nII', 'NIi'), with = FALSE]
microdata_HBS_district_sample.dt <- microdata_HBS_district_sample.dt[
  !duplicated(microdata_HBS_district_sample.dt, by = 'geo2')]

district_sample.dt <- microdata_HBS_district_sample.dt[
  , .(sII = srswor(nII, NIi)), by = "geo2"]
microdata_HBS_household_sample.dt <- microdata_HBS_household_sample.dt[
  , sII := district_sample.dt$sII][
  sII == 1]

microdata_HBS_individual_sample.dt <- microdata_HBS_individual_grTruth.dt[
  hid %chin% microdata_HBS_household_sample.dt$hid]

# Generate missing values ####
## Household ####
### Group 1 variables ####
microdata_HBS_household_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_household_sample.dt,
  target_vars      = MAR_group1_target_vars_household,
  auxiliary_vars   = MAR_group1_aux_vars_household,
  prop_missing     = MAR_group1_prop_missing_household,
  mechanism        = 'MAR',  
  mechanism_type   = MAR_group1_mechanism_type_household,
  mechanism_args   = MAR_group1_mechanism_args_household
)

### Group 2 variables ####
microdata_HBS_household_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_household_raw.dt,
  target_vars      = MAR_group2_target_vars_household,
  auxiliary_vars   = MAR_group2_aux_vars_household,
  prop_missing     = MAR_group2_prop_missing_household,
  mechanism        = 'MAR',  
  mechanism_type   = MAR_group2_mechanism_type_household,
  mechanism_args   = MAR_group2_mechanism_args_household
)

## Individual ####
### Group 1 variables ####
microdata_HBS_individual_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_individual_sample.dt,
  target_vars      = MCAR_group1_target_vars_individual,
  prop_missing     = MCAR_group1_prop_missing_individual,
  mechanism        = 'MCAR'
)

### Group 2 variables ####
microdata_HBS_individual_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_individual_raw.dt,
  target_vars      = MAR_group2_target_vars_individual,
  auxiliary_vars   = MAR_group2_aux_vars_individual,
  prop_missing     = MAR_group2_prop_missing_individual,
  mechanism        = 'MAR',  
  mechanism_type   = MAR_group2_mechanism_type_individual,
  mechanism_args   = MAR_group2_mechanism_args_individual
)


# Visualize NA values ####
## Household ####
skim(microdata_HBS_household_raw.dt)

(missing_indicator.df <- mice::md.pattern(
  microdata_HBS_household_raw.dt[
    , c(frame_vars_household, unlist(target_vars_household.lst)), with = FALSE], 
  plot = FALSE)
)
vis_miss(microdata_HBS_household_raw.dt[, c(frame_vars_household, unlist(target_vars_household.lst)), with = FALSE], warn_large_data = FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

(missing_rates_household <- colMeans(is.na(
  microdata_HBS_household_raw.dt[
    , c(frame_vars_household, unlist(target_vars_household.lst)), with = FALSE])) * 100
)
gg_miss_var(microdata_HBS_household_raw.dt[, c(frame_vars_household, unlist(target_vars_household.lst)), with = FALSE]) + 
  theme_minimal(base_size = 8)

## Individual ####
skim(microdata_HBS_individual_raw.dt)

(missing_indicator.df <- mice::md.pattern(
  microdata_HBS_individual_raw.dt[
    , c(frame_vars_individual, unlist(target_vars_individual.lst)), with = FALSE], 
  plot = FALSE)
)
vis_miss(microdata_HBS_individual_raw.dt[, c(frame_vars_individual, unlist(target_vars_individual.lst)), with = FALSE], warn_large_data = FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

(missing_rates_individual <- colMeans(is.na(
  microdata_HBS_individual_raw.dt[
    , c(frame_vars_individual, unlist(target_vars_individual.lst)), with = FALSE])) * 100
)
gg_miss_var(microdata_HBS_individual_raw.dt[, c(frame_vars_individual, unlist(target_vars_individual.lst)), with = FALSE]) + 
  theme_minimal(base_size = 8)

# Write raw microdata sets ####
fwrite(microdata_HBS_household_raw.dt, file = microdata_HBS_household_raw_fn, sep = ";")
fwrite(microdata_HBS_individual_raw.dt, file = microdata_HBS_individual_raw_fn, sep = ";")
