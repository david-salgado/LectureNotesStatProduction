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
library(stringr)

# Set relative paths ####
path_project        <- here()
path_HBS_samples    <- file.path(path_project, 'data', 'sample_selection', 'HBS')
path_HBS_collection <- file.path(path_project, 'data', 'data_collection', 'HBS')
path_src            <- file.path(path_project, 'src')
path_HBS_grTruth    <- file.path(path_project, 'data', 'ground_truth', 'HBS')

# Load src functions ####
source(file.path(path_src, "generate_missing_values.R"))
source(file.path(path_src, "generate_typo_errors.R"))
source(file.path(path_src, "generate_sign_errors.R"))
source(file.path(path_src, "generate_outliers.R"))
source(file.path(path_src, "compare_distributions.R"))

# Set absolute filenames ####
microdata_HBS_household_sample_fn  <- 'microdata_HBS_household_sample.csv'
microdata_HBS_household_sample_fn  <- file.path(
  path_HBS_samples, microdata_HBS_household_sample_fn)

microdata_HBS_individual_sample_fn  <- 'microdata_HBS_individual_sample.csv'
microdata_HBS_individual_sample_fn  <- file.path(
  path_HBS_samples, microdata_HBS_individual_sample_fn)

microdata_HBS_household_raw_fn <- "microdata_HBS_household_raw.csv"
microdata_HBS_household_raw_fn <- file.path(path_HBS_collection, microdata_HBS_household_raw_fn)

microdata_HBS_individual_raw_fn <- "microdata_HBS_individual_raw.csv"
microdata_HBS_individual_raw_fn <- file.path(path_HBS_collection, microdata_HBS_individual_raw_fn)

microdata_HBS_household_grTruth_fn <- "data_HBS2023_household_grTruth.csv"
microdata_HBS_household_grTruth_fn <- file.path(path_HBS_grTruth, microdata_HBS_household_grTruth_fn)

microdata_HBS_individual_grTruth_fn <- "data_HBS2023_individual_grTruth.csv"
microdata_HBS_individual_grTruth_fn <- file.path(path_HBS_grTruth, microdata_HBS_individual_grTruth_fn)

# Set parameters ####
## Household variables ####
frame_vars_household      <- c("hid", "geo1", "geo2", "ea", "urbrur", "hhsize")
target_vars_household.lst <- list(
  dwelling    = c("statocc", "rooms", "bedrooms", "floor", "walls", "roof"),
  water       = c("water", "piped_water", "toilet", "flush_toilet"),
  energy      = c("electricity", "cook_fuel"),
  assets      = c("phone", "cell", "car", "bicycle", "motorcycle", "refrigerator", "tv", "radio", "bank"),
  expenditure = c("exp_01", "exp_02", "exp_03", "exp_04", "exp_05", "exp_06", 
                  "exp_07", "exp_08", "exp_09", "exp_10", "exp_11", "exp_12",
                  "tot_exp", "tot_food", "share_food", "pc_exp")
)
vars_classes_household <- c('character', 'factor', 'factor', 'factor', 'factor', 'integer',
                            'factor', 'integer', 'integer', 'factor', 'factor', 'factor',
                            rep('factor', length(target_vars_household.lst$water)),
                            rep('factor', length(target_vars_household.lst$energy)),
                            rep('factor', length(target_vars_household.lst$assets)),
                            rep('numeric', length(target_vars_household.lst$expenditure)))
names(vars_classes_household) <- c(frame_vars_household, unlist(target_vars_household.lst))
vars_household <- c(frame_vars_household, unlist(target_vars_household.lst))

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

## Missing mechanism ####
### Partial NR - household ####
partial_MAR_group1_target_vars_household    <- unlist(
  target_vars_household.lst[c('dwelling', 'water', 'energy', 'assets')]
)
partial_MAR_group1_aux_vars_household       <- c("geo1", "urbrur")
partial_MAR_group1_prop_missing_household   <- rnorm(
  length(partial_MAR_group1_target_vars_household), 0.1, 0.02
)
partial_MAR_group1_mechanism_type_household <- "logistic"
partial_MAR_group1_mechanism_args_household <- list(interaction = TRUE)

partial_MAR_group2_target_vars_household    <- unlist(target_vars_household.lst['expenditure'])
partial_MAR_group2_aux_vars_household       <- c("geo1", "urbrur", "hhsize")
partial_MAR_group2_prop_missing_household   <- rnorm(length(partial_MAR_group2_target_vars_household), 0.30, 0.05)
partial_MAR_group2_mechanism_type_household <- "randomforest"
partial_MAR_group2_mechanism_args_household <- list(
  num.trees = 50,
  mtry = floor(sqrt(length(partial_MAR_group2_aux_vars_household))),
  importance = "none",
  probability = TRUE,
  classification = TRUE
)

### Partial NR - individual ####
partial_MCAR_group1_target_vars_individual    <- unlist(
  target_vars_individual.lst["demographics"]
)
partial_MCAR_group1_prop_missing_individual   <- rnorm(
  length(partial_MCAR_group1_target_vars_individual), 0.02, 0.001
)

partial_MAR_group2_target_vars_individual    <- unlist(
  target_vars_individual.lst[c("employment", "migration", "disabilities")]
)
partial_MAR_group2_aux_vars_individual       <- c("sex", "age")
partial_MAR_group2_prop_missing_individual   <- rnorm(length(partial_MAR_group2_target_vars_individual), 0.1, 0.03)
partial_MAR_group2_mechanism_type_individual <- "randomforest"
partial_MAR_group2_mechanism_args_individual <- list(
  num.trees = 50,
  mtry = floor(sqrt(length(partial_MAR_group2_aux_vars_individual))),
  importance = "none",
  probability = TRUE,
  classification = TRUE
)

### Total NR - household ####
total_MAR_target_vars_household    <- unlist(target_vars_household.lst)
total_MAR_aux_vars_household       <- c("geo1", "urbrur", "hhsize")
total_MAR_prop_missing             <- 0.05
total_MAR_mechanism_type_household <- "logistic"
total_MAR_mechanism_args_household <- list(interaction = TRUE)

### Total NR - individual ####
total_MAR_target_vars_individual    <- unlist(target_vars_individual.lst)
total_MAR_aux_vars_individual       <- c("sex", "age")
total_MAR_prop_missing_individual   <- 0.05
total_MAR_mechanism_type_individual <- "randomforest"
total_MAR_mechanism_args_individual <- list(
  num.trees = 50,
  mtry = floor(sqrt(length(total_MAR_aux_vars_individual))),
  importance = "none",
  probability = TRUE,
  classification = TRUE
)

# Read HBS sample ####
microdata_HBS_household_sample.dt <- fread(
  microdata_HBS_household_sample_fn,
  sep = ";",
  colClasses = vars_classes_household[names(vars_classes_household) %in% frame_vars_household]
)

microdata_HBS_individual_sample.dt <- fread(
  microdata_HBS_individual_sample_fn,
  sep = ";",
  colClasses = vars_classes_individual[names(vars_classes_individual) %in% c(frame_vars_individual, "relation", "sex", "age")]
)

# Read HBS ground truth ####
microdata_HBS_household_grTruth.dt <- fread(
  microdata_HBS_household_grTruth_fn,
  sep = ";",
  colClasses = vars_classes_household
)[, ..vars_household]

microdata_HBS_individual_grTruth.dt <- fread(
  microdata_HBS_individual_grTruth_fn,
  sep = ";",
  colClasses = vars_classes_individual
)

# Simulate data collection ####
## Household ####
microdata_HBS_household_sample.dt <- merge(
  microdata_HBS_household_sample.dt, microdata_HBS_household_grTruth.dt,
  by = frame_vars_household, all.x = TRUE
)

for (var in unlist(target_vars_household.lst)) {
  
  microdata_HBS_household_sample.dt[sII == 0 , (var) := NA]
  
}

## Individual ####
microdata_HBS_individual_sample.dt <- merge(
  microdata_HBS_individual_sample.dt, microdata_HBS_individual_grTruth.dt,
  by = c(frame_vars_individual, "relation", "sex", "age") , all.x = TRUE
)

for (var in unlist(target_vars_individual.lst)) {
  
  microdata_HBS_individual_sample.dt[s == 0 , (var) := NA]
  
}


# Generate missing values - partial nonresponse ####
## Household ####
### Group 1 variables ####
microdata_HBS_household_missing_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_household_sample.dt[sII == 1],
  target_vars      = partial_MAR_group1_target_vars_household,
  auxiliary_vars   = partial_MAR_group1_aux_vars_household,
  prop_missing     = partial_MAR_group1_prop_missing_household,
  nonresponse      = 'partial',
  mechanism        = 'MAR',  
  mechanism_type   = partial_MAR_group1_mechanism_type_household,
  mechanism_args   = partial_MAR_group1_mechanism_args_household
)

### Group 2 variables ####
microdata_HBS_household_missing_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_household_missing_raw.dt,
  target_vars      = partial_MAR_group2_target_vars_household,
  auxiliary_vars   = partial_MAR_group2_aux_vars_household,
  prop_missing     = partial_MAR_group2_prop_missing_household,
  nonresponse      = 'partial',
  mechanism        = 'MAR',  
  mechanism_type   = partial_MAR_group2_mechanism_type_household,
  mechanism_args   = partial_MAR_group2_mechanism_args_household
)

## Individual ####
### Group 1 variables ####
microdata_HBS_individual_missing_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_individual_sample.dt[s == 1],
  target_vars      = partial_MCAR_group1_target_vars_individual,
  prop_missing     = partial_MCAR_group1_prop_missing_individual,
  nonresponse      = 'partial',
  mechanism        = 'MCAR'
)

### Group 2 variables ####
microdata_HBS_individual_missing_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_individual_missing_raw.dt,
  target_vars      = partial_MAR_group2_target_vars_individual,
  auxiliary_vars   = partial_MAR_group2_aux_vars_individual,
  prop_missing     = partial_MAR_group2_prop_missing_individual,
  nonresponse      = 'partial',
  mechanism        = 'MAR',  
  mechanism_type   = partial_MAR_group2_mechanism_type_individual,
  mechanism_args   = partial_MAR_group2_mechanism_args_individual
)

# Generate missing values - total nonresponse ####
## Household ####
microdata_HBS_household_missing_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_household_missing_raw.dt,
  target_vars      = total_MAR_target_vars_household,
  auxiliary_vars   = total_MAR_aux_vars_household,
  prop_missing     = total_MAR_prop_missing,
  nonresponse      = 'total',
  mechanism        = 'MAR',  
  mechanism_type   = total_MAR_mechanism_type_household,
  mechanism_args   = total_MAR_mechanism_args_household
)
setnames(microdata_HBS_household_missing_raw.dt, "r", "rII")


for (var in unlist(target_vars_household.lst)) {
  
  microdata_HBS_household_missing_raw.dt[rII == 0 , (var) := NA]
  
}

## Individual ####
microdata_HBS_individual_missing_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_individual_missing_raw.dt,
  target_vars      = total_MAR_target_vars_individual,
  prop_missing     = total_MAR_prop_missing_individual,
  nonresponse      = 'total',
  mechanism        = 'MAR',
  mechanism_type   = total_MAR_mechanism_type_individual,
  mechanism_args   = total_MAR_mechanism_args_individual
)


for (var in unlist(target_vars_individual.lst)) {
  
  microdata_HBS_individual_missing_raw.dt[r == 0 , (var) := NA]
  
}

# Visualize missing values ####
## Household ####
skim(microdata_HBS_household_missing_raw.dt[
  rII == 1][
  , c(frame_vars_household, unlist(target_vars_household.lst)), with = FALSE], .data_name = 'Household Data')

(missing_indicator.df <- mice::md.pattern(
  microdata_HBS_household_missing_raw.dt[
    rII == 1][
    , c(frame_vars_household, unlist(target_vars_household.lst)), with = FALSE], 
  plot = FALSE)
)
vis_miss(microdata_HBS_household_missing_raw.dt[rII == 1, c(frame_vars_household, unlist(target_vars_household.lst)), with = FALSE], warn_large_data = FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

(missing_rates_household <- colMeans(is.na(
  microdata_HBS_household_missing_raw.dt[
    rII == 1][
    , c(frame_vars_household, unlist(target_vars_household.lst)), with = FALSE])) * 100
)
gg_miss_var(microdata_HBS_household_missing_raw.dt[rII == 1, c(frame_vars_household, unlist(target_vars_household.lst)), with = FALSE], show_pct = TRUE) + 
  theme_minimal(base_size = 8)

## Individual ####
skim(microdata_HBS_individual_missing_raw.dt[
    r == 1][
  , c(frame_vars_individual, unlist(target_vars_individual.lst)), with = FALSE], .data_name = 'Individual Data')

(missing_indicator.df <- mice::md.pattern(
  microdata_HBS_individual_missing_raw.dt[
    r == 1][
    , c(frame_vars_individual, unlist(target_vars_individual.lst)), with = FALSE], 
  plot = FALSE)
)
vis_miss(microdata_HBS_individual_missing_raw.dt[r == 1, c(frame_vars_individual, unlist(target_vars_individual.lst)), with = FALSE], warn_large_data = FALSE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

(missing_rates_individual <- colMeans(is.na(
  microdata_HBS_individual_missing_raw.dt[
    r == 1][
    , c(frame_vars_individual, unlist(target_vars_individual.lst)), with = FALSE])) * 100
)
gg_miss_var(
  microdata_HBS_individual_missing_raw.dt[
    r == 1][
    , c(frame_vars_individual, unlist(target_vars_individual.lst)), with = FALSE], 
  show_pct = TRUE) + 
  theme_minimal(base_size = 8)

# Generate non-sampling errors ####
## Household ####
### Typos errors ####
microdata_HBS_household_raw_typos_categorical.dt <- generate_typo_errors(
  microdata_HBS_household_missing_raw.dt, 
  vars        = c('geo2', 'ea', 'urbrur', 'water', 'toilet', 'electricity', 'tv'),
  error_rate  = 0.05, 
  error_types = c("keyboard", "transposition", "double_strike"),
  typo_prob   = 0.3, max_typos = 2, custom_typos = NULL,
  seed        = 123)

custom_probs <- list(
  exp_01 = 0.03,  
  exp_02 = 0.01,
  exp_03 = 0.04
)

### Sign errors ####
microdata_HBS_household_raw_sign_exp.dt <- generate_sign_errors(
  data          = microdata_HBS_household_raw_typos_categorical.dt,
  vars          = paste("exp", str_pad(1:10, 2, pad = "0"), sep = "_"),
  skip_negative = TRUE,
  error_rate    = 0.02,  # Default for exp_nn
  custom_prob   = custom_probs
)

### Outliers ####
microdata_HBS_household_raw_outliers.dt <- generate_outliers(
  data = microdata_HBS_household_raw_sign_exp.dt,
  numerical_columns = c("exp_01", "exp_10"),
  mechanisms = "replace_extreme",
  proportion = 0.02,
  strength = 4,
  seed = 123
)

### Measurement units error ####
microdata_HBS_household_raw_unitError.dt <- microdata_HBS_household_raw_outliers.dt[
  , tot_exp := tot_exp / 1000
]

# Join non-sampling units ####
## Household ####
microdata_HBS_household_missing_raw.dt <- rbind(
  microdata_HBS_household_missing_raw.dt,
  microdata_HBS_household_sample.dt[, rII := 0][sII == 0]
)

## Individual ####
microdata_HBS_individual_missing_raw.dt <- rbind(
  microdata_HBS_individual_missing_raw.dt,
  microdata_HBS_individual_sample.dt[, r := 0][s == 0]
)

# Write raw microdata sets ####
fwrite(microdata_HBS_household_missing_raw.dt,  file = microdata_HBS_household_raw_fn,  sep = ";")
fwrite(microdata_HBS_individual_missing_raw.dt, file = microdata_HBS_individual_raw_fn, sep = ";")
