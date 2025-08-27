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
path_HBS_samples <- file.path(path_project, 'data', 'sample_selection', 'HBS')
path_src <- file.path(path_project, 'src')

# Load src functions ####
source(file.path(path_src, "generate_missing_values.R"))

# Set absolute filenames ####
microdata_HBS_household_sample_fn  <- 'microdata_HBS_household_sample.csv'
microdata_HBS_household_sample_fn  <- file.path(
  path_HBS_samples, microdata_HBS_household_sample_fn)

microdata_HBS_individual_sample_fn  <- 'microdata_HBS_individual_sample.csv'
microdata_HBS_individual_sample_fn  <- file.path(
  path_HBS_samples, microdata_HBS_individual_sample_fn)

microdata_HBS_household_raw_fn <- "microdata_HBS_household_raw_nonResp.csv"
microdata_HBS_household_raw_fn <- file.path(path_HBS_samples, microdata_HBS_household_raw_fn)

microdata_HBS_individual_raw_fn <- "microdata_HBS_individual_raw_nonResp.csv"
microdata_HBS_individual_raw_fn <- file.path(path_HBS_samples, microdata_HBS_individual_raw_fn)


# Set parameters ####
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
partial_MAR_group2_prop_missing_household   <- rnorm(length(partial_MAR_group2_target_vars_household), 0.5, 0.1)
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
partial_MAR_group2_prop_missing_individual   <- rnorm(length(partial_MAR_group2_target_vars_individual), 0.1, 0.05)
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
  colClasses = vars_classes_household
)

microdata_HBS_individual_sample.dt <- fread(
  microdata_HBS_individual_sample_fn,
  sep = ";",
  colClasses = vars_classes_individual
)

# Generate missing values - partial nonresponse ####
## Household ####
### Group 1 variables ####
microdata_HBS_household_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_household_sample.dt,
  target_vars      = partial_MAR_group1_target_vars_household,
  auxiliary_vars   = partial_MAR_group1_aux_vars_household,
  prop_missing     = partial_MAR_group1_prop_missing_household,
  nonresponse      = 'partial',
  mechanism        = 'MAR',  
  mechanism_type   = partial_MAR_group1_mechanism_type_household,
  mechanism_args   = partial_MAR_group1_mechanism_args_household
)

### Group 2 variables ####
microdata_HBS_household_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_household_raw.dt,
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
microdata_HBS_individual_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_individual_sample.dt,
  target_vars      = partial_MCAR_group1_target_vars_individual,
  prop_missing     = partial_MCAR_group1_prop_missing_individual,
  nonresponse      = 'partial',
  mechanism        = 'MCAR'
)

### Group 2 variables ####
microdata_HBS_individual_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_individual_raw.dt,
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
microdata_HBS_household_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_household_raw.dt,
  target_vars      = total_MAR_target_vars_household,
  auxiliary_vars   = total_MAR_aux_vars_household,
  prop_missing     = total_MAR_prop_missing,
  nonresponse      = 'total',
  mechanism        = 'MAR',  
  mechanism_type   = total_MAR_mechanism_type_household,
  mechanism_args   = total_MAR_mechanism_args_household
)

## Individual ####
microdata_HBS_individual_raw.dt <- generate_missing_values(
  complete_data.dt = microdata_HBS_individual_raw.dt,
  target_vars      = total_MAR_target_vars_individual,
  prop_missing     = total_MAR_prop_missing_individual,
  nonresponse      = 'total',
  mechanism        = 'MAR',
  mechanism_type   = total_MAR_mechanism_type_individual,
  mechanism_args   = total_MAR_mechanism_args_individual
)

# Visualize NA values ####
## Household ####
skim(microdata_HBS_household_raw.dt[
  , c(frame_vars_household, unlist(target_vars_household.lst)), with = FALSE], .data_name = 'Household Data')

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
skim(microdata_HBS_individual_raw.dt[
  , c(frame_vars_individual, unlist(target_vars_individual.lst)), with = FALSE], .data_name = 'Individual Data')

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
