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
path_data_grTruth_HBS <- file.path(path_project, 'data', 'ground_truth', 'HBS')
path_data_frame       <- file.path(path_project, 'data', 'frames')
path_data_samples_HBS <- file.path(path_project, 'data', 'sample_selection', 'HBS')
path_src <- file.path(path_project, 'src')

# Load src functions ####
source(file.path(path_src, "generate_missing_values.R"))

# Set absolute filenames ####
data_HBS_household_frame_fn  <- 'data_frame2023_household_perfect.csv'
data_HBS_household_frame_fn  <- file.path(
  path_data_frame, data_HBS_household_frame_fn)

data_HBS_individual_frame_fn  <- 'data_frame2023_individual_perfect.csv'
data_HBS_individual_frame_fn  <- file.path(
  path_data_frame, data_HBS_individual_frame_fn)

microdata_HBS_household_sample_fn <- "microdata_HBS_household_sample.csv"
microdata_HBS_household_sample_fn <- file.path(path_data_samples_HBS, microdata_HBS_household_sample_fn)

microdata_HBS_individual_sample_fn <- "microdata_HBS_individual_sample.csv"
microdata_HBS_individual_sample_fn <- file.path(path_data_samples_HBS, microdata_HBS_individual_sample_fn)

microdata_HBS_household_raw_fn <- "microdata_HBS_household_raw.csv"
microdata_HBS_household_raw_fn <- file.path(path_data_samples_HBS, microdata_HBS_household_raw_fn)

microdata_HBS_individual_raw_fn <- "microdata_HBS_individual_raw.csv"
microdata_HBS_individual_raw_fn <- file.path(path_data_samples_HBS, microdata_HBS_individual_raw_fn)


# Set parameters ####
## Sampling fractions ####
sampling_fraction_district <- 12/61 # At least one district (total 61) per province (geo1 10) + 2 of margin
sampling_fraction_household <- 0.01 # Same for all districts

## Household variables ####
frame_vars_household      <- c("hid", "geo1", "geo2", "ea", "urbrur", "hhsize")
vars_classes_household <- c('character', 'factor', 'factor', 'factor', 'factor', 'integer')
names(vars_classes_household) <- frame_vars_household

## Individual variables ####
frame_vars_individual      <- c("hid", "idno")
vars_classes_individual <- c('character', 'factor')
names(vars_classes_individual) <- frame_vars_individual


# Read HBS frame data at household level ####
## Household ####
microdata_HBS_household_frame.dt <- fread(
  data_HBS_household_frame_fn, sep = ";")

for (i in seq_along(vars_classes_household)) {
  
  col <- names(microdata_HBS_household_frame.dt)[i]
  clase <- vars_classes_household[i]
  set(microdata_HBS_household_frame.dt, j = col, 
      value = match.fun(paste0("as.", clase))(microdata_HBS_household_frame.dt[[col]]))
  
}

## Individual ####
microdata_HBS_individual_frame.dt <- fread(
  data_HBS_individual_frame_fn, sep = ";")

for (i in seq_along(vars_classes_individual)) {
  
  col <- names(microdata_HBS_individual_frame.dt)[i]
  clase <- vars_classes_individual[i]
  set(microdata_HBS_individual_frame.dt, j = col, 
      value = match.fun(paste0("as.", clase))(microdata_HBS_individual_frame.dt[[col]]))
  
}

# Calculate household first-order inclusion probabilities ####
NI <- length(unique(microdata_HBS_household_frame.dt$geo2))
nI <- round(sampling_fraction_district * NI)
microdata_HBS_household_frame.dt[
  , NIi := .N, by = "geo2"]

microdata_HBS_district.dt <- microdata_HBS_household_frame.dt[
  , c("geo2", "NIi"), with = FALSE]

microdata_HBS_district.dt <- microdata_HBS_district.dt[
  !duplicated(microdata_HBS_district.dt, by = 'geo2')][
  , piIi := inclusionprobabilities(NIi, nI)]

microdata_HBS_household_sample.dt <- microdata_HBS_household_frame.dt[
  microdata_HBS_district.dt, on = c('geo2', 'NIi')]

# Select district and household samples ####
microdata_HBS_district_sample.dt <- microdata_HBS_district.dt[
  , sI := UPrandompivotal(piIi)]

microdata_HBS_household_sample.dt <- microdata_HBS_household_sample.dt[
  microdata_HBS_district_sample.dt, on = c('geo2', 'NIi', 'piIi')][
  , nII := round(sampling_fraction_household * NIi)]

microdata_HBS_district_sample.dt <- microdata_HBS_household_sample.dt[
  , c('geo2', 'nII', 'NIi'), with = FALSE]
microdata_HBS_district_sample.dt <- microdata_HBS_district_sample.dt[
  !duplicated(microdata_HBS_district_sample.dt, by = 'geo2')]

district_sample.dt <- microdata_HBS_district_sample.dt[
  , .(sII = srswor(nII, NIi)), by = "geo2"]
microdata_HBS_household_sample.dt <- microdata_HBS_household_sample.dt[
  , sII := district_sample.dt$sII]

microdata_HBS_individual_sample.dt <- microdata_HBS_individual_frame.dt[
  , s := (hid %chin% microdata_HBS_household_sample.dt[sII == 1][['hid']]) * 1L]

fwrite(microdata_HBS_household_sample.dt, 
       microdata_HBS_household_sample_fn,
       sep = ";")

fwrite(microdata_HBS_individual_sample.dt, 
       microdata_HBS_individual_sample_fn,
       sep = ";")
