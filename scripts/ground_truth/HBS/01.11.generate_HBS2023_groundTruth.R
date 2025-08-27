# Load packages ####
library(haven)
library(data.table)
library(here)
library(xml2)

# Set relative paths ####
path_project   <- here()
path_data_orig <- file.path(path_project, 'data', 'original')
path_data_grTruth <- file.path(path_project, 'data', 'HBS', 'ground_truth')
path_metadata_HBS_grTruth <- file.path(path_project, 'metadata', 'HBS', 'ground_truth')

# Set absolute filenames ####
data_household_orig_fn  <- 'WLD_2023_SYNTH-CEN-HLD-EN_v01_M.dta'
data_household_orig_fn  <- file.path(
  path_data_orig, data_household_orig_fn)
data_individual_orig_fn <- 'WLD_2023_SYNTH-CEN-IND-EN_v01_M.dta'
data_individual_orig_fn <- file.path(
  path_data_orig, data_individual_orig_fn)

data_HBS2023_household_grTruth_fn  <- 'data_HBS2023_household_grTruth.csv'
data_HBS2023_household_grTruth_fn  <- file.path(
  path_data_grTruth, data_HBS2023_household_grTruth_fn)
data_HBS2023_individual_grTruth_fn <- 'data_HBS2023_individual_grTruth.csv'
data_HBS2023_individual_grTruth_fn <- file.path(
  path_data_grTruth, data_HBS2023_individual_grTruth_fn)

metadata_HBS2023_grTruth_fn <- file.path(path_metadata_HBS_grTruth, "HBS_2023_synth_ground-truth.xml")

# Read data and metadata ####
## Population of households ####
data_household_orig.dt <- as.data.table(read_dta(data_household_orig_fn))

## Population of individuals ####
data_individual_orig.dt <- as.data.table(read_dta(data_individual_orig_fn))

## Metadata in DDI-C ####
metadata_HBS2023_grTruth.xml <- xml_ns_strip(
  read_xml(metadata_HBS2023_grTruth_fn))

# Select HBS variables and set their class ####
variables.xmlnodeset <- xml_find_all(metadata_HBS2023_grTruth.xml, "//var")
varnames_HBS2023_household    <- c()
varclasses_HBS2023_household  <- c()
varnames_HBS2023_individual   <- c()
varclasses_HBS2023_individual <- c()
for(i in seq_along(variables.xmlnodeset)) {
  
  var_file <- xml_attr(variables.xmlnodeset[i], "files")
  var_name <- xml_attr(variables.xmlnodeset[i], "name")
  var_class <- xml_attr(variables.xmlnodeset[i], "intrvl")
  if (var_file == "F1") {
    
    varnames_HBS2023_household <- c(varnames_HBS2023_household, var_name)
    varclasses_HBS2023_household <- c(varclasses_HBS2023_household, var_class)
    
  }
  
  if (var_file == "F2") {
    
    varnames_HBS2023_individual <- c(varnames_HBS2023_individual, var_name)
    varclasses_HBS2023_individual <- c(varclasses_HBS2023_individual, var_class)
    
  }
}

## Household variables ####
names(varnames_HBS2023_household) <- varclasses_HBS2023_household
varnames_HBS2023_household_discrt <- varnames_HBS2023_household[names(varnames_HBS2023_household) == "discrete"]
varnames_HBS2023_household_cont <- varnames_HBS2023_household[names(varnames_HBS2023_household) == "contin"]
data_HBS2023_household_grTruth.dt <- data_household_orig.dt[, ..varnames_HBS2023_household]
data_HBS2023_household_grTruth.dt[
  , (varnames_HBS2023_household_discrt) := lapply(.SD, as.factor), .SDcols = varnames_HBS2023_household_discrt]
data_HBS2023_household_grTruth.dt[
  , (varnames_HBS2023_household_cont) := lapply(.SD, as.numeric), .SDcols = varnames_HBS2023_household_cont]

## Individual variables ####
names(varnames_HBS2023_individual) <- varclasses_HBS2023_individual
varnames_HBS2023_individual_discrt <- varnames_HBS2023_individual[names(varnames_HBS2023_individual) == "discrete"]
varnames_HBS2023_individual_cont <- varnames_HBS2023_individual[names(varnames_HBS2023_individual) == "contin"]
data_HBS2023_individual_grTruth.dt <- data_individual_orig.dt[, ..varnames_HBS2023_individual]
data_HBS2023_individual_grTruth.dt[
  , (varnames_HBS2023_individual_discrt) := lapply(.SD, as.factor), .SDcols = varnames_HBS2023_individual_discrt]
data_HBS2023_individual_grTruth.dt[
  , (varnames_HBS2023_individual_cont) := lapply(.SD, as.numeric), .SDcols = varnames_HBS2023_individual_cont]


# Write datasets ####
fwrite(data_HBS2023_household_grTruth.dt, 
       file = data_HBS2023_household_grTruth_fn,
       sep = ";")
if (file.exists(data_HBS2023_household_grTruth_fn)) cat(paste0('File written in ', data_HBS2023_household_grTruth_fn))
fwrite(data_HBS2023_individual_grTruth.dt, 
       file = data_HBS2023_individual_grTruth_fn,
       sep = ";")
if (file.exists(data_HBS2023_individual_grTruth_fn)) cat(paste0('File written in ', data_HBS2023_individual_grTruth_fn))
