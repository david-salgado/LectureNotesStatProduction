# Load packages ####
library(haven)
library(data.table)
library(here)
library(xml2)

# Set relative paths ####
path_project   <- here()
path_data_orig <- file.path(path_project, 'data', 'original')
path_data_frames <- file.path(path_project, 'data', 'frames')
path_metadata_frames <- file.path(path_project, 'metadata', 'frames')

# Set absolute filenames ####
data_household_orig_fn  <- 'WLD_2023_SYNTH-CEN-HLD-EN_v01_M.dta'
data_household_orig_fn  <- file.path(
  path_data_orig, data_household_orig_fn)
data_individual_orig_fn <- 'WLD_2023_SYNTH-CEN-IND-EN_v01_M.dta'
data_individual_orig_fn <- file.path(
  path_data_orig, data_individual_orig_fn)

data_frame2023_household_perfect_fn  <- 'data_frame2023_household_perfect.csv'
data_frame2023_household_perfect_fn  <- file.path(
  path_data_frames, data_frame2023_household_perfect_fn)
data_frame2023_individual_perfect_fn <- 'data_frame2023_individual_perfect.csv'
data_frame2023_individual_perfect_fn <- file.path(
  path_data_frames, data_frame2023_individual_perfect_fn)

metadata_frame2023_perfect_fn <- file.path(path_metadata_frames, "frame_2023_synth_perfect.xml")

# Read data and metadata ####
## Household frame ####
data_household_orig.dt <- as.data.table(read_dta(data_household_orig_fn))

## Individual frame ####
data_individual_orig.dt <- as.data.table(read_dta(data_individual_orig_fn))

## Metadata in DDI-C ####
metadata_frame2023_perfect.xml <- xml_ns_strip(
  read_xml(metadata_frame2023_perfect_fn))

# Select LFS variables and set their class ####
variables.xmlnodeset <- xml_find_all(metadata_frame2023_perfect.xml, "//var")
varnames_frame2023_household    <- c()
varclasses_frame2023_household  <- c()
varnames_frame2023_individual   <- c()
varclasses_frame2023_individual <- c()
for(i in seq_along(variables.xmlnodeset)) {
  
  var_file <- xml_attr(variables.xmlnodeset[i], "files")
  var_name <- xml_attr(variables.xmlnodeset[i], "name")
  var_class <- xml_attr(variables.xmlnodeset[i], "intrvl")
  if (var_file == "F1") {
    
    varnames_frame2023_household   <- c(varnames_frame2023_household, var_name)
    varclasses_frame2023_household <- c(varclasses_frame2023_household, var_class)
    
  }
  
  if (var_file == "F2") {
    
    varnames_frame2023_individual   <- c(varnames_frame2023_individual, var_name)
    varclasses_frame2023_individual <- c(varclasses_frame2023_individual, var_class)
    
  }
}

## Household variables ####
names(varnames_frame2023_household) <- varclasses_frame2023_household
varnames_frame2023_household_discrt <- varnames_frame2023_household[names(varnames_frame2023_household) == "discrete"]
varnames_frame2023_household_cont <- varnames_frame2023_household[names(varnames_frame2023_household) == "contin"]
data_frame2023_household_perfect.dt <- data_household_orig.dt[, ..varnames_frame2023_household]
data_frame2023_household_perfect.dt[
  , (varnames_frame2023_household_discrt) := lapply(.SD, as.factor), .SDcols = varnames_frame2023_household_discrt]
data_frame2023_household_perfect.dt[
  , (varnames_frame2023_household_cont) := lapply(.SD, as.numeric), .SDcols = varnames_frame2023_household_cont]

## Individual variables ####
names(varnames_frame2023_individual) <- varclasses_frame2023_individual
varnames_frame2023_individual_discrt <- varnames_frame2023_individual[names(varnames_frame2023_individual) == "discrete"]
varnames_frame2023_individual_cont <- varnames_frame2023_individual[names(varnames_frame2023_individual) == "contin"]
data_frame2023_individual_perfect.dt <- data_individual_orig.dt[, ..varnames_frame2023_individual]
data_frame2023_individual_perfect.dt[
  , (varnames_frame2023_individual_discrt) := lapply(.SD, as.factor), .SDcols = varnames_frame2023_individual_discrt]
data_frame2023_individual_perfect.dt[
  , (varnames_frame2023_individual_cont) := lapply(.SD, as.numeric), .SDcols = varnames_frame2023_individual_cont]


# Write datasets ####
fwrite(data_frame2023_household_perfect.dt, 
       file = data_frame2023_household_perfect_fn,
       sep = ";")
if (file.exists(data_frame2023_household_perfect_fn)) cat(paste0('File written in ', data_frame2023_household_perfect_fn))
fwrite(data_frame2023_individual_perfect.dt, 
       file = data_frame2023_individual_perfect_fn,
       sep = ";")
if (file.exists(data_frame2023_individual_perfect_fn)) cat(paste0('File written in ', data_frame2023_individual_perfect_fn))
