# Load packages
library(haven)
library(data.table)

# Set relative paths
path_project <- here::here()
path_data_raw <- file.path(path_project, 'data', 'raw')

# Read data
data_household_raw.dt <- as.data.table(
  read_dta(file.path(path_data_raw, 'WLD_2023_SYNTH-CEN-HLD-EN_v01_M.dta'))
)

data_individual_raw.dt <- as.data.table(
  read_dta(file.path(path_data_raw, 'WLD_2023_SYNTH-CEN-IND-EN_v01_M.dta'))
) 
