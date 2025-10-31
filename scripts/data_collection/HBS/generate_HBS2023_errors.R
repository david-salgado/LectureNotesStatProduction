microdata_HBS_household_raw_typos_categorical.dt <- generate_typo_errors(
  microdata_HBS_household_raw.dt, 
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
 
microdata_HBS_household_raw_sign_exp.dt <- generate_sign_errors(
   data          = microdata_HBS_household_raw_typos_categorical.dt,
   vars          = paste("exp", str_pad(1:10, 2, pad = "0"), sep = "_"),
   skip_negative = TRUE,
   error_rate    = 0.02,  # Default for exp_nn
   custom_prob   = custom_probs
 )

microdata_HBS_household_raw_outliers.dt <- generate_outliers(
  data = microdata_HBS_household_raw_sign_exp.dt,
  numerical_columns = c("exp_01", "exp_10"),
  mechanisms = "replace_extreme",
  proportion = 0.02,
  strength = 4,
  seed = 123
)

microdata_HBS_household_raw_unitError.dt <- microdata_HBS_household_raw_outliers.dt[
  , tot_exp := tot_exp / 1000
]
