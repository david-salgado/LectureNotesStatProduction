#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#                                                                              #
# This script applies different element-wise sampling designs to the household #
# frame and compare the fraction of population in both the frame and the       #
# selected sample of households across the frame variables to assess their     #
# difference.                                                                  #
#                                                                              #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# Load packages ####
library(here)
library(sampling)
library(data.table)
library(ggplot2)
library(ggdist)
library(gghalves)
library(ggbeeswarm)

# Set relative paths ####
path_project <- here()
path_frames  <- file.path(path_project, 'data', 'frames')
path_samples <- file.path(path_project, 'data', 'samples')
path_src     <- file.path(path_project, 'src')

# Load src functions ####
source(file.path(path_src, "plot_raincloud_relative_diff_popFraction.R"))
source(file.path(path_src, "calculate_relative_diff_popFraction.R"))

# Set absolute filenames ####
frame_household_perfect_fn  <- 'data_frame2023_household_perfect.csv'
frame_household_perfect_fn  <- file.path(path_frames, frame_household_perfect_fn)
frame_individual_perfect_fn  <- 'data_frame2023_individual_perfect.csv'
frame_individual_perfect_fn  <- file.path(path_frames, frame_individual_perfect_fn)

diff_rel_household_srswor_geo1_fn <- 'diff_rel_household_srswor_geo1.csv'
diff_rel_household_srswor_geo1_fn <- file.path(
  path_samples, diff_rel_household_srswor_geo1_fn)

diff_rel_household_srswor_geo2_fn <- 'diff_rel_household_srswor_geo2.csv'
diff_rel_household_srswor_geo2_fn <- file.path(
  path_samples, diff_rel_household_srswor_geo2_fn)

diff_rel_household_srswor_urbrur_fn <- 'diff_rel_household_srswor_urbrur.csv'
diff_rel_household_srswor_urbrur_fn <- file.path(
  path_samples, diff_rel_household_srswor_urbrur_fn)

diff_rel_household_syswor_geo1_fn <- 'diff_rel_household_syswor_geo1.csv'
diff_rel_household_syswor_geo1_fn <- file.path(
  path_samples, diff_rel_household_syswor_geo1_fn)

diff_rel_household_syswor_geo2_fn <- 'diff_rel_household_syswor_geo2.csv'
diff_rel_household_syswor_geo2_fn <- file.path(
  path_samples, diff_rel_household_syswor_geo2_fn)

diff_rel_household_syswor_urbrur_fn <- 'diff_rel_household_syswor_urbrur.csv'
diff_rel_household_syswor_urbrur_fn <- file.path(
  path_samples, diff_rel_household_syswor_urbrur_fn)

diff_rel_household_poisson_geo1_fn <- 'diff_rel_household_poisson_geo1.csv'
diff_rel_household_poisson_geo1_fn <- file.path(
  path_samples, diff_rel_household_poisson_geo1_fn)

diff_rel_household_poisson_geo2_fn <- 'diff_rel_household_poisson_geo2.csv'
diff_rel_household_poisson_geo2_fn <- file.path(
  path_samples, diff_rel_household_poisson_geo2_fn)

diff_rel_household_poisson_urbrur_fn <- 'diff_rel_household_poisson_urbrur.csv'
diff_rel_household_poisson_urbrur_fn <- file.path(
  path_samples, diff_rel_household_poisson_urbrur_fn)

# Read household frame ####
frame_household_perfect.dt <- fread(
  frame_household_perfect_fn, sep = ";", 
  colClasses = c('character', 'factor', 'factor', 'numeric', 'factor', 'numeric'))

# Set parameters ####
N_household  <- frame_household_perfect.dt[, .N]
f_household  <- seq(from= 0.001, to= 0.05, length.out= 5)
n_household  <- round(f_household * N_household)

num_samples <- 1000

# Set benchmark figures ####
total_household_geo1.dt <- frame_household_perfect.dt[
  , .(total = .N), by = geo1][
  , freq_frame := total / sum(total)][
  order(as.integer(as.character(geo1)))]
total_household_geo2.dt <- frame_household_perfect.dt[
  , .(total = .N), by = geo2][
  , freq_frame := total / sum(total)][
  order(as.integer(as.character(geo2)))]
total_household_urbrur.dt <- frame_household_perfect.dt[
  , .(total = .N), by = urbrur][
  , freq_frame := total / sum(total)][
  order(as.integer(as.character(urbrur)))]

# Select samples ####
### srswor ####
#### geo1 ####
diff_rel_household_srswor_geo1.dt <- calculate_relative_diff(
  variable = "geo1",
  sampling_method = "srswor",
  frame_data = frame_household_perfect.dt,
  n_values = n_household,
  num_samples = num_samples,
  freq_frame_data = total_household_geo1.dt)


fwrite(
  diff_rel_household_srswor_geo1.dt, 
  file = diff_rel_household_srswor_geo1_fn,
  sep = ";")

plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_srswor_geo1.dt,
  x = "geo1", y = "diff_rel",
  xlab = "Categories of variable geo1",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo1",
  subtitle = "srswor sampling",
  facet_formula = "n ~ ."
)

#### geo2 ####
diff_rel_household_syswor_geo2.dt <- calculate_relative_diff(
  variable = "geo2",
  sampling_method = "srswor",
  frame_data = frame_household_perfect.dt,
  n_values = n_household,
  num_samples = num_samples,
  freq_frame_data = total_household_geo2.dt)

fwrite(
  diff_rel_household_srswor_geo2.dt, 
  file = diff_rel_household_srswor_geo2_fn,
  sep = ";")

diff_rel_household_srswor_geo2.dt[
  , geo1 := ifelse(nchar(as.character(geo2)) == 2, substr(geo2, 1, 1), substr(geo2, 1, 2))]

##### geo1 == 1 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_srswor_geo2.dt[geo1 == 1],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable geo2",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "srswor sampling -- variable geo1 == 1",
  facet_formula = "n ~ ."
)

##### geo1 == 2 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_srswor_geo2.dt[geo1 == 2],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable geo2",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "srswor sampling -- variable geo1 == 2",
  facet_formula = "n ~ ."
)

##### geo1 == 3 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_srswor_geo2.dt[geo1 == 3],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable geo2",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "srswor sampling -- variable geo1 == 3",
  facet_formula = "n ~ ."
)

##### geo1 == 4 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_srswor_geo2.dt[geo1 == 4],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable geo2",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "srswor sampling -- variable geo1 == 4",
  facet_formula = "n ~ ."
)

##### geo1 == 5 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_srswor_geo2.dt[geo1 == 5],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable geo2",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "srswor sampling -- variable geo1 == 5",
  facet_formula = "n ~ ."
)

##### geo1 == 6 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_srswor_geo2.dt[geo1 == 6],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable geo2",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "srswor sampling -- variable geo1 == 6",
  facet_formula = "n ~ ."
)

##### geo1 == 7 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_srswor_geo2.dt[geo1 == 7],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable geo2",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "srswor sampling -- variable geo1 == 7",
  facet_formula = "n ~ ."
)

##### geo1 == 8 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_srswor_geo2.dt[geo1 == 8],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable geo2",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "srswor sampling -- variable geo1 == 8",
  facet_formula = "n ~ ."
)

##### geo1 == 9 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_srswor_geo2.dt[geo1 == 9],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable geo2",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "srswor sampling -- variable geo1 == 9",
  facet_formula = "n ~ ."
)

##### geo1 == 10 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_srswor_geo2.dt[geo1 == 10],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable geo2",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "srswor sampling -- variable geo1 == 10",
  facet_formula = "n ~ ."
)


#### urbrur ####
diff_rel_household_syswor_urbrur.dt <- calculate_relative_diff(
  variable = "urbrur",
  sampling_method = "srswor",
  frame_data = frame_household_perfect.dt,
  n_values = n_household,
  num_samples = num_samples,
  freq_frame_data = total_household_urbrur.dt)

fwrite(
  diff_rel_household_srswor_urbrur.dt, 
  file = diff_rel_household_srswor_urbrur_fn,
  sep = ";")

plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_srswor_urbrur.dt,
  x = "urbrur", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable urbrur",
  subtitle = "srswor sampling",
  facet_formula = "n ~ ."
)


### syswor ####
#### geo1 ####
diff_rel_household_syswor_geo1.dt <- calculate_relative_diff(
  variable = "geo1",
  sampling_method = "syswor",
  frame_data = frame_household_perfect.dt,
  n_values = n_household,
  num_samples = num_samples,
  freq_frame_data = total_household_geo1.dt)

fwrite(
  diff_rel_household_syswor_geo1.dt, 
  file = diff_rel_household_syswor_geo1_fn,
  sep = ";")

plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_syswor_geo1.dt,
  x = "geo1", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo1",
  subtitle = "syswor sampling",
  facet_formula = "n ~ ."
)

#### geo2 ####
diff_rel_household_syswor_geo2.dt <- calculate_relative_diff(
  variable = "geo2",
  sampling_method = "syswor",
  frame_data = frame_household_perfect.dt,
  n_values = n_household,
  num_samples = num_samples,
  freq_frame_data = total_household_geo2.dt)

fwrite(
  diff_rel_household_syswor_geo2.dt, 
  file = diff_rel_household_syswor_geo2_fn,
  sep = ";")

diff_rel_household_syswor_geo2.dt[
  , geo1 := ifelse(nchar(as.character(geo2)) == 2, substr(geo2, 1, 1), substr(geo2, 1, 2))]

##### geo1 == 1 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_syswor_geo2.dt[geo1 == 1],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "syswor sampling -- variable geo1 == 1",
  facet_formula = "n ~ ."
)

##### geo1 == 2 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_syswor_geo2.dt[geo1 == 2],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "syswor sampling -- variable geo1 == 2",
  facet_formula = "n ~ ."
)

##### geo1 == 3 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_syswor_geo2.dt[geo1 == 3],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "syswor sampling -- variable geo1 == 3",
  facet_formula = "n ~ ."
)

##### geo1 == 4 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_syswor_geo2.dt[geo1 == 4],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "syswor sampling -- variable geo1 == 4",
  facet_formula = "n ~ ."
)

##### geo1 == 5 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_syswor_geo2.dt[geo1 == 5],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "syswor sampling -- variable geo1 == 5",
  facet_formula = "n ~ ."
)

##### geo1 == 6 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_syswor_geo2.dt[geo1 == 6],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "syswor sampling -- variable geo1 == 6",
  facet_formula = "n ~ ."
)

##### geo1 == 7 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_syswor_geo2.dt[geo1 == 7],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "syswor sampling -- variable geo1 == 7",
  facet_formula = "n ~ ."
)

##### geo1 == 8 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_syswor_geo2.dt[geo1 == 8],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "syswor sampling -- variable geo1 == 8",
  facet_formula = "n ~ ."
)

##### geo1 == 9 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_syswor_geo2.dt[geo1 == 9],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "syswor sampling -- variable geo1 == 9",
  facet_formula = "n ~ ."
)

##### geo1 == 10 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_syswor_geo2.dt[geo1 == 10],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "syswor sampling -- variable geo1 == 10",
  facet_formula = "n ~ ."
)

#### urbrur ####
diff_rel_household_syswor_urbrur.dt <- calculate_relative_diff(
     variable = "urbrur",
     sampling_method = "syswor",
     frame_data = frame_household_perfect.dt,
     n_values = n_household,
     num_samples = num_samples,
     freq_frame_data = total_household_urbrur.dt)

fwrite(
  diff_rel_household_syswor_urbrur.dt, 
  file = diff_rel_household_syswor_urbrur_fn,
  sep = ";")

plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_syswor_urbrur.dt,
  x = "urbrur", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable urbrur",
  subtitle = "syswor sampling",
  facet_formula = "n ~ ."
)

### poisson ####
#### geo1 ####
diff_rel_household_poisson_geo1.dt <- calculate_relative_diff(
  variable = "geo1",
  sampling_method = "poisson",
  frame_data = frame_household_perfect.dt,
  n_values = n_household,
  num_samples = num_samples,
  freq_frame_data = total_household_geo1.dt,
  pik = frame_household_perfect.dt$hhsize)

fwrite(
  diff_rel_household_poisson_geo1.dt, 
  file = diff_rel_household_poisson_geo1_fn,
  sep = ";")

plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_poisson_geo1.dt,
  x = "geo1", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo1",
  subtitle = "Poisson sampling",
  facet_formula = "n ~ ."
)

#### geo2 ####
diff_rel_household_poisson_geo2.dt <- calculate_relative_diff(
  variable = "geo2",
  sampling_method = "poisson",
  frame_data = frame_household_perfect.dt,
  n_values = n_household,
  num_samples = num_samples,
  freq_frame_data = total_household_geo2.dt,
  pik = frame_household_perfect.dt$hhsize)

fwrite(
  diff_rel_household_poisson_geo2.dt, 
  file = diff_rel_household_poisson_geo2_fn,
  sep = ";")

diff_rel_household_poisson_geo2.dt[
  , geo1 := ifelse(nchar(as.character(geo2)) == 2, substr(geo2, 1, 1), substr(geo2, 1, 2))]

##### geo1 == 1 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_poisson_geo2.dt[geo1 == 1],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "Poisson sampling -- variable geo1 == 1",
  facet_formula = "n ~ ."
)

##### geo1 == 2 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_poisson_geo2.dt[geo1 == 2],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "Poisson sampling -- variable geo1 == 2",
  facet_formula = "n ~ ."
)

##### geo1 == 3 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_poisson_geo2.dt[geo1 == 3],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "Poisson sampling -- variable geo1 == 3",
  facet_formula = "n ~ ."
)

##### geo1 == 4 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_poisson_geo2.dt[geo1 == 4],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "Poisson sampling -- variable geo1 == 4",
  facet_formula = "n ~ ."
)

##### geo1 == 5 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_poisson_geo2.dt[geo1 == 5],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "Poisson sampling -- variable geo1 == 5",
  facet_formula = "n ~ ."
)

##### geo1 == 6 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_poisson_geo2.dt[geo1 == 6],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "Poisson sampling -- variable geo1 == 6",
  facet_formula = "n ~ ."
)

##### geo1 == 7 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_poisson_geo2.dt[geo1 == 7],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "Poisson sampling -- variable geo1 == 7",
  facet_formula = "n ~ ."
)

##### geo1 == 8 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_poisson_geo2.dt[geo1 == 8],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "Poisson sampling -- variable geo1 == 8",
  facet_formula = "n ~ ."
)

##### geo1 == 9 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_poisson_geo2.dt[geo1 == 9],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "Poisson sampling -- variable geo1 == 9",
  facet_formula = "n ~ ."
)

##### geo1 == 10 ####
plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_poisson_geo2.dt[geo1 == 10],
  x = "geo2", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable geo2",
  subtitle = "Poisson sampling -- variable geo1 == 10",
  facet_formula = "n ~ ."
)

#### urbrur ####
diff_rel_household_poisson_urbrur.dt <- calculate_relative_diff(
  variable = "urbrur",
  sampling_method = "poisson",
  frame_data = frame_household_perfect.dt,
  n_values = n_household,
  num_samples = num_samples,
  freq_frame_data = total_household_urbrur.dt,
  pik = frame_household_perfect.dt$hhsize))

fwrite(
  diff_rel_household_poisson_urbrur.dt, 
  file = diff_rel_household_poisson_urbrur_fn,
  sep = ";")

plot_raincloud_relative_diff_popFraction(
  data = diff_rel_household_poisson_urbrur.dt,
  x = "urbrur", y = "diff_rel",
  xlab = "Categories of variable urbrur",
  ylab = bquote(frac(f["sample"] - f["frame"], f["frame"])),
  title = "Distribution of selected units across categories of variable urbrur",
  subtitle = "Poisson sampling",
  facet_formula = "n ~ ."
)

## stsrswor ####