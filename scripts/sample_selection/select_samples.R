# Load packages ####
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

# Read frames ####
## Household ####
frame_household_perfect.dt <- fread(
  frame_household_perfect_fn, sep = ";", 
  colClasses = c('character', 'factor', 'factor', 'numeric', 'factor'))

# Set parameters ####
N_household  <- frame_household_perfect.dt[, .N]
f_household  <- seq(from= 0.001, to= 0.05, length.out= 5)
n_household  <- round(f_household * N_household)

N_individual <- frame_individual_perfect.dt[, .N]
f_individual <- seq(from= 0.001, to= 0.05, length.out= 5)
n_individual <- round(f_individual * N_individual)

num_samples <- 100

# Set benchmark figures ####
## Household ####
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
### Household ####
#### srswor ####
##### geo1 ####
geo1_levels <- sort(as.integer(levels(frame_household_perfect.dt$geo1)))
diff_rel_household_srswor_geo1.dt <- data.table(
  geo1 = factor(character(0), levels = geo1_levels), 
  n = integer(0), 
  iter = integer(0), 
  diff_rel = numeric(0))
for (n in n_household){
  cat(paste0("n= ", n, '...'))
  for (i in 1:num_samples){
    frame_household_perfect.dt[, s := srswor(n = n, N = N_household)]
    total_household_sample_geo1.dt <- frame_household_perfect.dt[
      , .(total_sample = sum(s)), by = geo1][
      order(geo1)]
    aux.dt <- merge(
      total_household_geo1.dt, total_household_sample_geo1.dt, by = 'geo1')[
      , auxvar := total_sample / sum(total_sample)][
      , diff_rel := (auxvar - freq_frame) / freq_frame][
      , n := n][
      , iter := i][
      , .(geo1, n, iter, diff_rel)]
    diff_rel_household_srswor_geo1.dt <- rbindlist(list(diff_rel_household_srswor_geo1.dt, aux.dt))
  }
  cat(" ok.\n")
}

fwrite(
  diff_rel_household_srswor_geo1.dt, 
  file = diff_rel_household_srswor_geo1_fn,
  sep = ";")

ggplot(diff_rel_household_srswor_geo1.dt, aes(x = geo1, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of geo1",
       title = 'Distribution of selected units across categories of variable geo1',
       subtitle = 'srswor sampling') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

##### geo2 ####
geo2_levels <- sort(as.integer(levels(frame_household_perfect.dt$geo2)))
diff_rel_household_srswor_geo2.dt <- data.table(
  geo2 = factor(character(0), levels = geo2_levels), 
  n = integer(0), 
  iter = integer(0), 
  diff_rel = numeric(0))
for (n in n_household){
  cat(paste0("n= ", n, '...'))
  for (i in 1:num_samples){
    frame_household_perfect.dt[, s := srswor(n = n, N = N_household)]
    total_household_sample_geo2.dt <- frame_household_perfect.dt[
      , .(total_sample = sum(s)), by = geo2][
      order(geo2)]
    aux.dt <- merge(
      total_household_geo2.dt, total_household_sample_geo2.dt, by = 'geo2')[
        , auxvar := total_sample / sum(total_sample)][
        , diff_rel := (auxvar - freq_frame) / freq_frame][
        , n := n][
        , iter := i][
        , .(geo2, n, iter, diff_rel)]
    diff_rel_household_srswor_geo2.dt <- rbindlist(list(diff_rel_household_srswor_geo2.dt, aux.dt))
  }
  cat(" ok.\n")
}

fwrite(
  diff_rel_household_srswor_geo2.dt, 
  file = diff_rel_household_srswor_geo2_fn,
  sep = ";")

diff_rel_household_srswor_geo2.dt[
  , geo1 := ifelse(nchar(as.character(geo2)) == 2, substr(geo2, 1, 1), substr(geo2, 1, 2))]

# geo1 == 1
ggplot(diff_rel_household_srswor_geo2.dt[geo1 == 1], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'srswor sampling -- variable geo1 == 1') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 2
ggplot(diff_rel_household_srswor_geo2.dt[geo1 == 2], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'srswor sampling -- variable geo1 == 2') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 3
ggplot(diff_rel_household_srswor_geo2.dt[geo1 == 3], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'srswor sampling -- variable geo1 == 3') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 4
ggplot(diff_rel_household_srswor_geo2.dt[geo1 == 4], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'srswor sampling -- variable geo1 == 4') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 5
ggplot(diff_rel_household_srswor_geo2.dt[geo1 == 5], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'srswor sampling -- variable geo1 == 5') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 6
ggplot(diff_rel_household_srswor_geo2.dt[geo1 == 6], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'srswor sampling -- variable geo1 == 6') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 7
ggplot(diff_rel_household_srswor_geo2.dt[geo1 == 7], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'srswor sampling -- variable geo1 == 7') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 8
ggplot(diff_rel_household_srswor_geo2.dt[geo1 == 8], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'srswor sampling -- variable geo1 == 8') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 9
ggplot(diff_rel_household_srswor_geo2.dt[geo1 == 9], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'srswor sampling -- variable geo1 == 9') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 10
ggplot(diff_rel_household_srswor_geo2.dt[geo1 == 10], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'srswor sampling -- variable geo1 == 10') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

##### urbrur ####
urbrur_levels <- sort(as.integer(levels(frame_household_perfect.dt$urbrur)))
diff_rel_household_srswor_urbrur.dt <- data.table(
  urbrur = factor(character(0), levels = urbrur_levels), 
  n = integer(0), 
  iter = integer(0), 
  diff_rel = numeric(0))
for (n in n_household){
  cat(paste0("n= ", n, '...'))
  for (i in 1:num_samples){
    frame_household_perfect.dt[, s := srswor(n = n, N = N_household)]
    total_household_sample_urbrur.dt <- frame_household_perfect.dt[
      , .(total_sample = sum(s)), by = urbrur][
        order(urbrur)]
    aux.dt <- merge(
      total_household_urbrur.dt, total_household_sample_urbrur.dt, by = 'urbrur')[
       , auxvar := total_sample / sum(total_sample)][
       , diff_rel := (auxvar - freq_frame) / freq_frame][
       , n := n][
       , iter := i][
       , .(urbrur, n, iter, diff_rel)]
    diff_rel_household_srswor_urbrur.dt <- rbindlist(
      list(diff_rel_household_srswor_urbrur.dt, aux.dt))
  }
  cat(" ok.\n")
}

fwrite(
  diff_rel_household_srswor_urbrur.dt, 
  file = diff_rel_household_srswor_urbrur_fn,
  sep = ";")

ggplot(diff_rel_household_srswor_urbrur.dt, aes(x = urbrur, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable urbrur",
       title = 'Distribution of selected units across categories of variable urbrur',
       subtitle = 'srswor sampling') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))


#### syswor ####
##### geo1 ####
geo1_levels <- sort(as.integer(levels(frame_household_perfect.dt$geo1)))
diff_rel_household_syswor_geo1.dt <- data.table(
  geo1 = factor(character(0), levels = geo1_levels), 
  n = integer(0), 
  iter = integer(0), 
  diff_rel = numeric(0))
for (n in n_household){
  cat(paste0("n= ", n, '...'))
  pik <- rep(n / N_household, times = N_household)
  for (i in 1:num_samples){
    frame_household_perfect.dt[, s := UPsystematic(pik)]
    total_household_sample_geo1.dt <- frame_household_perfect.dt[
      , .(total_sample = sum(s)), by = geo1][
      order(geo1)]
    aux.dt <- merge(
      total_household_geo1.dt, total_household_sample_geo1.dt, by = 'geo1')[
      , auxvar := total_sample / sum(total_sample)][
      , diff_rel := (auxvar - freq_frame) / freq_frame][
      , n := n][
      , iter := i][
      , .(geo1, n, iter, diff_rel)]
    diff_rel_household_syswor_geo1.dt <- rbindlist(list(diff_rel_household_syswor_geo1.dt, aux.dt))
  }
  cat(" ok.\n")
}

fwrite(
  diff_rel_household_syswor_geo1.dt, 
  file = diff_rel_household_syswor_geo1_fn,
  sep = ";")

ggplot(diff_rel_household_syswor_geo1.dt, aes(x = geo1, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of geo1",
       title = 'Distribution of selected units across categories of variable geo1',
       subtitle = 'syswor sampling') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

##### geo2 ####
geo2_levels <- sort(as.integer(levels(frame_household_perfect.dt$geo2)))
diff_rel_household_syswor_geo2.dt <- data.table(
  geo2 = factor(character(0), levels = geo2_levels), 
  n = integer(0), 
  iter = integer(0), 
  diff_rel = numeric(0))
for (n in n_household){
  cat(paste0("n= ", n, '...'))
  pik <- rep(n / N_household, times = N_household)
  for (i in 1:num_samples){
    frame_household_perfect.dt[, s := UPsystematic(pik)]
    total_household_sample_geo2.dt <- frame_household_perfect.dt[
      , .(total_sample = sum(s)), by = geo2][
      order(geo2)]
    aux.dt <- merge(
      total_household_geo2.dt, total_household_sample_geo2.dt, by = 'geo2')[
        , auxvar := total_sample / sum(total_sample)][
        , diff_rel := (auxvar - freq_frame) / freq_frame][
        , n := n][
        , iter := i][
        , .(geo2, n, iter, diff_rel)]
    diff_rel_household_syswor_geo2.dt <- rbindlist(list(diff_rel_household_syswor_geo2.dt, aux.dt))
  }
  cat(" ok.\n")
}

fwrite(
  diff_rel_household_syswor_geo2.dt, 
  file = diff_rel_household_syswor_geo2_fn,
  sep = ";")

diff_rel_household_syswor_geo2.dt[
  , geo1 := ifelse(nchar(as.character(geo2)) == 2, substr(geo2, 1, 1), substr(geo2, 1, 2))]

# geo1 == 1
ggplot(diff_rel_household_syswor_geo2.dt[geo1 == 1], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'syswor sampling -- variable geo1 == 1') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 2
ggplot(diff_rel_household_syswor_geo2.dt[geo1 == 2], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'syswor sampling -- variable geo1 == 2') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 3
ggplot(diff_rel_household_syswor_geo2.dt[geo1 == 3], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'syswor sampling -- variable geo1 == 3') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 4
ggplot(diff_rel_household_syswor_geo2.dt[geo1 == 4], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'syswor sampling -- variable geo1 == 4') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 5
ggplot(diff_rel_household_syswor_geo2.dt[geo1 == 5], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'syswor sampling -- variable geo1 == 5') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 6
ggplot(diff_rel_household_syswor_geo2.dt[geo1 == 6], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'syswor sampling -- variable geo1 == 6') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 7
ggplot(diff_rel_household_syswor_geo2.dt[geo1 == 7], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'syswor sampling -- variable geo1 == 7') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 8
ggplot(diff_rel_household_syswor_geo2.dt[geo1 == 8], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'syswor sampling -- variable geo1 == 8') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 9
ggplot(diff_rel_household_syswor_geo2.dt[geo1 == 9], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'syswor sampling -- variable geo1 == 9') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

# geo1 == 10
ggplot(diff_rel_household_syswor_geo2.dt[geo1 == 10], aes(x = geo2, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable geo2",
       title = 'Distribution of selected units across categories of variable geo2',
       subtitle = 'syswor sampling -- variable geo1 == 10') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))

##### urbrur ####
urbrur_levels <- sort(as.integer(levels(frame_household_perfect.dt$urbrur)))
diff_rel_household_syswor_urbrur.dt <- data.table(
  urbrur = factor(character(0), levels = urbrur_levels), 
  n = integer(0), 
  iter = integer(0), 
  diff_rel = numeric(0))
for (n in n_household){
  cat(paste0("n= ", n, '...'))
  pik <- rep(n / N_household, times = N_household)
  for (i in 1:num_samples){
    frame_household_perfect.dt[, s := UPsystematic(pik)]
    total_household_sample_urbrur.dt <- frame_household_perfect.dt[
      , .(total_sample = sum(s)), by = urbrur][
        order(urbrur)]
    aux.dt <- merge(
      total_household_urbrur.dt, total_household_sample_urbrur.dt, by = 'urbrur')[
        , auxvar := total_sample / sum(total_sample)][
          , diff_rel := (auxvar - freq_frame) / freq_frame][
            , n := n][
              , iter := i][
                , .(urbrur, n, iter, diff_rel)]
    diff_rel_household_syswor_urbrur.dt <- rbindlist(
      list(diff_rel_household_syswor_urbrur.dt, aux.dt))
  }
  cat(" ok.\n")
}

fwrite(
  diff_rel_household_syswor_urbrur.dt, 
  file = diff_rel_household_syswor_urbrur_fn,
  sep = ";")

ggplot(diff_rel_household_syswor_urbrur.dt, aes(x = urbrur, y = diff_rel)) + 
  facet_grid(n ~ ., scales = "free") +
  geom_hline(yintercept = 0, color = "red") +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .2) +
  labs(y = bquote(frac(f["sample"] - f["frame"], f["frame"])), x = "Categories of variable urbrur",
       title = 'Distribution of selected units across categories of variable urbrur',
       subtitle = 'syswor sampling') +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust= 0.5),
        plot.subtitle = element_text(size = 12, face = "bold", hjust= 0.5))
