#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#                                                                              #
# This script illustrates the core concepts of sampling designs to a toy       #
# finite population of N=3 units.                                              #
#                                                                              #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# Load packages ####
library(here)
library(data.table)


# Set relative paths ####
path_project <- here()
path_src     <- file.path(path_project, 'src')


# Load src functions ####
source(file.path(path_src, "illustrate_sampling_properties.R"))

# Set parameters ####
N <- 3
samples_prob <- c(1/7, 1/14, 1/14, 3/14, 4/14, 1/7, 1/14)

n_iter <- 10000

# Sample representation ####
## Vector notation ####
s_vector <- integer(N)
while (sum(s_vector) == 0) {
  s_vector <- sample(c(0L, 1L), N, replace = TRUE)  ## Arbitrary sample assignment
}

## Set notation ####
s_set <- which(s_vector == 1)

# Sampling design ####
## Possible samples ####
(samples_possible <- generate_all_possible_samples(N))

## Sampling design ####
(samples_prob <- define_sampling_design(samples_possible, samples_prob))

## Sample selection ####
# We select n_iter samples to illustrate empirically the statistical properties
(sample_iter <- simulate_sample_selection(samples_possible, samples_prob, n_iter) )

# Inclusion probabilities ####
(inclusion_probs <- calculate_inclusion_probabilities(samples_possible, samples_prob, samples_iter, N))


# Sampling size ####
(samples_size <- rowSums(samples_possible))

# Statistical properties ####
(sample_size_properties <- analyze_sample_size_properties(samples_possible, samples_prob, samples_iter, inclusion_probs))

## Mean value of sample size: theoretical and empirical ####
c(sample_size_properties$size_mean['empirical'], formula = sum(pik_empirical))
c(sample_size_properties$size_mean['theoretical'], formula = sum(pik_theoretical))

## Variance of sample size: theoretical and empirical ####
c(sample_size_properties$size_variance['theoretical'], formula = sum(sample_size_properties$Deltakl$theoretical))

c(sample_size_properties$size_variance['empirical'], formula = sum(sample_size_properties$Deltakl$empirical))

## Different number of iterations ####
n_iters <- c(1e2, 1e3, 1e4, 1e5, 1e6)
analysis.lst <- list()
for (iter in n_iters){

  cat("iter= ", iter, "...")
  analysis.lst[[as.character(iter)]] <- run_sampling_analysis(3, iter, probs = samples_prob)
  cat(" ok.\n")
}

## Convergence of empirical first-order inclusion probabilities ####
pik_emp.mat <- Reduce(rbind, lapply(analysis.lst, function(lst){
  lst[['inclusion_probabilities']][["pik_empirical"]]}))
rownames(pik_emp.mat) <- n_iters
pik_emp.dt <- rbindlist(lapply(1:N, function(i){
  
  data.table(n_iter = n_iters, unit = as.character(i), pik_emp = pik_emp.mat[, i])
  
}))

pik_theor.mat <- Reduce(rbind, lapply(analysis.lst, function(lst){
  lst[['inclusion_probabilities']][["pik_theoretical"]]}))
rownames(pik_theor.mat) <- n_iters
pik_theor.dt <- rbindlist(lapply(1:N, function(i){
  
  data.table(n_iter = n_iters, unit = as.character(i), pik_theor = pik_theor.mat[, i])

}))

pik.dt <- merge(pik_theor.dt, pik_emp.dt, by = c('n_iter', 'unit'), all = TRUE)
pik.dt <- melt(pik.dt, id.vars = c('n_iter', 'unit'))

ggplot(pik.dt, aes(x= log10(n_iter), y = value, colour = variable)) +
  geom_point(size = 3) +
  geom_line(size = 2) +
  facet_grid(unit ~ ., scales = "free") +
  labs(x= "Number of selected samples (log10)", y= "Value\n", title= "Convergence of empirical first-order inclusion probabilities") +
  theme_bw() +
  theme(plot.title = element_text(size= 14, hjust = 0.5))
