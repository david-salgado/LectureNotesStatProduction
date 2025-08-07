# Load packages ####
library(data.table)

# Set parameters ####
N <- 3
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
samples_possible <- as.matrix(
  expand.grid(
    lapply(1:N, function(i) c(0,1))
  )
)
colnames(samples_possible) <- paste0('u', 1:N)
samples_possible <- samples_possible[-1, ] # We remove the empty sample
rownames(samples_possible) <- paste0("sample_", 1:nrow(samples_possible))

## Sampling design ####
samples_prob <- c(1/7, 1/14, 1/14, 3/14, 4/14, 1/7, 1/14)
names(samples_prob) <- rownames(samples_possible)
all(samples_prob >= 0)            # check nonnegative probabilities
abs(sum(samples_prob) - 1) < 1e-9 # check sum all probabilities

## Sample selection ####
# We select n_iter samples to illustrate empirically the statistical properties
sample_indicator <- sample(1:nrow(samples_possible), size = n_iter, replace = TRUE, prob = samples_prob)
samples_iter <- Reduce(rbind, lapply(sample_indicator, function(i){t(samples_possible[i,])}))


# Inclusion probabilities ####
## First order ##
pik_theoretical <- sapply(1:N, function(i){sum(samples_possible[, i] * samples_prob)})
pik_empirical <- colMeans(samples_iter)

## Second order ##
unit_pairs.dt <- as.data.table(expand.grid(k = 1:N, l = 1:N))[
  k < l][
  , pikl := sum(samples_possible[, k] * samples_possible[, l] * samples_prob), by = c('k','l')]
pikl_theoretical <- matrix(numeric(N^2), nrow = N)
for (kidx in 1:N){
  for (lidx in 1:N){
    if (kidx > lidx) { pikl_theoretical[kidx, lidx] <- pikl_theoretical[lidx, kidx] } else {
        
      pikl_theoretical[kidx, lidx] <- ifelse(kidx == lidx, pik_theoretical[kidx], unit_pairs.dt[k == kidx & l== lidx, pikl])
    }
  }
}

pikl_empirical  <- matrix(numeric(N^2), nrow = N)
for (k in 1:N){
  for (l in 1:N){
    if (k > l) pikl_empirical[k, l] <- pikl_empirical[l, k]
    pikl_empirical[k, l] <- ifelse(k == l, pik_empirical[k], sum(samples_iter[, k] * samples_iter[, l]) / n_iter)
  }
}

# Sampling size ####
samples_size <- rowSums(samples_possible)

# Statistical properties ####
## Mean value of sample size: theoretical and empirical ####
sample_size_mean_theoretical <- sum(samples_size * samples_prob)
sample_size_mean_empirical <- mean(apply(samples_iter, 1, sum))
c(sample_size_mean_empirical, sum(pik_empirical))
c(sample_size_mean_theoretical, sum(pik_theoretical))

## Variance of sample size: theoretical and empirical ####
sample_size_var_theoretical <- sum(samples_prob * (samples_size - sample_size_mean_theoretical)^2)  
sample_size_var_empirical <- (n_iter - 1 ) / n_iter * var(apply(samples_iter, 1, sum))
Deltakl_theoretical <- pikl_theoretical - pik_theoretical %*% t(pik_theoretical)
c(sample_size_var_theoretical, sum(Deltakl_theoretical))
Deltakl_empirical <- pikl_empirical - pik_empirical %*% t(pik_empirical)
c(sample_size_var_empirical, sum(Deltakl_empirical))






