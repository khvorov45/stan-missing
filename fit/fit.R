# Simulated data
# Arseniy Khvorov
# Created 2019/11/22
# Last edit 2019/11/22

library(rstan)
library(tidyverse)

rstan_options(auto_write = TRUE)

model_dir <- "model"
data_dir <- "data"

# Functions ===================================================================

fit_lin <- function(lin_data, model_compiled) {
  replace_miss <- function(vec) {
    vec[is.na(vec)] <- -1000
    vec
  }
  sampling(
    model_compiled,
    data = list(
      n = nrow(lin_data),
      y = replace_miss(lin_data$y_obs),
      x1 = replace_miss(lin_data$x1_obs),
      x2 = replace_miss(lin_data$x2_obs),
      n_x1miss = sum(is.na(lin_data$x1_obs)),
      n_x2miss = sum(is.na(lin_data$x2_obs)),
      n_ymiss = sum(is.na(lin_data$y_obs)),
      ind_x1miss = which(is.na(lin_data$x1_obs)),
      ind_x2miss = which(is.na(lin_data$x2_obs)),
      ind_ymiss = which(is.na(lin_data$y_obs))
    ),
    cores = 1,
    chains = 2,
    iter = 5e3
  )
}

fit_bin <- function(bin_data, model_compiled) {
  replace_miss <- function(vec) {
    vec[is.na(vec)] <- 0
    vec
  }
  sampling(
    model_compiled,
    data = list(
      n = nrow(bin_data),
      y = replace_miss(bin_data$y_obs),
      x1 = replace_miss(bin_data$x1_obs),
      x2 = replace_miss(bin_data$x2_obs),
      n_x1miss = sum(is.na(bin_data$x1_obs)),
      n_x2miss = sum(is.na(bin_data$x2_obs)),
      n_ymiss = sum(is.na(bin_data$y_obs)),
      ind_x1miss = which(is.na(bin_data$x1_obs)),
      ind_x2miss = which(is.na(bin_data$x2_obs)),
      y_is_miss = as.integer(is.na(bin_data$y_obs))
    ),
    cores = 1,
    chains = 2,
    iter = 5e3
  )
}

summ_fit <- function(fit) {
  nms <- names(fit)[names(fit) != "lp__"]
  nms <- nms[!str_detect(nms, "\\[")]
  summ <- summary(fit, pars = nms)$summary
  as_tibble(summ) %>%
    mutate(term = rownames(summ)) %>%
    select(term, everything())
}

logit <- function(x) log(x / (1 - x))

# Script ======================================================================

# No missing
lin_nonmiss_data <- read_csv(file.path(data_dir, "lin_nonmiss.csv"))
lin_nonmiss_comp <- stan_model(file.path(model_dir, "lin_nonmiss.stan"))

lin_nonmiss_fit <- fit_lin(lin_nonmiss_data, lin_nonmiss_comp)
summ_fit(lin_nonmiss_fit)

# x1 missing
lin_x1miss_data <- read_csv(file.path(data_dir, "lin_x1miss.csv"))
lin_x1miss_comp <- stan_model(file.path(model_dir, "lin_x1miss.stan"))

lin_x1miss_fit <- fit_lin(lin_x1miss_data, lin_x1miss_comp)
summ_fit(lin_x1miss_fit)

# x1 and x2 missing
lin_x1x2miss_data <- read_csv(file.path(data_dir, "lin_x1x2miss.csv"))
lin_x1x2miss_comp <- stan_model(file.path(model_dir, "lin_x1x2miss.stan"))

lin_x1x2miss_fit <- fit_lin(lin_x1x2miss_data, lin_x1x2miss_comp)
summ_fit(lin_x1x2miss_fit)

# x1, x2 and y missing
lin_x1x2ymiss_data <- read_csv(file.path(data_dir, "lin_x1x2ymiss.csv"))
lin_x1x2ymiss_comp <- stan_model(file.path(model_dir, "lin_x1x2ymiss.stan"))

lin_x1x2ymiss_fit <- fit_lin(lin_x1x2ymiss_data, lin_x1x2ymiss_comp)
summ_fit(lin_x1x2ymiss_fit)

# Binary no missing
bin_nonmiss_data <- read_csv(file.path(data_dir, "bin_nonmiss.csv"))
bin_nonmiss_comp <- stan_model(file.path(model_dir, "bin_nonmiss.stan"))

bin_nonmiss_fit <- fit_bin(bin_nonmiss_data, bin_nonmiss_comp)
summ_fit(bin_nonmiss_fit)

# Binary with x1, x2 and y missing
bin_x1x2ymiss_data <- read_csv(file.path(data_dir, "bin_x1x2ymiss.csv"))
bin_x1x2ymiss_comp <- stan_model(file.path(model_dir, "bin_x1x2ymiss.stan"))

bin_x1x2ymiss_fit <- fit_bin(bin_x1x2ymiss_data, bin_x1x2ymiss_comp)
summ_fit(bin_x1x2ymiss_fit)

y_pred <- rstan::extract(bin_x1x2ymiss_fit, "y_pred")$y_pred
logit(bin_x1x2ymiss_data$probs[[1]])
hist(y_pred[, 1], breaks = 100)
