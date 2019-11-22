# Simulated data
# Arseniy Khvorov
# Created 2019/11/22
# Last edit 2019/11/22

library(tidyverse)
library(extraDistr)

# Directories to be used lated
data_dir <- "data"

# Functions ===================================================================

# Simulate linear data
sim_lin <- function(nsam, b0, b1, b2, res_sd,
                    seed = sample.int(.Machine$integer.max, 1)) {
  set.seed(seed)
  tibble(
    x1 = rnorm(nsam, 0, 5),
    x2 = rnorm(nsam, 10, 3),
    y = rnorm(nsam, b0 + b1 * x1 + b2 * x2, res_sd)
  )
}

sim_bin <- function(nsam, b0, b1, b2,
                    seed = sample.int(.Machine$integer.max, 1)) {
  set.seed(seed)
  tibble(
    x1 = rnorm(nsam, 0, 2),
    x2 = rnorm(nsam, 1, 1.5),
    probs = 1 - 1 / (1 + exp(b0 + b1 * x1 + b2 * x2)),
    y = rbern(nsam, probs)
  )
}

# Adds missing values to generated data
add_miss <- function(dat, miss_y, miss_x1, miss_x2,
                     seed = sample.int(.Machine$integer.max, 1)) {
  set.seed(seed)
  dat %>%
    mutate(
      x1_miss = rbern(n(), miss_x1),
      x1_obs = if_else(as.logical(x1_miss), NA_real_, x1),
      x2_miss = rbern(n(), miss_x2),
      x2_obs = if_else(as.logical(x2_miss), NA_real_, x2),
      y_miss = rbern(n(), miss_y),
      y_obs = if_else(as.logical(y_miss), NA_real_, y),
    ) %>%
    select(-contains("_miss"))
}

# Saves the csv
save_csv <- function(dat, name, folder) {
  write_csv(dat, file.path(folder, paste0(name, ".csv")))
}

# Script ======================================================================

lin_nomiss <- sim_lin(300, -3, 2, 5, 3, 20191122) %>% add_miss(0, 0, 0)
save_csv(lin_nomiss, "lin_nonmiss", data_dir)
lin_x1miss <- lin_nomiss %>% add_miss(0, 0.1, 0, 20191122)
save_csv(lin_x1miss, "lin_x1miss", data_dir)
lin_x1x2miss <- lin_nomiss %>% add_miss(0, 0.1, 0.1, 20191122)
save_csv(lin_x1x2miss, "lin_x1x2miss", data_dir)
lin_x1x2ymiss <- lin_nomiss %>% add_miss(0.1, 0.1, 0.1, 20191122)
save_csv(lin_x1x2ymiss, "lin_x1x2ymiss", data_dir)

bin_nomiss <- sim_bin(300, -5, 1.5, 1.5, 20191122) %>% add_miss(0, 0, 0)
save_csv(bin_nomiss, "bin_nonmiss", data_dir)
bin_x1miss <- bin_nomiss %>% add_miss(0, 0.1, 0, 20191122)
save_csv(bin_x1miss, "bin_x1miss", data_dir)
bin_x1x2miss <- bin_nomiss %>% add_miss(0, 0.1, 0.1, 20191122)
save_csv(bin_x1x2miss, "bin_x1x2miss", data_dir)
bin_x1x2ymiss <- bin_nomiss %>% add_miss(0.1, 0.1, 0.1, 20191122)
save_csv(bin_x1x2ymiss, "bin_x1x2ymiss", data_dir)
