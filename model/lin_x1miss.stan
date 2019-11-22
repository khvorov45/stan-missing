// Linear model with missing values on x1
// Arseniy Khvorov
// Created 2019/11/22
// Last edit 2019/11/22

data {
  int<lower=1> n;
  int<lower=0> n_x1miss;
  int<lower=1> ind_x1miss[n_x1miss];
  vector[n] y;
  vector[n] x1;
  vector[n] x2;
}

parameters {
  real b0;
  real b1;
  real b2;
  real<lower=0> sigma;
  vector[n_x1miss] x1_imp;
}

transformed parameters {
  vector[n] x1_reconst;
  x1_reconst = x1;
  x1_reconst[ind_x1miss] = x1_imp;
}

model {
  b0 ~ normal(0, 100);
  b1 ~ normal(0, 100);
  b2 ~ normal(0, 100);
  sigma ~ exponential(0.1);
  x1_imp ~ normal(0, 5);
  y ~ normal(b0 + b1 * x1_reconst + b2 * x2, sigma);
}
