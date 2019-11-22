// Linear model with missing values on x1 and x2
// Arseniy Khvorov
// Created 2019/11/22
// Last edit 2019/11/22

data {
  int<lower=1> n;
  int<lower=0> n_x1miss;
  int<lower=1> ind_x1miss[n_x1miss];
  int<lower=0> n_x2miss;
  int<lower=1> ind_x2miss[n_x2miss];
  int<lower=0> n_ymiss;
  int<lower=1> ind_ymiss[n_ymiss];
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
  vector[n_x2miss] x2_imp;
  vector[n_ymiss] y_imp;
}

transformed parameters {
  vector[n] x1_reconst;
  vector[n] x2_reconst;
  vector[n] y_reconst;
  x1_reconst = x1;
  x1_reconst[ind_x1miss] = x1_imp;
  x2_reconst = x2;
  x2_reconst[ind_x2miss] = x2_imp;
  y_reconst = y;
  y_reconst[ind_ymiss] = y_imp;
}

model {
  b0 ~ normal(0, 100);
  b1 ~ normal(0, 100);
  b2 ~ normal(0, 100);
  sigma ~ exponential(0.1);
  x1_imp ~ normal(0, 5);
  x2_imp ~ normal(10, 3);
  y_imp ~ normal(50, 50);
  y_reconst ~ normal(b0 + b1 * x1_reconst + b2 * x2_reconst, sigma);
}
