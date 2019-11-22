// Linear model with no missing value handling
// Arseniy Khvorov
// Created 2019/11/22
// Last edit 2019/11/22

data {
  int<lower=1> n;
  vector[n] y;
  vector[n] x1;
  vector[n] x2;
}

parameters {
  real b0;
  real b1;
  real b2;
  real<lower=0> sigma;
}

model {
  b0 ~ normal(0, 100);
  b1 ~ normal(0, 100);
  b2 ~ normal(0, 100);
  sigma ~ exponential(0.1);
  y ~ normal(b0 + b1 * x1 + b2 * x2, sigma);
}
