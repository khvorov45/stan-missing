// Binary model with no missing value handling
// Arseniy Khvorov
// Created 2019/11/22
// Last edit 2019/11/22

data {
  int<lower=1> n;
  int<lower=0,upper=1> y[n];
  vector[n] x1;
  vector[n] x2;
}

parameters {
  real b0;
  real b1;
  real b2;
}

model {
  b0 ~ normal(0, 100);
  b1 ~ normal(0, 100);
  b2 ~ normal(0, 100);
  y ~ bernoulli_logit(b0 + b1 * x1 + b2 * x2);
}
