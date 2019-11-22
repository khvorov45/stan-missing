// Binary model with missing values on x1, x2 and y
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
  int<lower=0,upper=1> y_is_miss[n];
  int<lower=0,upper=1> y[n];
  vector[n] x1;
  vector[n] x2;
}

parameters {
  real b0;
  real b1;
  real b2;
  vector[n_x1miss] x1_imp;
  vector[n_x2miss] x2_imp;

}

transformed parameters {
  vector[n] x1_reconst;
  vector[n] x2_reconst;
  vector[n] y_pred;
  x1_reconst = x1;
  x1_reconst[ind_x1miss] = x1_imp;
  x2_reconst = x2;
  x2_reconst[ind_x2miss] = x2_imp;
  y_pred = b0 + b1 * x1_reconst + b2 * x2_reconst;
}

model {
  b0 ~ normal(0, 10);
  b1 ~ normal(0, 10);
  b2 ~ normal(0, 10);
  x1_imp ~ normal(0, 2);
  x2_imp ~ normal(1, 1.5);
  for (i in 1:n) {
    if (y_is_miss[i] == 1) {
      target += log_mix(
        inv_logit(y_pred[i]),
        bernoulli_logit_lpmf(1 | y_pred[i]),
        bernoulli_logit_lpmf(0 | y_pred[i])
      );
    } else {
      target += bernoulli_logit_lpmf(y[i] | y_pred[i]);
    }
  }
}
