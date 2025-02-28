data {
  int<lower=0> N;          // number of observations
  int<lower=0> K;          // number of covariates
  matrix[N, K] X;          // covariate matrix
  int<lower=0, upper=1> y[N];  // binary outcome variable
}

parameters {
  vector[K] beta;          // coefficients
}

model {
  // Priors
  beta ~ normal(0, 5);    // weakly informative priors for beta
  
  // Likelihood
  y ~ bernoulli_logit(X * beta);  // Logistic regression
}
