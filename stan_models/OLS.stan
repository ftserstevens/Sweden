data {
  int<lower=0> N;          // number of observations
  int<lower=0> K;          // number of covariates
  matrix[N, K] X;          // covariate matrix
  vector[N] y;             // outcome variable
}

parameters {
  vector[K] beta;          // coefficients
  real<lower=0> sigma;     // standard deviation
}

model {
  // Priors
  beta ~ normal(0, 10);    // weakly informative priors for beta
  sigma ~ cauchy(0, 5);      // Half-Cauchy prior for standard deviation

  // Likelihood
  y ~ normal(X * beta, sigma); // Linear model 
}
