data {
  int<lower=0> N;          // number of observations
  int<lower=0> K;          // number of covariates
  int<lower=1> A;          // number of authors
  int<lower=1> T;          // number of time periods
  matrix[N, K] X;          // covariate matrix
  int<lower=1, upper=A> author_id[N]; // author ID for each observation
  int<lower=1, upper=T> time_id[N];   // time ID for each observation
  vector[N] y;             // outcome variable
}

parameters {
  vector[K] beta;               // coefficients for fixed effects
  real<lower=0> sigma;          // standard deviation for residuals
  vector[A] u_author;           // random effects for authors
  vector[T] u_time;             // random effects for time
  real<lower=0> sigma_author;   // standard deviation for author random effects
  real<lower=0> sigma_time;     // standard deviation for time random effects
}

model {
  // Priors
  beta ~ normal(0, 10);             // weakly informative priors for beta
  sigma ~ cauchy(0, 5);             // Half-Cauchy prior for residual standard deviation
  u_author ~ normal(0, sigma_author); // random effects for authors
  u_time ~ normal(0, sigma_time);     // random effects for time
  sigma_author ~ cauchy(0, 5);      // Half-Cauchy prior for author standard deviation
  sigma_time ~ cauchy(0, 5);        // Half-Cauchy prior for time standard deviation

  // Vectorized Likelihood
  y ~ normal(X * beta + u_author[author_id] + u_time[time_id], sigma);
}
