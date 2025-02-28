data {
  int<lower=0> y_np;    // Number of NP events (tweets classified as NP)
  int<lower=0> y_pp;    // Number of PP events (tweets classified as PP)
  real<lower=0> t;      // Total number of tweets (exposure period)
}

parameters {
  real<lower=0> lambda_np;  // Rate of NP events
  real<lower=0> lambda_pp;  // Rate of PP events
}

model {
  // Jeffreys prior for lambda: p(λ) ∝ 1/√λ
  target += -0.5 * log(lambda_np);
  target += -0.5 * log(lambda_pp);

  // Poisson likelihood for NP and PP events
  y_np ~ poisson(lambda_np * t);  // NP event count
  y_pp ~ poisson(lambda_pp * t);  // PP event count
}

generated quantities {
  real rate_ratio;   // Rate ratio of NP to PP
  real rate_diff;    // Rate difference of NP to PP

  // Compute rate comparisons
  rate_ratio = lambda_np / lambda_pp;
  rate_diff = lambda_np - lambda_pp;
}
