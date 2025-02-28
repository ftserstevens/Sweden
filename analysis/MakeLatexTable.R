
make_stan_summary_table <- function(stan_fit, probs = c(0.025, 0.5, 0.975), param_names) {
  # Load required packages
  if (!requireNamespace("xtable", quietly = TRUE)) {
    stop("The 'xtable' package is required but not installed. Please install it.")
  }
  
  # Extract posterior samples
  posterior_samples <- as.data.frame(rstan::extract(stan_fit, pars = c("beta")))
  
  # Compute summary statistics
  stan_summary <- summary(stan_fit, probs = probs)$summary
  stan_summary <- stan_summary[grepl("^beta", rownames(stan_summary)), ]
  
  
  # Create a tidy data frame from the summary output
  tidy_summary <- as.data.frame(stan_summary)
  tidy_summary$term <- param_names  # Add parameter names as a column
  
  # Compute P(X >= 0) for each parameter
  p_geq_0 <- sapply(names(posterior_samples), function(param) {
    mean(posterior_samples[[param]] >= 0)
  })
  
  # Ensure alignment between summary and probability calculations
  tidy_summary$`Pgeq0` <- p_geq_0
  
  # Reorder columns
  tidy_summary <- tidy_summary[, c("term",  "2.5%", "50%", "97.5%", "Pgeq0")]
  
  # Generate LaTeX code for the table
  latex_table <- xtable::xtable(tidy_summary, 
                                caption = "Regression Results for Stan Model", 
                                label = "tab:stan_model_results")
  
  xtable::print.xtable(latex_table, type = "latex", include.rownames = FALSE, 
                       sanitize.colnames.function = identity)
}

