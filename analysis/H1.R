library(xtable)
library(tidyverse)
library(rstan)


rm(list=ls())
gc()
if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}
# Rstan Options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Preprocess --------------------------------------------------------------


df = readRDS("./data/tweets_merge3dwscore.rds")
df$sentiment = df$sentiment_openai_sw #change sent metric
df$group = df$target_inbloc # change target group metric

df$pp = df$group & df$sentiment =="POSITIVE"
df$np = !df$group & df$sentiment =="NEGATIVE"

df = df[as.logical(df$np + df$pp),]


# Example data: Total counts for NP and PP and total tweets
y_np <- sum(df$sentiment == "NEGATIVE")
y_pp <- sum(df$sentiment == "POSITIVE")
t <- nrow(df)  # Total number of tweets

# Create a list to pass to Stan
stan_data <- list(
  y_np = y_np,
  y_pp = y_pp,
  t = t
)


# run stan ----------------------------------------------------------------


stan_fit <- stan(file = "/Users/francois/Documents/Sweden/stan_models/poisson.stan", 
                 data = stan_data,
                 iter = 2000, 
                 chains = 8, 
                 seed = 123)

# Print a summary of the results
print(stan_fit, probs = c(0.025, 0.5, 0.975))


stan_summary <- summary(stan_fit)$summary
stan_summary_df <- as.data.frame(stan_summary)
stan_summary_apa <- stan_summary_df[, c("mean", "sd", "2.5%","50%", "97.5%","Rhat","n_eff")]

stan_summary_apa <- cbind(Parameter = rownames(stan_summary_df), stan_summary_apa)
rownames(stan_summary_apa) <- NULL

latex_table <- xtable(stan_summary_apa, 
                      caption = "Summary of Bayesian Model Parameters", 
                      label = "tab:stan_summary")

# Print LaTeX table
print(latex_table, include.rownames = FALSE, booktabs = TRUE, 
      caption.placement = "top", sanitize.colnames.function = identity)

