rm(list=ls())
gc()

library(fastDummies)
library(tidyverse)
library(rstan)


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}
df = readRDS("./data/tweets_merge3dwscore.rds")



# change paremeters for correct regression --------------------------------

sent_metric = "sentiment_openai_sw"
reaction = 'retweets' #or "likes|retweets"
group = "party" #or 'bloc|party


# -------------------------------------------------------------------------




if(reaction== "retweets") {y <- df$public_metrics.retweet_count}
if(reaction== "likes") {y <- df$public_metrics.like_count}

reference_var <- if (group == "party") "S" else "left-bloc"
group_var <- if (group == "party") df$party_author else df$bloc_author
target_var <- if (group == "party") df$target_inparty else df$target_inbloc
df$sentiment = df[,sent_metric]


# Create matrix X dynamically
X <- as.matrix(
  cbind(
    intercept = 1,
    group = relevel(factor(group_var), ref = reference_var),
    target_out = !target_var,
    sentiment = relevel(factor(df$sentiment), ref = "NEUTRAL"),
    out_negative = !target_var & df$sentiment == "NEGATIVE",
    in_positive = target_var & df$sentiment == "POSITIVE"
  )
)


# dummy coding
X <- as.data.frame(X) %>% 
  mutate(across(c(sentiment, group), as.factor)) %>%  # Convert to factors for dummy coding
  fastDummies::dummy_cols(remove_selected_columns = TRUE, remove_first_dummy = TRUE) %>% #dummy codes
  as.matrix()  # Convert back to matrix

colnames(X)[grep("^group_", colnames(X))] <- paste("group", levels(relevel(factor(group_var), ref = reference_var))[-1], sep = "_")
colnames(X)[grep("^sentiment_", colnames(X))] <- paste("sentiment", levels(relevel(factor(df$sentiment), ref = "NEUTRAL"))[-1], sep = "_")


stan_data = list(
  y = log(1 + as.numeric(y)),
  X = X,
  K = ncol(X),
  N =nrow(X),
  author_id = as.numeric(factor(df$author_id)),
  A = length(unique(df$author_id)),
  time_id = as.numeric(factor(df$timeofday)),
  T = length(unique(df$time))
)


stan_fit <- stan(file = "./stan_models/OLS_reffect.stan", 
                 data = stan_data, 
                 iter = 2000, 
                 chains = 8, 
                 seed = 123)
saveRDS(object = stan_fit, file = paste0("./TrainedModels/fitH2_",reaction,"_",group,".rds"))
print(stan_fit, probs = c(0.025, 0.5, 0.975))



colnames(X)
