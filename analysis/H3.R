rm(list=ls())
gc()
if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}


library(fastDummies)
library(tidyverse)
library(rstan)



rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
df = readRDS("./data/tweets_merge3dwscore.rds")


# change paremeters for correct regression --------------------------------
sent_metric = "sentiment_openai_sw"
ideo_metrics = c("score_GPTparty", "score_GPTpol", "CHES", "coord1D")
groups = c("party","bloc")

#for(ideo_metric in ideo_metrics){
#  for (group in groups) {
ideo_metric = 'score_GPTpol'
group = 'party'


if (ideo_metric == "coord1D") {df = df[!is.na(df$coord1D_author),]}

df$sentiment = df[,sent_metric]
df$ideo_metric = as.numeric(df[,paste0(ideo_metric,"_author")])

X <- as.matrix(
  cbind(
    intercept = 1,
    likes = scale(log(1+ df$public_metrics.like_count)), #scale here for prior relevance in model. 
    retweets = scale(log(1+ df$public_metrics.retweet_count)),
    #ideo_metric = abs(df$ideo_metric)
    ideo_metric = df$ideo_metric,
    ideo_metric2 = (df$ideo_metric)**2
  )
)


stan_data = list(
  y = if(group == "party") {as.numeric(df$sentiment == "NEGATIVE" & !df$target_inparty)} else
    if(group == "bloc")  {y <- as.numeric(df$sentiment == "NEGATIVE" & !df$target_inbloc)},
  X = X,
  K = ncol(X),
  N = nrow(X),
  author_id = as.numeric(factor(df$author_id)),
  A = length(unique(df$author_id)),
  time_id = as.numeric(factor(df$timeofday)),
  T = length(unique(df$time))
)

summary(lm(data = stan_data, y ~ X -1))


stan_fit <- stan(file = "./stan_models/binlogit_reffect.stan", 
                 data = stan_data, 
                 iter = 2000, 
                 chains = 8, 
                 seed = 123)
saveRDS(object = stan_fit, file = paste0("./TrainedModels/fitH3squaredcent_",ideo_metric,"_",group,".rds"))
print(stan_fit, probs = c(0.025, 0.5, 0.975))



# }
#}
