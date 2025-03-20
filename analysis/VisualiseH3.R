rm(list=ls())
gc()
if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}


library(ggplot2)
library(ggridges)
library(rstan)
library(stringr)
source(file = "./analysis/MakeLatexTable.R")

ideo_metric = "score_GPTpol"
group = "party"


fit = readRDS(paste0("./TrainedModels/fitH3squaredcent_",ideo_metric,"_",group,".rds"))
make_stan_summary_table(fit, param_names =c('Intercept','Likes','Retweets', 'IdeologyMetric','IdeologyMetric2'))


df = readRDS("./data/tweets_merge3dwscore.rds")
df$like = scale(log(df$public_metrics.like_count+1))
df$retweet = scale(log(df$public_metrics.retweet_count+1))

df_parties = df %>% group_by(party_author) %>% 
  summarise(CHES = mean(CHES_author), GPTparty = mean(as.numeric(score_GPTparty_author)),
            likes = mean(like), retweets = mean(retweet))



fit_CHES = readRDS(paste0("./TrainedModels/fitH3squaredcent_","CHES","_party.rds"))
fit_GPTparty = readRDS(paste0("./TrainedModels/fitH3squaredcent_","score_GPTparty","_party.rds"))

beta_CHES = rstan::extract(fit_CHES)$beta
beta_GPTparty = rstan::extract(fit_GPTparty)$beta


# Initialize list columns to store posterior distributions
for (r in c("CHES", "GPTparty")) {
  if (r == "CHES") { beta = beta_CHES } else { beta = beta_GPTparty }
  
  # Initialize as lists to store full posterior samples
  df_parties[[paste0(r, "_loggedodds")]] = vector("list", nrow(df_parties))
  df_parties[[paste0(r, "_prob")]] = vector("list", nrow(df_parties))
  
  for (i in 1:nrow(df_parties)) {
    score = df_parties[i, r]
    likes = df_parties[i, "likes"]
    retweets = df_parties[i, "retweets"]
    
    # Compute posterior samples (vector, one per row)
    log_odds_samples = beta[,1] + 
      as.numeric(score) * beta[,4] + as.numeric(score**2) * beta[,5] +  # Quadratic term
      as.numeric(likes) * beta[,2] + as.numeric(retweets) * beta[,3] 
    
    prob_samples <- exp(log_odds_samples) / (exp(log_odds_samples) + 1)  # Convert log-odds to probability
    
    # Store posterior distributions (list of vectors)
    df_parties[[paste0(r, "_loggedodds")]][[i]] = log_odds_samples
    df_parties[[paste0(r, "_prob")]][[i]] = prob_samples
  }
}

df_long = df_parties %>%
  select(party_author, CHES_prob, GPTparty_prob) %>%
  unnest(cols = c(CHES_prob, GPTparty_prob)) %>%
  pivot_longer(cols = c(CHES_prob, GPTparty_prob), names_to = "Rating_Type", values_to = "Probability")


ggplot(df_long, aes(x = Probability, y = reorder(party_author, Probability, FUN = median), fill = Rating_Type)) +
  geom_density_ridges(alpha = 0.4, scale = 1) +  # Ridge plot with transparency
  theme_minimal() +
  labs(x = "Probability of Posting a Negative Partisan Tweet", y = "") +
  scale_fill_manual(values = c("CHES_prob" = "#00BFC4", "GPTparty_prob" = "#F8766D"), 
                    labels = c("CHES_prob" = "CHES", "GPTparty_prob" = "GPT Party")) +  # Change legend labels
  theme(
    strip.text = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),  
    axis.text.x = element_text(size = 12),  
    axis.title.x = element_text(size = 14, color = "black"), 
    legend.position = "top",  
    legend.justification = "center",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, color = "black"),
    panel.spacing.x = unit(2, "lines"),
    strip.text.y.right = element_blank()
  )
ggsave("./plots/H3plot.pdf", width = 8, height = 5, units = "in")

vec_C <- unlist(df_parties[df_parties$party_author == "C", "CHES_prob"])
vec_SD <- unlist(df_parties[df_parties$party_author == "SD", "CHES_prob"])

# Compute element-wise ratio
quantile(x = (vec_SD / vec_C), probs = c(.025,.5,.975))
quantile(x = vec_SD, probs = c(.025,.5,.975))
quantile(x = vec_C, probs = c(.025,.5,.975))


as.vector(df_parties[df_parties$party_author =="C",]$CHES_prob)  as.vector(df_parties[df_parties$party_author =="SD",]$CHES_prob)

