rm(list=ls())
gc()
if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}

library(tidyverse)

df = readRDS("./data/tweets_merge3dwscore.rds")
df$created_at = as.Date(df$created_at)
df$n =1
df1= df %>% group_by(created_at) %>% summarise(n = sum(n))

ggplot(df1, aes(x = created_at, y = n)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) +  # Add smoothed line
  labs(x = "Date", y = "Number of Tweets Posted") +
  theme_bw() +
  geom_vline(xintercept = as.Date("2022-09-11"), color = "red", alpha = .65, size = 1.2) + 
  annotate("text", x = as.Date("2022-09-11"), y = 300, label = "Elections", color = "red", vjust = +1.5, hjust = .6, angle = 90)
ggsave("./plots/TweetsbyDate.pdf", width = 8, height = 8, units = "cm")


df2 = df %>% group_by(author_id) %>% summarise(n = sum(n)) %>% arrange(desc(n))
ggplot(df2, aes(x = 1:nrow(df2), y = n)) +
  geom_line() +
  labs(x = "Account", y = "Number of Tweets Posted") +
  theme_bw()
ggsave("./plots/TweetsbyAuthor.pdf", width = 8, height = 8, units = "cm")


ggplot(df2, aes(x = n)) +
  geom_density(alpha = 0.2, color = "black") +
  theme_bw() +
  labs(x = "Number of Tweets Posted", y = "Frequency", title = "Distribution of Tweet Counts")
ggsave("./plots/TweetsPostingDist.pdf", width = 8, height = 8, units = "cm")

