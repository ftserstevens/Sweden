rm(list=ls())
gc()
if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}

library(dplyr)

df = read.csv("./get_sweparlimentscore/CHES.csv")
df = df %>%
  filter(country == 16) %>%
  select(party, lrgen)

df$lrgen = (df$lrgen -5)/5

saveRDS(df, "./get_sweparlimentscore/CHES_score.rds")
