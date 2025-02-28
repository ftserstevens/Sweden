rm(list=ls())
gc()
if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}


library(ggplot2)
library(ggridges)
library(rstan)
library(stringr)
source(file = "./analysis/MakeLatexTable.R")

ideo_metric = "coord1D"
group = "bloc"

fit = readRDS(paste0("./TrainedModels/fitH3squared_",ideo_metric,"_",group,".rds"))
fit
make_stan_summary_table(fit, 
                           param_names =
                             c('Intercept','Likes','Retweets','IdeologyMetric','IdeologyMetric2'))
