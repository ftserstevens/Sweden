rm(list=ls())
gc()
if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}

library(corrplot)
library(Hmisc)

source("./analysis/CalculateExtremity.R")


df = readRDS("./data/tweets_merge3dwscore.rds")
df = df[!is.na(df$coord1D_author),]
X = df[,c("score_GPTpol_author", "score_GPTparty_author",  "CHES_author", "coord1D_author")]
X <- data.frame(apply(X, 2, function(x) {as.numeric(as.character(x))}))# Convert each column to numeric

colnames(X) = c('GPT Poltician','GPT Party','CHES','DW-NOMINATE')


# Calculate the correlation matrix
cor_matrix <- cor(X)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, 
         addCoef.col = "black")  # Plot the upper triangle with correlation coefficients
