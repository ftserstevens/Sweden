rm(list=ls())
gc()

# load utils
library(data.table)
library(tidyverse)

if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}

cat("Calculate Sentiment before running this script:\n",  
    "./TweetSentiment/sentiment_scoring.ipynb")

translate = read.csv("./TweetSentiment/translate.csv")
translated = read.csv("./TweetSentiment/translated.csv")
table(translate$sw_text %in% translated$sw_text)


###merge external info
scored_en = read.csv("./TweetSentiment/sentiment_scored_en.csv")
scored_sw = read.csv("./TweetSentiment/sentiment_scored_sw_KBL.csv")
scored_gpt = read.csv("./TweetSentiment/sentiment_scored_gpt.csv")

#https://huggingface.co/facebook/nllb-200-distilled-600M #translation
#https://huggingface.co/cardiffnlp/twitter-roberta-base-sentiment-latest # sentiment
#https://huggingface.co/marma/bert-base-swedish-cased-sentiment

#change format of en scores
en_sentiment_cols = c("negative_score_en", "neutral_score_en", "positive_score_en")
scored_en$sentiment_en <- apply(scored_en[en_sentiment_cols], 1, function(row) {
  en_sentiment_cols[which.max(row)]})
scored_en$sentiment_en <- toupper(gsub("_score_en", "", scored_en$sentiment_en))
scored_en = scored_en %>% select(!en_sentiment_cols)

#remove unused col
scored_sw = scored_sw %>% select(!score_sw)

#merge all df's
scored = merge(scored_en, scored_sw, by = c("sw_text","en_translated_text"))
scored = merge(scored, scored_gpt, by = c("sw_text","en_translated_text"))

#remove whre GPT got congused in en (no issues in Sw -2 issues)
indices = which(scored$sentiment_openai_en %in% c("POSITIVE","NEGATIVE","NEUTRAL"))
scored = scored[indices,]

#Save data
tweets = readRDS("./data/preprocessed_tweets.rds")
tweets = merge(tweets,scored, by.x = "uni_text", by.y = "sw_text")

saveRDS(tweets, "./data/processed_tweets.rds")


