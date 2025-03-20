rm(list=ls())
gc()

if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}



library(readxl)
library(caret)
library(dplyr)
#library(knitr)
library(irr)

# Load datasets
df <- readRDS("./data/tweets_merge3dwscore.rds")
ratings <- readxl::read_xlsx("./TweetSentiment/AnnotatedTweets.xlsx")
ratings <- ratings[!(ratings$rating == "0.0"),]

df <- df %>% select(sentiment_en, sentiment_sw, sentiment_openai_en, sentiment_openai_sw, text, id)
df <- merge(df, ratings, by = c("text", "id"), all.x = FALSE, all.y = FALSE)
df <- unique(df)

# Define the methods to compare
methods <- c("sentiment_en", "sentiment_sw", "sentiment_openai_en", "sentiment_openai_sw")

# Function to compute ordinal-aware Weighted F1 Score
weighted_f1 <- function(conf_matrix, weight_matrix) {
  precision <- diag(conf_matrix) / colSums(conf_matrix)
  recall <- diag(conf_matrix) / rowSums(conf_matrix)
  f1_per_class <- 2 * (precision * recall) / (precision + recall)
  
  # Apply weight matrix
  weighted_f1 <- sum(f1_per_class * diag(weight_matrix), na.rm = TRUE) / sum(diag(weight_matrix))
  return(weighted_f1)
}

# Function to compute Quadratic Weighted F1 Score
quadratic_weighted_f1 <- function(conf_matrix) {
  n <- nrow(conf_matrix)
  
  # Quadratic weight matrix
  weight_matrix <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      weight_matrix[i, j] <- 1 - ((i - j)^2 / (n - 1)^2)
    }
  }
  
  return(weighted_f1(conf_matrix, weight_matrix))
}

# Initialize results dataframe
results <- data.frame(Method = character(), Weighted_F1 = numeric(), Quadratic_Weighted_F1 = numeric(), Balanced_Accuracy = numeric(), stringsAsFactors = FALSE)

# Compute metrics for each method
for (method in methods) {
  cm <- confusionMatrix(as.factor(df[[method]]), as.factor(df$rating))
  
  # Weighted F1 Score
  n_classes <- length(unique(df$rating))
  weight_matrix <- matrix(0, n_classes, n_classes)
  for (i in 1:n_classes) {
    for (j in 1:n_classes) {
      weight_matrix[i, j] <- 1 - abs(i - j) / (n_classes - 1)  # Linearly reducing weight for further errors
    }
  }
  
  weighted_f1_score <- weighted_f1(cm$table, weight_matrix)
  qwf1_score <- quadratic_weighted_f1(cm$table)
  
  # Balanced Accuracy
  recall <- diag(cm$table) / rowSums(cm$table)
  balanced_accuracy <- mean(recall, na.rm = TRUE)
  
  # Store results
  results <- rbind(results, data.frame(Method = method, Weighted_F1 = weighted_f1_score, Quadratic_Weighted_F1 = qwf1_score, Balanced_Accuracy = balanced_accuracy))
}

# Print the results table
print(results)

# Format results for LaTeX (APA style)
results %>%
  mutate(
    Weighted_F1 = round(Weighted_F1, 3),
    Quadratic_Weighted_F1 = round(Quadratic_Weighted_F1, 3),
    Balanced_Accuracy = round(Balanced_Accuracy, 3)
  ) %>%
  kable(
    format = "latex",
    caption = "Model Performance Metrics: Ordinal-Aware F1 Scores and Balanced Accuracy",
    col.names = c("Prediction Method", "Weighted F1", "Quadratic Weighted F1", "Balanced Accuracy"),
    booktabs = TRUE,
    align = "lccc"
  )
