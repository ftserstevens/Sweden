library(readxl)
library(caret)
library(dplyr)
library(knitr)
library(irr)

df = readRDS("./data/tweets_merge3dwscore.rds")

ratings= readxl::read_xlsx("./TweetSentiment/AnnotatedTweets.xlsx")
ratings = ratings[!(ratings$rating == "0.0"),]


df = df %>% select(sentiment_en, sentiment_sw, sentiment_openai_en, sentiment_openai_sw, text, id)
df = merge(all.x = F, all.y = F, df, ratings, by = c("text",'id'))
df = unique(df)
# Define the methods to compare
methods <- c("sentiment_en", "sentiment_sw", "sentiment_openai_en", "sentiment_openai_sw")

# Initialize a data frame to store results
results <- data.frame(Method = character(), F1 = numeric(), Balanced_Accuracy = numeric(), stringsAsFactors = FALSE)

# Loop through each method and calculate metrics
for (method in methods) {
  cm <- confusionMatrix(as.factor(df[[method]]), as.factor(df$rating))
  
  # Precision, recall, and F1 per class
  precision <- diag(cm$table) / colSums(cm$table)
  recall <- diag(cm$table) / rowSums(cm$table)
  f1_per_class <- 2 * (precision * recall) / (precision + recall)
  
  # Weighted F1 score
  support <- rowSums(cm$table)
  weighted_f1 <- sum(f1_per_class * support / sum(support), na.rm = TRUE)
  
  # Balanced Accuracy
  balanced_accuracy <- mean(recall, na.rm = TRUE)
  
  # Append results
  results <- rbind(results, data.frame(Method = method, F1 = weighted_f1, Balanced_Accuracy = balanced_accuracy))
}

# Print the results table
print(results)


# Create the table in APA format
results %>%
  mutate(
    F1 = round(F1, 3),
    Balanced_Accuracy = round(Balanced_Accuracy, 3)
  ) %>%
  kable(
    format = "latex",
    caption = "Model Performance Metrics: F1 Score and Balanced Accuracy",
    col.names = c("Prediction Method", "F1 Score", "Balanced Accuracy"),
    booktabs = TRUE,
    align = "lcc"
  )
