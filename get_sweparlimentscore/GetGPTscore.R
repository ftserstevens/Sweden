rm(list=ls())
gc()
if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}


library(openai)
library(dplyr)

OPENAI_API_KEY= "YOURPAIKEY"
df = readRDS("./data/tweets_merge3dwscore.rds")
scores_pol <- data.frame(politician = unique(df$politician_author), score_GPTpol = NA)
scores_party <- data.frame(party = unique(df$party_author), score_GPTparty = NA)


score_politician <- function(x) {
  completion = create_chat_completion(
    temperature = 0,
    openai_api_key = OPENAI_API_KEY,
    model = "gpt-4o",  # Use a model available in the OpenAI package
    messages = list(
      list(
        role = "system",
        content = "As an AI model with extensive knowledge in Swedish politics, rate this politician from -1 (extreme left) to 1 (extreme right) with 2 decimal numbers. Answer only with a number in the aforementionned range."
      ),
      list(
        role = "user",
        content = x
      )
    )
  )
  return(completion$choices$message.content)
}

score_party <- function(x) {
  completion = create_chat_completion(
    temperature = 0,
    openai_api_key = OPENAI_API_KEY,
    model = "gpt-4o",  # Use a model available in the OpenAI package
    messages = list(
      list(
        role = "system",
        content = "As an AI model with extensive knowledge in Swedish politics, rate this political party from -1 (extreme left) to 1 (extreme right) with 2 decimal numbers. Answer only with a number in the aforementionned range."
      ),
      list(
        role = "user",
        content = x
      )
    )
  )
  return(completion$choices$message.content)
}


for (i in 1:nrow(scores_pol)) {
  scores_pol$score_GPTpol[i] = score_politician(scores_pol$politician[i])  
}

for (i in 1:nrow(scores_party)) {
  scores_party$score_GPTparty[i] = score_party(scores_party$party[i])  
}

saveRDS(scores_pol,"./get_sweparlimentscore/GPT_polscore.rds")
saveRDS(scores_party,"./get_sweparlimentscore/GPT_partyscore.rds")
