cat("Get the Swedish parliament data to run this script, relevant scripts are: \n",
    "    - 'get_sweparlimentscore/GetSessions.ipynb'  \n",
    "    - 'get_sweparlimentscore/GetDWscore.R'\n")

rm(list=ls())
gc()

if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}
library(dplyr)
getwd()

extract_surname <- function(name) {
  sapply(strsplit(name, " "), function(x) tail(x, 1))
}



# DW_score ----------------------------------------------------------------



scores = readRDS("./get_sweparlimentscore/dw_score.rds")$legislators
tweets = readRDS("./data/processed_tweets.rds")



scores$last_name = extract_surname(scores$name)
unique_politicians = data.frame(full_name = unique(tweets$politician_author),
                                last_name = extract_surname(unique(tweets$politician_author)))

#table(unique_politicians$last_name) # Andersson and Forssell have 2 matches. take first name
#Using unqiue amtches is fine here:
#there are plenty Anderssons (& Forssel) in pariliment but only 2 in the tweet data set.
#For those cases I match the surnames.
#Swedish parliment unfortunately does not provide an ID nor is consistent with the naming.

patterns <- c("Andersson", "Forssel")
for (pattern in patterns) {
  dupmatches = grep(pattern, unique_politicians$last_name)
  unique_politicians[dupmatches, "last_name"] = unique_politicians[dupmatches, "full_name"]
  
  dupmatches = grep(pattern, scores$last_name)
  scores[dupmatches, "last_name"] = scores[dupmatches, "name"]
  }


### only select the latest session for every politician
scores <- scores %>%
  dplyr::group_by(last_name) %>%
  dplyr::slice(which.max(session))

unique_politicians = inner_join(unique_politicians, scores[,c('last_name','coord1D','coord2D')]) %>% select(-last_name)


tweets = left_join(tweets, unique_politicians, by = c("politician_author" = "full_name"))


# Rename the new columns added from unique_politicians for clarity (if needed)
# Use suffixes such as "_author" if more descriptive names are required
overlap_columns <- intersect(names(unique_politicians), names(tweets))
names(tweets)[names(tweets) %in% overlap_columns] <- 
  paste0(overlap_columns, "_author")


names(tweets)

# Merge for politician_target
tweets <- left_join(tweets, unique_politicians, 
                    by = c("politician_target" = "full_name"),
                    suffix = c("", "_target"))

# Rename only the columns from unique_politicians that were added during this merge
overlap_columns <- intersect(names(unique_politicians), names(tweets))
names(tweets)[names(tweets) %in% overlap_columns] <- 
  paste0(overlap_columns, "_target")


# GPT scores ---------------------------------------------------------------



###add GPT scores - politicans
GPT_pol = readRDS("./get_sweparlimentscore/GPT_polscore.rds")
tweets = left_join(tweets, GPT_pol, by = c("politician_author" = "politician")) %>%
  rename_with(~ paste0(., "_author"), .cols = names(GPT_pol)[-1])
tweets = left_join(tweets, GPT_pol, by = c("politician_target" = "politician")) %>%
  rename_with(~ paste0(., "_target"), .cols = names(GPT_pol)[-1])

###add GPT scores - parites
GPT_party = readRDS("./get_sweparlimentscore/GPT_partyscore.rds")
tweets = left_join(tweets, GPT_party, by = c("party_author" = "party")) %>%
  rename_with(~ paste0(., "_author"), .cols = names(GPT_party)[-1])
tweets = left_join(tweets, GPT_party, by = c("party_target" = "party")) %>%
  rename_with(~ paste0(., "_target"), .cols = names(GPT_party)[-1])


# CHES scores -------------------------------------------------------------
CHES = readRDS("./get_sweparlimentscore/CHES_score.rds")
CHES$party = as.character(CHES$party)
CHES$party = replace(CHES$party, CHES$party == "S/SAP", "S") 
CHES = rename(CHES, CHES = lrgen)

tweets = left_join(tweets, CHES, by = c("party_author" = "party")) %>%
  rename_with(~ paste0(., "_author"), .cols = names(CHES)[-1])
tweets = left_join(tweets, CHES, by = c("party_target" = "party")) %>%
  rename_with(~ paste0(., "_target"), .cols = names(CHES)[-1])


# save to RDS -------------------------------------------------------------


saveRDS(tweets,"./data/tweets_merge3dwscore.rds")
