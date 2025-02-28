rm(list=ls())
gc()

library(dplyr)
library(tidyr)
library(progress)

if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}

tweets =readRDS("./data/tweets.rds")
politicians =  readRDS("./data/politicians_info.rds")
politicians = politicians[complete.cases(politicians), ]

# Select tweets that mention political persona ---------------------------


# Initialize the list columns in the `tweets` data frame
tweets$politician_target <- vector("list", length = nrow(tweets))



# Iterate through each alias in keywords

aliases <- tolower(politicians$alias)
tweet_texts <- tolower(tweets$text)

# Initialize a progress bar
pb <- progress_bar$new(
  total = length(aliases),
  format = "  Matching [:bar] :current/:total (:percent) - ETA: :eta",
  clear = FALSE, width = 60
)

# Iterate through the aliases and match in the tweets
for (i in seq_along(aliases)) {
  # Print progress
  pb$tick()
  
  # Find the matched indices
  alias_match <- grep(pattern = aliases[i], x = tweet_texts)
  
  if (length(alias_match) > 0) {
    # Append unique politician name to the matched indices
    tweets$politician_target[alias_match] <- lapply(tweets$politician_target[alias_match], 
                                                    function(x) unique(c(x, politicians$name[i])))
  }
}

# add the target info -----------------------------------------------------

tweets = tweets[which(sapply(tweets$politician_target, function(x) length(x) > 0)),]
tweets = tidyr::unnest(tweets, cols = politician_target)

politician_target = unique(subset(politicians, select = -c(alias,author_id)))
names(politician_target) = c("politician_target","ingov_target","party_target",'bloc_target')

tweets = merge(tweets, politician_target, by = "politician_target")


# info about the author ---------------------------------------------------

politician_author = subset(politicians, select = -c(alias))
names(politician_author)[c(1,3:5)] = c("politician_author","ingov_author","party_author",'bloc_author')
politician_author = unique(politician_author)
tweets  = merge(tweets,politician_author, 
      all.x = F, all.y = F,
      by = "author_id")

tweets$hour_posted = as.integer(substr(tweets$created_at, 12, 13))
tweets = mutate(tweets, 
       timeofday = cut(hour_posted, 
                        breaks = c(-1, 3, 7, 11, 15, 19, 23), 
                        labels = c("Block 1: 00:00 - 3:59", 
                                   "Block 2: 4:00 - 7:59", 
                                   "Block 3: 8:00 - 11:59", 
                                   "Block 4: 12:00 - 15:59", 
                                   "Block 5: 16:00 - 19:59", 
                                   "Block 6: 20:00 - 23:59")))




# politicians -  in government at the correct time -------------------------------------------


tweets$ingov_author = 
  ifelse(tweets$ingov_author == "before" & tweets$created_at < as.Date("2022-09-11",'%Y-%m-%d') |
           tweets$ingov_author == "after" & tweets$created_at >= as.Date("2022-09-11",'%Y-%m-%d'), 
         T,F)
  
tweets$ingov_target = 
  ifelse(tweets$ingov_target == "before" & tweets$created_at < as.Date("2022-09-11",'%Y-%m-%d') |
           tweets$ingov_target == "after" & tweets$created_at >= as.Date("2022-09-11",'%Y-%m-%d'), 
         T,F)



# identify in/out ---------------------------------------------------------

tweets$target_inbloc = ifelse(tweets$bloc_target == tweets$bloc_author, T, F)
tweets$target_inparty = ifelse(tweets$party_target == tweets$party_author, T, F)

# identify RT and clean text ---------------------------------------------------------


tweets$retweet = substr(tweets$text,1,3) == "RT "
tweets$uni_text = sub(pattern = "\n","",tweets$text)
tweets$uni_text = ifelse(tweets$retweet == T, 
                         substr(tweets$uni_text,4,nchar(tweets$uni_text)), 
                         tweets$uni_text)


# make files --------------------------------------------------------------
saveRDS(tweets, "./data/preprocessed_tweets.rds")

translate = unique(as.data.frame(cbind(sw_text = tweets$uni_text)))
write.csv(translate, './TweetSentiment/translate.csv', row.names = FALSE)

