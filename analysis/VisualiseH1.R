rm(list=ls())
gc()
if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}
library(tidyverse)
library(ggthemes)

df = readRDS("./data/tweets_merge3dwscore.rds")



# group by sentiment and target -------------------------------------------



inbloc_sentimenten = df %>% group_by(target_inbloc, sentiment = sentiment_en) %>% summarise(sentiment_en = length(uni_text))
inbloc_sentimentsw = df %>% group_by(target_inbloc, sentiment = sentiment_sw) %>% summarise(sentiment_sw = length(uni_text))
inbloc_sentimenten_gpt = df %>% group_by(target_inbloc, sentiment = sentiment_openai_en) %>% summarise(sentiment_gpten = length(uni_text))
inbloc_sentimentsw_gpt = df %>% group_by(target_inbloc, sentiment =  sentiment_openai_sw) %>% summarise(sentiment_gptsw = length(uni_text))

inparty_sentimenten = df %>% group_by(target_inparty, sentiment = sentiment_en) %>% summarise(sentiment_en = length(uni_text))
inparty_sentimentsw = df %>% group_by(target_inparty, sentiment = sentiment_sw) %>% summarise(sentiment_sw = length(uni_text))
inparty_sentimenten_gpt = df %>% group_by(target_inparty, sentiment = sentiment_openai_en) %>% summarise(sentiment_gpten = length(uni_text))
inparty_sentimentsw_gpt = df %>% group_by(target_inparty, sentiment =  sentiment_openai_sw) %>% summarise(sentiment_gptsw = length(uni_text))


# merge all group bys -----------------------------------------------------





mergeddf_bloc = inbloc_sentimenten %>%
  left_join(inbloc_sentimentsw, by = c("target_inbloc", "sentiment")) %>%
  left_join(inbloc_sentimenten_gpt, by = c("target_inbloc", "sentiment")) %>%
  left_join(inbloc_sentimentsw_gpt, by = c("target_inbloc", "sentiment")) %>%
  filter(sentiment != "NEUTRAL") %>%
  pivot_longer(cols = c(sentiment_en, sentiment_sw, sentiment_gpten, sentiment_gptsw), 
               names_to = "sentiment_type", 
               values_to = "sentiment_value")

mergeddf_party = inparty_sentimenten %>%
  left_join(inparty_sentimentsw, by = c("target_inparty", "sentiment")) %>%
  left_join(inparty_sentimenten_gpt, by = c("target_inparty", "sentiment")) %>%
  left_join(inparty_sentimentsw_gpt, by = c("target_inparty", "sentiment")) %>%
  filter(sentiment != "NEUTRAL") %>%
  pivot_longer(cols = c(sentiment_en, sentiment_sw, sentiment_gpten, sentiment_gptsw), 
               names_to = "sentiment_type", 
               values_to = "sentiment_value")






# plots -------------------------------------------------------------------



ggplot(mergeddf_bloc, aes(x = sentiment, y = sentiment_value, fill = sentiment_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = .9)) +
  facet_wrap(~ target_inbloc, 
             labeller = labeller(target_inbloc = c("TRUE" = "In-Bloc", "FALSE" = "Out-Bloc"))) +  # Change facet labels
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu",
                    labels = c("Open Source English", "GPT English", "GPT Swedish", "OpenSource Swedish")) +
  labs(
    x = "Sentiment",
    y = "Count",
    fill = "") +
  theme(
    legend.title = element_blank(),  # Remove the legend title
    legend.position = "bottom",  # Move legend to the bottom
    legend.box.margin = margin(10, 0, 0, 0),  # Add margin around the legend box (optional)
    legend.text = element_text(color = "black"),  # Black text for the legend
    strip.text = element_text(size = 15),  # Change the size of facet labels (In-Party/Out-Party)
    axis.text = element_text(size = 12),  # Change axis label size
    axis.title = element_text(size = 14),  # Change axis title size (optional)
    axis.title.x = element_blank()
  )
ggsave("./plots/H1PlotBloc.pdf", width = 6, height = 4, units = "in")




ggplot(mergeddf_party, aes(x = sentiment, y = sentiment_value, fill = sentiment_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = .9)) +
  facet_wrap(~ target_inparty, 
             labeller = labeller(target_inparty = c("TRUE" = "In-Party", "FALSE" = "Out-Party"))) +  # Change facet labels
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu",
                    labels = c("Open Source English", "GPT English", "GPT Swedish", "OpenSource Swedish")) +
  labs(
    y = "Count") +
  theme(
    legend.title = element_blank(),  # Remove the legend title
    legend.position = "bottom",  # Move legend to the bottom
    legend.box.margin = margin(10, 0, 0, 0),  # Add margin around the legend box (optional)
    legend.text = element_text(color = "black"),  # Black text for the legend
    strip.text = element_text(size = 15),  # Change the size of facet labels (In-Party/Out-Party)
    axis.text = element_text(size = 12),  # Change axis label size
    axis.title = element_text(size = 14),
    axis.title.x = element_blank()
  )

ggsave("./plots/H1PlotParty.pdf", width = 6, height = 4, units = "in")
