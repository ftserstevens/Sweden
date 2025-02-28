rm(list=ls())
gc()

if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}

library(ggplot2)
library(ggridges)
library(rstan)
library(stringr)
source(file = "./analysis/MakeLatexTable.R")


# Define possible values for group and reaction
groups <- c("party", "bloc")
reactions <- c("likes", "retweets")

# Initialize an empty list to store data frames
all_data <- list()

# Loop through each combination of group and reaction
for (g in groups) {
  for (r in reactions) {
    # Load the corresponding model
    stan_fit = readRDS(paste0("./TrainedModels/fitH2_", r, "_", g, ".rds"))
    betas = extract(stan_fit, pars = "beta")$beta
    
    # Compute NP and PP vectors
    if (g =="party") {
    NP_vec = betas[,1] + betas[,2] + betas[,3] + betas[,12]
    PP_vec = betas[,1] + betas[,4] + betas[,13]
    }
    if (g =="bloc"){
      NP_vec = betas[,1] + betas[,2] + betas[,3] + betas[,6]
      PP_vec = betas[,1] + betas[,4] + betas[,7]
    }
    
    df_temp <- data.frame(
      value = c(NP_vec, PP_vec),
      group = rep(c("Negative Partisanship", "Positive Partisanship"), each = length(NP_vec)),
      reaction = paste0( "Estimated Log of ",str_to_title(r)),
      category = str_to_title(g)
    )
    
    # Append to the list
    all_data[[paste(r, g, sep = "_")]] <- df_temp
  }
}
df_all <- do.call(rbind, all_data)

# Plot the ridge plot with faceting

ggplot(df_all, aes(x = value, y = category, fill = group)) +
  geom_density_ridges(alpha = 0.5, scale = 1, rel_min_height = 0, position = "identity") +
  facet_grid(category ~ reaction, scales = "free_y") +  # Facet by category (Bloc/Party) and reaction
  theme_minimal() +
  geom_text(data = df_all %>%
              group_by(category, reaction, group) %>%
              summarise(median_value = median(value), .groups = 'drop'),
            aes(x = median_value, y = category, label = round(median_value, 2)),
            color = "black", size = 3, vjust = +1.8) +
  labs(x = "", y = "") +
  theme(
    strip.text = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),  # Bigger, bold, and black font for "Party" and "Bloc"
    legend.position = "top",  # Center legend below plot
    legend.justification = "center",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, color = "black"),
    panel.spacing.x = unit(2, "lines"),
    strip.text.y.right = element_blank()
  )
ggsave("./plots/H2plot.pdf", width = 8, height = 6, units = "in")

