rm(list=ls())
gc()

library(dplyr)
library(igraph)
library(tidyr)

if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}
set.seed(seed = 0)

tweets = readRDS("./data/tweets_merge3dwscore.rds")
tweets$p = tweets$sentiment_openai_en =="POSITIVE"
tweets$n = tweets$sentiment_openai_en =="NEGATIVE"
tweets$neu = tweets$sentiment_openai_en =="NEUTRAL"
tweets$sentiment = tweets$sentiment_openai_sw



politicians =  readRDS("./data/politicians_info.rds")
politicians = unique(politicians[complete.cases(politicians),-2])



# compute layout ----------------------------------------------------------


# Create edges and attributes
edge <- as.data.frame(tweets[, c("politician_author", "politician_target", "sentiment")])  # Include 'sentiment' column
attributes <- unique(as.data.frame(politicians[, c("name", "party_affiliation", "bloc_affiliation")]))

# Create the graph
graph <- graph_from_data_frame(edge, directed = TRUE, vertices = attributes)

# Assign weights based on sentiment
E(graph)$weight <- ifelse(edge$sentiment == "POSITIVE", .5, 
                          ifelse(edge$sentiment == "NEGATIVE", 2, 1))  # Neutral is 1


# Compute layout using weights
layout_weighted <- layout_with_fr(graph, weights = E(graph)$weight)

# visualization -----------------------------------------------------------




in_degree <- degree(graph, mode = "in")
out_degree <- degree(graph, mode = "out")
threshold <- quantile(in_degree, .90)
top_nodes <- V(graph)[in_degree > threshold]
vertex_labels <- ifelse(V(graph) %in% top_nodes, V(graph)$name, NA)

groups <- unique(attributes$party_affiliation)
color_palette <- c("#006a39","#b00100","#ed1b34","#231977","#52a044","#6bb7ec", "#ffdf07","#1a4adf")
group_colors <- setNames(color_palette, groups)
vertex_colors <- sapply(V(graph)$party_affiliation, function(g) group_colors[g])



layout_weighted = scale(layout_weighted, center = T, scale = T)
layout_weighted[,1] = ifelse(layout_weighted[,1]>= 1.8,1.8,layout_weighted[,1])
layout_weighted[,1] = ifelse(layout_weighted[,1]<= -1.8, - 1.8,layout_weighted[,1])
layout_weighted[,2] = ifelse(layout_weighted[,2]>= 1.8 ,1.8,layout_weighted[,2])
layout_weighted[,2] = ifelse(layout_weighted[,2]<= -1.8, - 1.8,layout_weighted[,2])




# Plot the graph
pdf("./NetworkPlots/network.pdf", width = 10, height = 10)
plot(igraph::simplify(graph, remove.multiple = TRUE, remove.loops = TRUE), 
     layout = layout_weighted,
     vertex.size = log(in_degree) *1.2,   # Node size based on out-degree
     vertex.color = vertex_colors,
     vertex.label = vertex_labels,   # Labels only for top nodes
     vertex.label.color = "black",   # Label color
     vertex.label.cex = 0.000001,         # Label size
     edge.arrow.size = 0.0000,
     edge.color = "#f0f0f0",
     edge.width = 0.075,
     asp = 0, margin = -0,
     frame = FALSE  )
legend("bottomleft", legend = unique(attributes$party_affiliation), fill = group_colors, title = "Party Affiliation",
       cex = 1.35,   # Adjust text size
       bty = "n") 
dev.off()