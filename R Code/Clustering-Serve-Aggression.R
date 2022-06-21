# PURPOSE: Clustering For EDA Project Looking at Aces and Double Faults


# Load in Data

library(tidyverse)
library(protoclust)
library(ggdendro)

wta <- read_csv("Data/wta.csv")
duplicate_match <- read_csv("Data/duplicate_match_wta.csv")
indiv_player_match <- read_csv("Data/indiv_player_match_wta.csv")


# Create player serve stats dataset


player_match_count <- (indiv_player_match%>%
                         count(player_name))[[2]]

player_serve_stats <- indiv_player_match%>%
  mutate(win = ifelse(result == "W", 1, 0))%>%
  group_by(player_name)%>%
  summarize_at(vars(win, sets, ace, df, svpt, first_in, first_won, second_in, second_won), 
               sum, 
               na.rm = TRUE)%>%
  ungroup()%>%
  mutate(matches_played = player_match_count,
         win_pct = win/matches_played,
         first_won_pct = first_won / first_in,
         second_in_won_pct = second_won / second_in,
         second_won_pct = second_won / (second_in + df),
         aces_set = ace / sets, 
         df_set = df / sets,
         aces_match = ace/ matches_played,
         df_match = df / matches_played)


# write_csv(player_serve_stats, "Data/player_serve_stats.csv")


player_serve_stats %>%
  filter(matches_played >= 50)%>%
  ggplot(aes(x = aces_match, y = df_match))+
  geom_point()+
  coord_fixed()


player_serve_stats%>%
  ggplot(aes(x = matches_played))+
  stat_ecdf()+
  geom_vline(xintercept = 50)


# Clustering using hclust -------------------------------------------------

player_serve_stats_filtered <- player_serve_stats %>%
  filter(matches_played >= 50)%>%
  mutate(std_aces_match = as.numeric(scale(aces_match)),
         # Standardizing aces and dfs
         std_df_match = as.numeric(scale(df_match)))  

# Examining scatterplot b/t standardized variables
player_serve_stats_filtered%>%
  ggplot(aes(x = std_aces_match, y = std_df_match))+
  geom_point(alpha = 0.5)


# Dist matrix
wta_player_dist <- dist(dplyr::select(player_serve_stats_filtered, std_aces_match, std_df_match))



# Clustering with complete linkage ----------------------------------------

wta_complete_hclust <- 
  hclust(wta_player_dist, method = "complete")

# Creating new dataset with cluster assignments
clusters_stats <- player_serve_stats_filtered %>%
  mutate(player_clusters = 
           as.factor(cutree(wta_complete_hclust, k = 4)))


# Key players to label in graph
key_players <- clusters_stats%>%
  filter(player_name %in% c("Serena Williams", 
                            "Maria Sharapova", 
                            "Naomi Osaka",
                            "Venus Williams"))

# Dendograpm
ggdendrogram(wta_complete_hclust, labels = FALSE, leaf_labels = FALSE,
             theme_dendro = FALSE)+
  labs(y = "Dissimilarity between clusters")+
  theme_bw()+
  geom_hline(yintercept = 5)+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

# Graph of complete linkgae clusters with k = 4
clusters_stats%>%
  select(player_name, contains("match"), player_clusters)%>%
  ggplot(aes(x = aces_match, y = df_match, color = player_clusters))+
  geom_point(alpha = 0.5, size = 3)+
  geom_label(data = key_players, aes(label = player_name), size = 3)+
  ggthemes::scale_color_colorblind()+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))+
  labs(x = "Aces per Match",
       y = "Double Faults per Match",
       color = "Player Cluster",
       title = "Complete Linkage Clustering Players Based on Aces and Double Faults",
       #subtitle = "There are few elite players who have high aces and low double faults",
       )


# Minimax Clustering ------------------------------------------------------


wta_minimax <-
  protoclust(wta_player_dist)

# For some reason dendogram with minimax won't work

# ggdendrogram(wta_minimax, labels = FALSE, leaf_labels = FALSE,
#              theme_dendro = FALSE)+
#   labs(y = "Dissimilarity between clusters")+
#   theme_bw()+
#   theme(axis.text.x = element_blank(),
#         axis.title.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         panel.grid = element_blank())

# 4 clusters

minimax_wta_player_clusters <- protocut(wta_minimax, k = 4)


# Add in clusters to dataset

clusters_stats_minimax <- player_serve_stats_filtered %>%
  mutate(player_clusters = 
           as.factor(minimax_wta_player_clusters$cl))


# Protos only dataset
wta_protos <- clusters_stats_minimax%>%
  slice(minimax_wta_player_clusters$protos)

# Clusters plotted with protos
clusters_stats_minimax%>%
  select(player_name, contains("match"), player_clusters)%>%
  ggplot(aes(x = aces_match, y = df_match, color = player_clusters))+
  geom_point(alpha = 0.5, size = 3)+
  geom_label(data = wta_protos, aes(label = player_name), size = 3)+
  ggthemes::scale_color_colorblind()+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(x = "Aces per Match",
       y = "Double Faults per Match",
       color = "Player Cluster",
       title = "Minimax Clustering Players Based on Aces and Double Faults",
       subtitle = "There are few elite players who have high aces and low double faults",
       caption = "Labeled players are cluster prototypes")


