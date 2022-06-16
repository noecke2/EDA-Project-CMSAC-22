# PURPOSE: Clustering For EDA Project Looking at Aces and Double Faults


# Load in Data

library(tidyverse)
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

player_serve_stats_filtered <- player_serve_stats %>%filter(matches_played >= 50)
wta_player_dist <- dist(dplyr::select(player_serve_stats_filtered, aces_match, df_match))


wta_complete_hclust <- 
  hclust(wta_player_dist, method = "complete")

player_serve_stats_filtered %>%
  mutate(player_clusters = 
           as.factor(cutree(wta_complete_hclust, k = 5)))%>%
  # select(player_name, aces_match, df_match, player_clusters)
  ggplot(aes(x = aces_match, y = df_match, color = player_clusters))+
  geom_point(alpha = 0.5, size = 3)+
  # ggthemes::scale_color_colorblind()+
  theme_bw()+
  theme(legend.position = "bottom")

