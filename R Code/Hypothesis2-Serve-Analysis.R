# PURPOSE: Explore Hypothesis 2 of EDA Project


## Hypothesis 2 / Research Question 2

#  How does a player's point winning percentage change when they commit a 
#  fault on their serve? How does the probability change given they got their second serve in?

# Will definitely decrease overall when fault on first serve
# Even given their second serve is in, percentage will still decrease due to safer serve

# Still deciding what final visual will be used



# Load in Data & Libraries ------------------------------------------------

library(tidyverse)
wta <- read_csv("Data/wta.csv")
duplicate_match <- read_csv("Data/duplicate_match_wta.csv")
indiv_player_match <- read_csv("Data/indiv_player_match_wta.csv")

# Creating Dataset Aggregating Serve Statistics ---------------------------



player_match_count <- (indiv_player_match%>%
                         count(player_name))[[2]]

player_serve_stats <- indiv_player_match%>%
  group_by(player_name)%>%
  summarize_at(vars(sets, ace, df, svpt, first_in, first_won, second_in, second_won), sum, na.rm = TRUE)%>%
  ungroup()%>%
  mutate(matches_played = player_match_count,
         first_won_pct = first_won / first_in,
         second_in_won_pct = second_won / second_in,
         second_won_pct = second_won / (second_in + df))


#% of pts won when first serve in
sum(player_serve_stats$first_won) / sum(player_serve_stats$first_in)

# % of second serves won given second serve in
sum(player_serve_stats$second_won) / (sum(player_serve_stats$second_in))

# % of second serves won overall (includes double faults)
sum(player_serve_stats$second_won) / (sum(player_serve_stats$second_in) + sum(player_serve_stats$df))



# Which players have biggest gap between win pct on first/second serves --------

player_serve_stats%>%
  filter(matches_played >= 30)%>%
  mutate(serve_diff_all_pct = first_won_pct - second_won_pct,
         serve_diff_in_pct = first_won_pct - second_in_won_pct,
         player_name = fct_reorder(player_name, serve_diff_all_pct))%>%
  slice_max(serve_diff_all_pct, n = 10)%>%
  ggplot(aes(x = player_name, y = serve_diff_all_pct))+
  geom_bar(stat = "identity")


# Which players double fault the most?

player_serve_stats%>%
  filter(matches_played >= 30)%>%
  mutate(avg_df = df / matches_played,
         avg_aces = ace / matches_played)%>%
  slice_max(avg_df, n = 10)%>%
  ggplot(aes(x = player_name, y = avg_df))+
  geom_bar(stat = "identity")
  

# Are players who double fault better at getting aces?
# That is, are they more aggressive, leading to more of both outcomes?

player_serve_stats%>%
  filter(matches_played >= 30)%>%
  mutate(avg_df = df / matches_played,
         avg_aces = ace / matches_played)%>%
  ggplot(aes(x = avg_aces, y = avg_df))+
  geom_point(alpha = 0.6)+
  geom_smooth(method = "lm", se = F)



# Serve Trends by Surface -------------------------------------------------



# Aggregate serving stats by surface type

surface_match_count <- (indiv_player_match%>%
                          count(surface))[[2]]

surface_serve_stats<- indiv_player_match%>%
  group_by(surface)%>%
  summarize_at(vars(ace, df, svpt, first_in, first_won, second_in, second_won), sum, na.rm = TRUE)%>%
  ungroup()%>%
  mutate(matches_played = surface_match_count,
         first_won_pct = first_won / first_in,
         second_in_won_pct = second_won / second_in,
         second_won_pct = second_won / (second_in + df))

# Do first/second serve stats change based on surface
surface_serve_stats%>%
  pivot_longer(c(10, 12))%>%
  ggplot(aes(x = surface, y = value, fill = name))+
  geom_bar(position = "dodge", stat = "identity")
  

# CDF of first and second serves won
surface_serve_stats%>%
  pivot_longer(c(10, 12))%>%
  ggplot(aes(x = value, color = name))+
  stat_ecdf()

