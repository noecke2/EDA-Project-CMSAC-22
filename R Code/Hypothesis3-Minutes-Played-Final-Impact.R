# Purpose: Answer Question 3 for EDA Project

# RQ3: Do players who have played less minutes leading up to a tournament final than their opponent see improved success in the final?


library(tidyverse)
wta <- read_csv("Data/wta.csv")
duplicate_match <- read_csv("Data/duplicate_match_wta.csv")
indiv_player_match <- read_csv("Data/indiv_player_match_wta.csv")


final_players <- indiv_player_match%>%
  filter(round == "F")

final_stats <- indiv_player_match%>%
  group_by(player_name, tourney_id)%>%
  summarize(tot_minutes = sum(minutes, na.rm = TRUE))%>%
  inner_join(final_players, by = c("player_name", "tourney_id"))%>%
  arrange(tourney_id, desc(result))

winner_tot_minutes <- final_stats$tot_minutes[seq(from = 1, to = 442, by = 2)]
loser_tot_minutes <- final_stats$tot_minutes[seq(from = 2, to = 442, by = 2)]


# Trying to fix it
final_players_wta <- wta%>%
  filter(round == "F")

final_minutes <- wta%>%
  group_by(winner_name, tourney_id)%>%
  summarize(tot_minutes = sum(minutes, na.rm = TRUE))%>%
  filter(winner_name %in% final_players_wta$winner_name,
         winner_name %in% final_players_wta$loser_name,
         tourney_id %in% final_players_wta$tourney_id)%>%
  right_join(final_players_wta)%>%
  arrange(tourney_id)%>%
  ungroup()%>%
  mutate(winner_tot_minutes = winner_tot_minutes,
         loser_tot_minutes = loser_tot_minutes,
         abs_minute_diff = abs(winner_tot_minutes - loser_tot_minutes))%>%
  select(winner_name, loser_name, winner_tot_minutes, loser_tot_minutes, tourney_id, surface, abs_minute_diff)




final_minutes%>%
  mutate(minute_diff = winner_tot_minutes - loser_tot_minutes)%>%
  filter(abs(minute_diff) < 1000)%>%
  ggplot(aes(x = minute_diff))+
  geom_histogram()+
  facet_wrap(~surface)

# Check that math / join were done right
indiv_player_match%>%
  filter(player_name == "Belinda Bencic", tourney_id == "2018-0300")%>%
  select(1:4, minutes)


# Recategorize the minute difference for plot

final_minutes <- final_minutes%>%
  mutate(winner_adv = ifelse(winner_tot_minutes < loser_tot_minutes, 1, 0),
         adv_category = ifelse(abs_minute_diff < 50, "0-49", "150+"),
         adv_category = ifelse(abs_minute_diff < 100 & abs_minute_diff >= 50,
                               "50-99", adv_category),
         adv_category = ifelse(abs_minute_diff < 150 & abs_minute_diff >= 100,
                               "100-149", adv_category),
         adv_category = fct_relevel(adv_category,
                                    "0-49",
                                    "50-99",
                                    "100-149",
                                    "150+"))


write_csv(final_minutes, "Data/final_minutes.csv")

# Official Visualization for Hypothesis 3 ---------------------------------

# Still need a title / takeaway - wording is tough

final_minutes%>%
  mutate(winner_adv = as.factor(winner_adv),
         winner_adv = ifelse(winner_adv == "1", 
                             "Minute Advantage", 
                             "Minute Disadvantage"))%>%
  ggplot(aes(x = adv_category, fill = winner_adv))+
  geom_bar(position = "dodge")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  # ggthemes::scale_fill_colorblind()+
  scale_fill_manual(values = c("darkblue", "darkorange"))+
  theme_bw()+
  labs(fill = "Winner Type",
       x = "Minute Difference",
       y = "Number of Final Matches",
       title = "Need to develop a title here")+
  theme(legend.position = "right",
        legend.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank())



