# Purpose - Explore Hypothesis / RQ1 

### RQ1 - How does a player's proportion of break points saved relate to their winning percentage?

# Load in data
library(tidyverse)
# library(patchwork)
library(cowplot)
wta <- read_csv("Data/wta.csv")
duplicate_match <- read_csv("Data/duplicate_match_wta.csv")
indiv_player_match <- read_csv("Data/indiv_player_match_wta.csv")


# Individual match break points - Hypothesis 1

bp_stats <- indiv_player_match%>%
  mutate(win = ifelse(result == "W", 1, 0))%>%
  group_by(player_name)%>%
  summarize(matches = n(),
            wins = sum(win, na.rm = TRUE),
            sum_faced = sum(bpFaced, na.rm = TRUE),
            sum_saved = sum(bpSaved, na.rm = TRUE),
            mean_faced = mean(bpFaced, na.rm = TRUE),
            mean_saved = mean(bpSaved, na.rm = TRUE))%>%
  mutate(prop_saved = sum_saved / sum_faced,
         win_pct = wins / matches)


bp_stats%>%
  ggplot(aes(x = sum_faced))+
  geom_histogram(binwidth = 25)+
  theme_bw()



bp_stats%>%
  ggplot(aes(x = matches, y = sum_faced))+
  geom_point(alpha = 0.5)

# With players who played > 50 matches
p1 <- bp_stats%>%
  filter(matches >= 50)%>%
  ggplot(aes(x = prop_saved, y = win_pct))+
  geom_point(aes(color = mean_faced),alpha = 0.75, size = 4)+
  scale_color_gradient(limits = c(5, 12), low = "darkorange", high = "darkblue")+
  theme_bw()+
  labs(x = "Proportion of Break Points Saved",
       y = "Winning Percentage",
       color = "Average Break Points Faced",
       title = "Winning players face fewer break points\n
       and save break points at better rates",
       caption = "Minimum 50 matches played")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# With players who played < 50 matches
p2 <- bp_stats%>%
  filter(matches < 50, matches > 5, mean_faced < 12)%>%
  ggplot(aes(x = prop_saved, y = win_pct))+
  geom_point(aes(color = mean_faced),alpha = 0.75, size = 4)+
  scale_color_gradient(limits = c(5, 12), low = "darkorange", high = "darkblue")+
  theme_bw()+
  # geom_smooth(method = "lm")+
  labs(x = "Proportion of Break Points Saved",
       y = "Winning Percentage",
       color = "Avg BP Faced",
       # title = "Winning players face fewer break points and save break points at better rates",
       caption = "Between 5 & 50 matches played")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position =c(0.8, 0.2))

cowplot::plot_grid(p1, p2, labels = "AUTO")

bp_stats%>%
  filter(matches >= 50)%>%
  ggplot(aes(x = prop_saved, y = mean_faced))+
  geom_point(alpha = 0.5, size = 3)
# There aren't really any player who are anomalies in that 
# they face a bunch of break points and save at a high percentage
# No one is really in the top right corner