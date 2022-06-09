#PURPOSE: Testing new Git REPO

library(tidyverse)
library(Lahman)



# Creating year batting summary -------------------------------------------


Batting <- as_tibble(Batting)
year_batting_summary <- Batting%>%
  filter(lgID %in% c("AL", "NL"))%>%
  group_by(yearID)%>%
  summarize_at(vars(H, HR, SO, BB, AB),
               sum, na.rm = TRUE)%>%
  mutate(batting_avg = H/AB)



# Other Stuff -------------------------------------------------------------


Batting%>%
  filter(lgID %in% c("AL", "NL"))%>%
  group_by(yearID)%>%
  summarize_if(is.numeric, sum, na.rm = TRUE)%>%
  mutate(batting_avg = H/AB)

# Testing
# Testing testing testing?

# Here's some other testing -----------------------------------------------

year_batting_summary%>%
  ggplot(aes(x = HR, y = SO))+
  geom_point(alpha = 0.3)




year_batting_summary%>%
  ggplot(aes(x = HR, y = SO))+
  geom_point(alpha = 0.3)+
  labs(title = "this is a trial plot")+
  theme_bw()


# Here's some more test comments?