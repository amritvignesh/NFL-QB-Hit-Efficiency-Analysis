install.packages("tidyverse")
install.packages("nflfastR")
install.packages("ggimage")
install.packages("gt")
install.packages("nflreadr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("nflplotR")
install.packages("ggrepel")

library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(nflreadr)
library(dplyr)
library(ggplot2)
library(nflplotR)
library(ggrepel)

pbp <- load_pbp(2022)
pbp %>% head()

names(pbp)
nrow(pbp)

qb_stats <- pbp %>%
  filter(pass == 1) %>%
  filter(!is.na(passer_player_name)) %>%
  group_by(passer_player_name) %>%
  summarize(pass_attempts = n(), posteam = last(posteam), hit_to_pass_ratio = mean(qb_hit), avg_epa = mean(qb_epa)) %>%
  filter(pass_attempts >= 100) %>%
  ungroup() %>%
  print(n = 47)

load_teams()
qb_stats <- qb_stats %>%
  left_join(load_teams(), by = c("posteam" = "team_abbr"))

qb_stats %>%
  ggplot(aes(x = hit_to_pass_ratio, y = avg_epa)) +
  geom_hline(yintercept = mean(qb_stats$avg_epa), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(qb_stats$hit_to_pass_ratio), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_nfl_logos(aes(team_abbr = posteam), width = 0.03, alpha = 0.75) +
  geom_text_repel(aes(label=passer_player_name), size = 3.5) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm') +
  labs(x = "QB Hit to Pass Attempt Ratio",
       y = "Average EPA",
       title = "Quarterback Efficiency When Hit in 2022 (At Least 100 Pass Attempts)",
       caption = "Amrit Vignesh") + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))
       
  