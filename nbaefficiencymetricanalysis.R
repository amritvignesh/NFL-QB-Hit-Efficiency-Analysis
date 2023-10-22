install.packages("tidyverse")
install.packages("remotes")
remotes::install_github("abresler/nbastatR")
remotes::install_github("ccagrawal/nbaTools")
install.packages("ggimage")
install.packages("dplyr")
install.packages("gt")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("png")

library(tidyverse)
library(nbastatR)
library(nbaTools)
library(ggimage)
library(gt)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(png)

Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 10)
pbp <- game_logs(seasons = 2023)
pbp %>% head()

names(pbp)  
nrow(pbp)

ts <- pbp %>%
  group_by(namePlayer) %>%
  mutate(avgts = (100 * (sum(pts)/(2 * (sum(fga) + 0.44 * sum(fta)))))) %>%
  mutate(avgefg = (100 * (sum(fgm) + 0.5 * sum(fg3m))/(sum(fga)))) %>%
  mutate(diff = abs(avgts - avgefg)) %>%
  mutate(vsei = 100 * (avgts/100)^(0.4) * (avgefg/100)^(0.4) * (1-(diff/((avgefg + avgts)/2)))^(0.2)) %>%
  filter(mean(pts) >= 20) %>%
  filter(!((fta == 0) & (fga == 0))) %>%
  filter(n() >= 41) %>%
  summarize(games = n(), ts = avgts, efg = avgefg, diff = diff, vsei = vsei, ppg = mean(pts)) %>%
  ungroup() 

ts <- ts %>% distinct()


ts %>%
  ggplot(aes(x = efg, y = ts, size = vsei, color = ppg)) + geom_point(alpha=0.7) +
  scale_size(name="Versatile Scoring Efficiency Index") +
  geom_hline(yintercept = mean(ts$efg), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(ts$ts), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_text_repel(aes(label=namePlayer), size = 3.5) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm') +
  labs(x = "Average Effective Field Goal Percentage",
       y = "Average True Shooting Percentage", color = "Points Per Game",
       title = "NBA Scoring Efficiency Comparison (20+ PPG, 41+ Games for 22-23 NBA Regular Season)",
       caption = "Amrit Vignesh") + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))
           
           
           
           