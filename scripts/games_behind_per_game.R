library(RCurl)
library(plyr)
library(dplyr)
library(ggplot2)



x <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/dodgers2017.csv")

d17 <- read.csv(text = x) %>%
  as_tibble()

gb <- tibble(game_num = d17$Gm., games_behind = d17$GB) %>%
  mutate(games_behind = gsub(x = games_behind,
                             pattern = 'Tied',
                             replacement = '0')) %>%
  mutate(games_behind = gsub(pattern = 'up',
                             replacement = '-',
                             x = games_behind)) %>%
  mutate(games_behind = gsub(pattern = ' ',
                             replacement = '',
                             x = games_behind)) %>%
  mutate(games_behind = -1*as.numeric(games_behind))


print(gb)

gb %>% 
  ggplot(aes(x = game_num, y = games_behind)) +
  geom_line(col = 'blue', size = 1.5)








