library(RCurl)
library(plyr)
library(dplyr)
library(ggplot2)



d17_URL <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/dodgers2017.csv")
a17_URL <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/astros2017.csv")

d17 <- read.csv(text = d17_URL) %>%
  as_tibble() %>%
  mutate(wins_so_far = cumsum(!grepl(x=W.L,pattern='L')),
         runs_so_far = cumsum(R),
         RA_so_far = cumsum(RA),
         streak = Streak)


a17 <- read.csv(text = a17_URL) %>%
  as_tibble() %>%
  mutate(wins_so_far = cumsum(!grepl(x=W.L,pattern='L')),
         runs_so_far = cumsum(R),
         RA_so_far = cumsum(RA),
         streak = Streak)

combined_init <- bind_rows(d17,a17) 

combined <- tibble(team = combined_init$Tm, 
         game_num = combined_init$Gm., 
         games_ahead = combined_init$GB,
         runs = combined_init$R,
         runs_allowed = combined_init$RA,
         record = combined_init$W.L.1,
         w_or_l = combined_init$W.L,
         wins_so_far = combined_init$wins_so_far,
         runs_so_far = combined_init$runs_so_far,
         RA_so_far = combined_init$RA_so_far,
         streak = combined_init$streak) %>%
  mutate(games_ahead = gsub(x = games_ahead,
                             pattern = 'Tied',
                             replacement = '0')) %>%
  mutate(games_ahead = gsub(pattern = 'up',
                             replacement = '-',
                             x = games_ahead)) %>%
  mutate(games_ahead = gsub(pattern = ' ',
                             replacement = '',
                             x = games_ahead)) %>%
  mutate(games_ahead = -1*as.numeric(games_ahead)) 

#print(combined)

#more successful/interesting
combined %>% 
  ggplot() +
  geom_line(aes(x = game_num, y = games_ahead, color = team), size = 1.5)

combined %>% 
  ggplot() +
  geom_line(aes(x = game_num, y = wins_so_far, color = team), size = 1.5)


combined %>% 
  ggplot() +
  geom_line(aes(x = game_num, y = runs_so_far, color = team), size = 1.5)

combined %>% 
  ggplot() +
  geom_line(aes(x = game_num, y = RA_so_far, color = team), size = 1.5)

combined %>% 
  ggplot() +
  geom_line(aes(x = game_num, y = runs_so_far-RA_so_far, color = team), size = 1.5)







#less successful/interesting
combined %>% 
  ggplot() +
  geom_line(aes(x = game_num, y = nchar(streak), color = team), size = 1.5)




combined %>% 
  ggplot() +
  geom_line(aes(x = game_num, y = runs_so_far-RA_so_far, color = team, size = nchar(streak)))



ggplot() +
  combined[which(combined$team == unique(combined$team)[1]), ] %>% 
  geom_point(mapping = aes(x = runs_allowed, y = runs), color = 'blue', size = 1.5) +
  combined[which(combined$team == unique(combined$team)[2]), ] %>% 
  geom_point(mapping = aes(x = runs_allowed, y = runs), color = 'red', size = 1.5)

ggplot()
