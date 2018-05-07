# library(shiny)
# library(shinythemes)
library(RCurl)
library(plyr)
library(dplyr)
library(ggplot2)

years <- 2006:2017

#get all urls to standings data on github
standings_all_years_urls <- getURL(
  paste("https://raw.githubusercontent.com/areevesman/world_series/master/data/standings",
        as.character(years),
        ".csv",
        sep = ""))

#get all standings tables
standings_all_years_list <- list()
for (i in 1 :length(standings_all_years_urls)){
  
  standings_all_years_list[[i]] <- read.csv(text = standings_all_years_urls[i], 
                                            stringsAsFactors = FALSE) %>%
    as_tibble() %>% 
    arrange(team) %>%
    mutate(year = years[i])
  
}
standings_all_years <- bind_rows(standings_all_years_list)



#get all urls to batting data on github
batting_all_years_urls <- getURL(
  paste("https://raw.githubusercontent.com/areevesman/world_series/master/data/batting",
        as.character(years),
        ".csv",
        sep = ""))

#get all batting tables
batting_all_years_list <- list()
for (i in 1 :length(batting_all_years_urls)){
  
  batting_all_years_list[[i]] <- read.csv(text = batting_all_years_urls[i],
                                          row.names = NULL,
                                          stringsAsFactors = FALSE) %>%
    as_tibble() %>% 
    arrange(team) %>%
    mutate(year = years[i])
  
}
batting_all_years <- bind_rows(batting_all_years_list)


#get all urls to pitching data on github
pitching_all_years_urls <- getURL(
  paste("https://raw.githubusercontent.com/areevesman/world_series/master/data/pitching",
        as.character(years),
        ".csv",
        sep = ""))

#get all pitching tables
pitching_all_years_list <- list()
for (i in 1 :length(pitching_all_years_urls)){
  
  pitching_all_years_list[[i]] <- read.csv(text = pitching_all_years_urls[i],
                                          row.names = NULL,
                                          stringsAsFactors = FALSE) %>%
    as_tibble() %>% 
    arrange(team) %>%
    mutate(year = years[i])
  
}
pitching_all_years <- bind_rows(pitching_all_years_list)


#get all urls to fielding data on github
fielding_all_years_urls <- getURL(
  paste("https://raw.githubusercontent.com/areevesman/world_series/master/data/fielding",
        as.character(years),
        ".csv",
        sep = ""))

#get all fielding tables
fielding_all_years_list <- list()
for (i in 1 :length(fielding_all_years_urls)){
  
  fielding_all_years_list[[i]] <- read.csv(text = fielding_all_years_urls[i],
                                           row.names = NULL,
                                           stringsAsFactors = FALSE) %>%
    as_tibble() %>% 
    arrange(team) %>%
    mutate(year = years[i])
  
}
fielding_all_years <- bind_rows(fielding_all_years_list)


#combine all stats, will have repeat columns for team, year
#it is okay because they are all arranged by team
all_stats <- bind_cols(standings_all_years,
                       batting_all_years,
                       pitching_all_years,
                       fielding_all_years)

#colnames(all_stats)

all_stats$batting_runs


#########



lm.out <- lm(wins ~ batting_stolen_bases + 
               batting_home_runs +
               batting_hits +
               batting_bases_on_walks +
               strikeouts +
               ERA +
               ERAp +
               wild_pitches+
               league,
             data = all_stats)

# lm.out <- lm(batting_runs ~ batting_stolen_bases + 
#                batting_caught_stealing,
#              data = all_stats)

lm.out
summary(lm.out)



all_stats %>% 
  ggplot(aes(x=wins, y=wild_pitches, color = team)) +
  geom_point()


all_stats %>% 
  ggplot(aes(x=batting_stolen_bases, y=batting_runs, color = team)) +
  geom_point()



all_stats %>% 
  ggplot(aes(x=strikeouts, y=pitching_wins, color = ERA)) +
  geom_point() +
  geom_smooth(method = 'lm')
  

all_stats %>% 
  ggplot(aes(x=batting_avg_age, y=batting_home_runs)) +
  geom_point() +
  geom_smooth(method = 'lm')

all_stats %>%
  ggplot(aes(x=paste(team,year), y=wins)) +
  geom_point()





#######

batting_some_vars <- batting_all_years %>% 
  filter(year %in% 2006:2017) %>%
  select(team, 
         year, 
         batting_avg_age, 
         batting_runs_per_game, 
         batting_hits,
         batting_home_runs,
         batting_RBIs,
         batting_stolen_bases,
         batting_caught_stealing,
         batting_bases_on_walks,
         batting_strikeouts,
         batting_ave,
         SLG,
         batting_double_plays,
         sac_bunts,
         sac_flies,
         batting_runners_lob)

k.out <- kmeans(x = batting_some_vars[,3:ncol(batting_some_vars)],
       centers = 2)

prcomp.out <- prcomp(x = batting_some_vars[,3:ncol(batting_some_vars)],
                     center = TRUE,
                     scale = TRUE)

prcomp.out$x

k.out$cluster


for (pc in 1:4){
  
  for (pc_ in (pc+1):5){
    
    print(paste(pc,pc_))
    
    new <- prcomp.out$x %>%
      as_tibble() %>%
      select(pc,pc_) %>%
      mutate(cluster = as.character(k.out$cluster))
    
    print(new %>% 
            ggplot(aes(x = new[,1],y = new[,2], color = cluster)) +
            geom_point() +
            ggtitle(paste('PC', pc, ' vs PC', pc_, sep = '')))
    
  }
  
}

dat <- bind_cols(all_stats[,c('team','year','r_per_game','wins')],
                 as_tibble(x = k.out$cluster),
                 as_tibble(prcomp.out$x))


dat %>%
  ggplot(aes(x = PC2, y = wins, color = as.character(value))) +
  geom_point() 

lm.dat <- lm(wins~PC1+PC2 + as.character(value), data = dat)

summary(lm.dat)








pitching_some_vars <- pitching_all_years %>% 
  filter(year %in% 2006:2017) %>%
  select(team, 
         year, 
         pitching_avg_age, 
         pitching_ra_per_game, 
         ERA,
         pitching_hits_allowed,
         innings_pitched,
         pitching_hr_allowed,
         pitching_bases_on_walks,
         strikeouts,
         times_hit_by_pitch,
         wild_pitches,
         pitching_runners_lob)

k.out <- kmeans(x = pitching_some_vars[,3:ncol(pitching_some_vars)],
                centers = 2)

prcomp.out <- prcomp(x = pitching_some_vars[,3:ncol(pitching_some_vars)],
                     center = TRUE,
                     scale = TRUE)

prcomp.out$x

k.out$cluster


for (pc in 1:4){
  
  for (pc_ in (pc+1):5){
    
    print(paste(pc,pc_))
    
    new <- prcomp.out$x %>%
      as_tibble() %>%
      select(pc,pc_) %>%
      mutate(cluster = as.character(k.out$cluster))
    
    print(new %>% 
            ggplot(aes(x = new[,1],y = new[,2], color = cluster)) +
            geom_point() +
            ggtitle(paste('PC', pc, ' vs PC', pc_, sep = '')))
    
  }
  
}



dat <- bind_cols(all_stats[,c('team','year','wins')],
                as_tibble(x = k.out$cluster),
                as_tibble(prcomp.out$x))


dat %>%
  ggplot(aes(x = PC1, y = PC2, color = wins > 81)) +
  geom_point() 

lm.dat <- lm(wins~PC1 + as.character(value), data = dat)

summary(lm.dat)






fielding_some_vars <- fielding_all_years %>% 
  filter(year %in% 2006:2017) %>%
  select(team, 
         year, 
         field_ra_per_game, 
         defEff,
         field_innings_played,
         defensive_chances,
         putouts,
         assists,
         field_errors,
         double_plays,
         fielding_pct)

k.out <- kmeans(x = fielding_some_vars[,3:ncol(fielding_some_vars)],
                centers = 2)

prcomp.out <- prcomp(x = fielding_some_vars[,3:ncol(fielding_some_vars)],
                     center = TRUE,
                     scale = TRUE)

prcomp.out$x

k.out$cluster


for (pc in 1:4){
  
  for (pc_ in (pc+1):5){
    
    print(paste(pc,pc_))
    
    new <- prcomp.out$x %>%
      as_tibble() %>%
      select(pc,pc_) %>%
      mutate(cluster = as.character(k.out$cluster))
    
    print(new %>% 
            ggplot(aes(x = new[,1],y = new[,2], color = cluster)) +
            geom_point() +
            ggtitle(paste('PC', pc, ' vs PC', pc_, sep = '')))
    
  }
  
}



dat <- bind_cols(all_stats[,c('team','year','wins')],
                 as_tibble(x = k.out$cluster),
                 as_tibble(prcomp.out$x))


dat %>%
  ggplot(aes(x = PC1, y = PC2, color = wins > 81)) +
  geom_point() 

lm.dat <- lm(wins~PC1 + PC2, data = dat)

summary(lm.dat)









all_stats_some_vars <- all_stats %>% 
  filter(year %in% 2006:2017) %>%
  select(team, 
         year,
         batting_runs_per_game, 
         batting_hits,
         batting_home_runs,
         batting_RBIs,
         batting_stolen_bases,
         batting_caught_stealing,
         batting_bases_on_walks,
         batting_strikeouts,
         batting_ave,
         SLG,
         batting_double_plays,
         sac_bunts,
         sac_flies,
         batting_runners_lob,
         pitching_avg_age, 
         pitching_ra_per_game, 
         ERA,
         pitching_hits_allowed,
         innings_pitched,
         pitching_hr_allowed,
         pitching_bases_on_walks,
         strikeouts,
         times_hit_by_pitch,
         wild_pitches,
         pitching_runners_lob,
         field_ra_per_game, 
         defEff,
         field_innings_played,
         defensive_chances,
         putouts,
         assists,
         field_errors,
         double_plays,
         fielding_pct)

k.out <- kmeans(x = all_stats_some_vars[,3:ncol(all_stats_some_vars)],
                centers = 2)

prcomp.out <- prcomp(x = all_stats_some_vars[,3:ncol(all_stats_some_vars)],
                     center = TRUE,
                     scale = TRUE)

prcomp.out$x
prcomp.out$rotation[sort(prcomp.out$rotation[,'PC2']),1:3]


k.out$cluster


for (pc in 1:4){
  
  for (pc_ in (pc+1):5){
    
    print(paste(pc,pc_))
    
    new <- prcomp.out$x %>%
      as_tibble() %>%
      select(pc,pc_) %>%
      mutate(cluster = as.character(k.out$cluster))
    
    print(new %>% 
            ggplot(aes(x = new[,1],y = new[,2], color = cluster)) +
            geom_point() +
            ggtitle(paste('PC', pc, ' vs PC', pc_, sep = '')))
    
  }
  
}



dat <- bind_cols(all_stats[,c('team','year','wins')],
                 as_tibble(x = k.out$cluster),
                 as_tibble(prcomp.out$x))


dat %>%
  ggplot(aes(x = PC2, y = wins, alpha = .05)) +
  geom_point() +
  geom_smooth(method='lm')

lm.dat <- lm(wins~PC1 + PC2 + PC3 + PC5 + PC6 + PC7 + PC9 + PC10, data = dat)

lm.dat <- lm(wins~PC2, data = dat)

summary(lm.dat)

plot(residuals(lm.dat))

plot(dist(all_stats_some_vars[,3:ncol(all_stats_some_vars)]))
h.out <- hclust(dist(all_stats_some_vars[,3:ncol(all_stats_some_vars)]))
h.out$labels <- paste(dat$team, dat$year, sep = '_')
plot(h.out, cex = .2)

