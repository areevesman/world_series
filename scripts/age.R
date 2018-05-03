library(RCurl)
library(plyr)
library(dplyr)
library(ggplot2)


#read in all of the data
#standings_url <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/standings2017.csv")
batting_url <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/batting2017.csv")
pitching_url <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/pitching2016.csv")



batting <- read.csv(text = batting_url, 
            stringsAsFactors = FALSE) %>%
  as_tibble()






