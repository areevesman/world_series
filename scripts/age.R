library(RCurl)
library(plyr)
library(dplyr)
library(ggplot2)


#read in all of the data
standings_url <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/standings2017.csv")
batting_url <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/batting2017.csv")
pitching_url <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/pitching2017.csv")



standings <- read.csv(text = standings_url, 
                     stringsAsFactors = FALSE)[,-1] %>%
  as_tibble() %>% 
  filter(!(Tm  %in% c('Avg','LgAvg','')))%>%
  arrange(Tm)

batting <- read.csv(text = batting_url, 
            stringsAsFactors = FALSE) %>%
  as_tibble() %>% 
  filter(!(Tm  %in% c('Avg','LgAvg','')))%>%
  arrange(Tm)

pitching <- read.csv(text = pitching_url, 
                    stringsAsFactors = FALSE) %>%
  as_tibble() %>% 
  filter(!(Tm  %in% c('Avg','LgAvg','')))%>%
  arrange(Tm)


plot(x = batting$SLG*batting$RBI, y = standings$W)


