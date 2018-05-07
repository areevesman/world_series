library(RCurl)
library(plyr)
library(dplyr)
library(ggplot2)




get_data <- function(table_name, years){
  
  #get all urls to all table_name data on github
  data_all_years_urls <- getURL(
    paste("https://raw.githubusercontent.com/areevesman/world_series/master/data/",
          table_name,
          as.character(years),
          ".csv",
          sep = ""))
  
  data_all_years_list <- list()
  for (i in 1:length(data_all_years_urls)){
    
    data_all_years_list[[i]] <- read.csv(text = data_all_years_urls[i], 
                                         stringsAsFactors = FALSE) %>%
      as_tibble() %>% 
      mutate(year = years[i])
    
  }
  
  names(data_all_years_list) <- paste(table_name, years, sep = "")
  
  return(data_all_years_list)
  
}



#get_data("cubs", 2016)







years <- 2006:2017

#get all urls to all table_name data on github
data_all_years_urls <- getURL(
  paste("https://raw.githubusercontent.com/areevesman/world_series/master/data/",
        table_name,
        as.character(years),
        ".csv",
        sep = ""))

#get all tables
data_all_years_list <- list()
for (i in 1 :length(data_all_years_urls)){
  
  data_all_years_list[[i]] <- read.csv(text = data_all_years_urls[i], 
                                            stringsAsFactors = FALSE) %>%
    as_tibble() %>% 
    mutate(year = years[i])
  
}
standings_all_years <- bind_rows(standings_all_years_list)







