library(RCurl)
library(plyr)
library(dplyr)
library(ggplot2)




get_data <- function(table_name, years = NULL, extention = "csv"){
  
  #get all urls to all table_name data on github
  data_all_years_urls <- getURL(
    paste("https://raw.githubusercontent.com/areevesman/world_series/master/data/",
          table_name,
          as.character(years),
          ".",
          extention,
          sep = ""))
  
  data_all_years_list <- list()
  for (i in 1:length(data_all_years_urls)){
    
    if (extention == "csv"){
      
      data_all_years_list[[i]] <- read.csv(text = data_all_years_urls[i], 
                                           stringsAsFactors = FALSE) %>%
        as_tibble() 
      
    }
    
    if (extention == "txt"){
      
      #print(data_all_years_urls[i])
      
      data_all_years_list[[i]] <- read.table(text = data_all_years_urls[i],
                                             sep = '*',
                                             header = TRUE,
                                             stringsAsFactors = FALSE) %>%
        as_tibble()
      
    }
    
  }
  
  names(data_all_years_list) <- paste(table_name, years, sep = "")
  
  return(data_all_years_list)
  
}




d <- get_data("CHC", 1959, extention = "txt")$CHC1959







