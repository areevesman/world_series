library(RCurl)
library(plyr)
library(dplyr)

data_directory <- "/Users/areevesman/Documents/github/world_series/data"

get_data <- function(table_name, year = NULL, extention = "csv"){
  
  #get all urls to all table_name data on github
  year_data_url <- getURL(
    paste("https://raw.githubusercontent.com/areevesman/world_series/master/data/",
          table_name,
          as.character(year),
          ".",
          extention,
          sep = ""))
  
    if (extention == "csv"){
      
      year_data <- read.csv(text = year_data_url, stringsAsFactors = FALSE) %>%
        as_tibble() 
      
    }
  
  return(year_data)
  
}




#d <- get_data("CHC", 1959, extention = "txt")$CHC1959





teamIDs <- get_data(table_name = "teamIDs", year = NULL)
teamIDs$Franchise_ID[1] <- 'LAA'
teamIDs$Team_ID[1] <- 'LAA'
teamIDs$First_Year[1] <- as.integer(2005)

team_choices <- teamIDs$Team_ID
team_num_years <- 2018 - as.integer(teamIDs$First_Year)

names(team_choices) <- teamIDs$Full_Team_Name
names(team_num_years) <- teamIDs$Full_Team_Name


I <-length(team_choices)
name_vec <- c()
all_team_schedule_list <- list()

# all_team_schedule_list <- as.list(rep("x",
#                                       times = I*sum(team_num_years)))
# names(all_team_schedule_list) <- rep("x",
#                                       times = I*sum(team_num_years))
index <- 1
for (i in 1:I){
  
  J <- team_num_years[[i]]
  team_schedule_list <- as.list(rep("x",
                                    times = J))
  
  #names(team_schedule_list) <- NULL
  #print(paste("i:", i))
  for (j in 1:J){
    
    #print(paste("j:", j))
    #print(paste(team_choices[i],
               # 2018 - team_num_years[i] + (j - 1),
               # sep = ''))
    
    team_schedule_list[[j]] <- mutate_all(get_data(team_choices[i],
                                        2018 - team_num_years[i] + (j - 1)),
                                        as.character) %>% 
      mutate(year = 2018 - team_num_years[i] + (j - 1))
    
    names(team_schedule_list)[j] <- paste(team_choices[i],
                                          as.character(2018 - team_num_years[i] + (j - 1)),
                                          sep = '')
    
    all_team_schedule_list <- append(all_team_schedule_list, team_schedule_list[[j]])
  }
  print(names(team_schedule_list))
  
  print(bind_rows(team_schedule_list))
  
  write.csv(x = bind_rows(team_schedule_list),
            file = file.path(data_directory, paste(team_choices[i],
                                                   '_all_years.csv',
                                                   sep = '')),
            row.names = FALSE)
  
  
  #name_vec <- append(name_vec, names(team_schedule_list))
  
  #print(all_team_schedule_list)
  #print(name_vec)
  
  #names(all_team_schedule_list)[index:(index+J-1)] <- name_vec
  print('.....')
  #print(names(all_team_schedule_list))
  #print('.....')
  index <- index + J
  
}

# write.csv(x = bind_rows(all_team_schedule_list),
#           file = file.path(data_directory, 'all_teams_all_years.csv'),
#           row.names = FALSE)






I <-length(team_choices)
graph_limits_data_list <- rep(list(data.frame()), times = I)
for (i in 1:I){
  
  x <- read.csv(file.path(data_directory, paste(team_choices[i],
                                                '_all_years.csv',
                                                sep = '')),
                stringsAsFactors = FALSE) %>%
    
    #as_tibble() %>%
    
    mutate(team = team_choices[i]) %>%
    
    mutate(games_ahead = gsub(x = games_behind,
                              pattern = 'Tied',
                              replacement = '0')) %>%
    mutate(games_ahead = gsub(pattern = 'up',
                              replacement = '-',
                              x = games_ahead)) %>%
    mutate(games_ahead = gsub(pattern = ' ',
                              replacement = '',
                              x = games_ahead)) %>%
    mutate(games_ahead = -1*as.numeric(games_ahead)) %>%
    
    mutate(run_diff = runs - runs_allowed) %>%
    
    mutate(attendance = as.character(attendance)) %>%
    
    mutate(attendance = str_remove_all(attendance, ",")) %>%
    
    mutate(attendance = as.numeric(attendance)) %>%
    
    mutate(r_so_far = cumsum(runs)) %>%
    
    mutate(ra_so_far = cumsum(runs_allowed)) %>%
    
    mutate(rd_so_far = cumsum(run_diff)) %>%
    
    mutate(wins_so_far = cumsum(!grepl(x = win_or_loss, pattern='L'))) %>%
    
    mutate(losses_so_far = cumsum(!grepl(x = win_or_loss, pattern='W'))) %>%
    
    mutate(record_so_far =  wins_so_far / (wins_so_far + losses_so_far)) %>%
    
    mutate(attendance_so_far = cumsum(attendance)) %>%

    mutate_all(as.numeric, na.rm = TRUE)

    
    
  min_val <- apply(x,2, min, na.rm = TRUE)
  max_val <- apply(x,2,max, na.rm = TRUE)
 
  graph_limits_data_list[[i]] <- data.frame(team = team_choices[i], 
                                            var = names(min_val), 
                                            min_val, max_val)
  
}

graph_limits <- bind_rows(graph_limits_data_list)





