library(RCurl)
library(plyr)
library(dplyr)
library(stringr)

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

#I is number of teams
#name_vec will store names (ex: "LAA2005")
#all_team_schedule_list[[i]] will be i-th team schedules across all years
I <-length(team_choices)
name_vec <- c()
all_team_schedule_list <- list()
#for each team...
for (i in 1:I){
  
  #J is number of years team has existed
  J <- team_num_years[[i]]
  #team_schedule_list will hold the teams schedule across all years
  team_schedule_list <- as.list(rep("x",
                                    times = J))

  for (j in 1:J){
    
    #team_schedule_list[[j]] will be teams schedule for just one year
    #will include all of these new stats
    #got rid of games_behind to avoid issues when binding
    team_schedule_list[[j]] <- get_data(team_choices[i],
                                        2018 - team_num_years[i] + (j - 1)) %>% 
      
      mutate(year = (2018 - team_num_years[i] + (j - 1))) %>%
      
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
      
      select(-games_behind)
    
    
    #print(team_schedule_list[[j]])
    
    #add the name to this list entry
    names(team_schedule_list)[j] <- paste(team_choices[i],
                                          as.character(2018 - team_num_years[i] + (j - 1)),
                                          sep = '')
    
  }
  #make sure we got correct team-year schedules
  print(names(team_schedule_list))
  
  #add all years schedule to all_teams_schedule_list
  all_team_schedule_list[[i]] <- (bind_rows(team_schedule_list))
  
  #write the all year data to a csv (ex: "LAA_all_years.csv")
  write.csv(x = bind_rows(team_schedule_list),
            file = file.path(data_directory, paste(team_choices[i],
                                                   '_all_years.csv',
                                                   sep = '')),
            row.names = FALSE)
  
  print('.....')
}

# write.csv(x = bind_rows(all_team_schedule_list),
#           file = file.path(data_directory, 'all_teams_all_years.csv'),
#           row.names = FALSE)



#for each_team, fill graph_limits_data (data about min and max of each stat)
graph_limits_data_list <- rep(list(data.frame()), times = I)
for (i in 1:I){
  
  x <- read.csv(file.path(data_directory, paste(team_choices[i],
                                                '_all_years.csv',
                                                sep = '')),
                stringsAsFactors = FALSE)
  
  #change "None" to "0" to avoid issues with missing values
  x[x == "None"] <- "0"
  
  x <- x %>%

  #   mutate(team = team_choices[i]) %>%
  #   
  #   mutate(games_ahead = gsub(x = games_behind,
  #                             pattern = 'Tied',
  #                             replacement = '0')) %>%
  #   mutate(games_ahead = gsub(pattern = 'up',
  #                             replacement = '-',
  #                             x = games_ahead)) %>%
  #   mutate(games_ahead = gsub(pattern = ' ',
  #                             replacement = '',
  #                             x = games_ahead)) %>%
  #   mutate(games_ahead = -1*as.numeric(games_ahead)) %>%
  #   
  #   mutate(run_diff = runs - runs_allowed) %>%
  #   
  #   mutate(attendance = as.character(attendance)) %>%
  #   
  #   mutate(attendance = str_remove_all(attendance, ",")) %>%
  #   
  #   mutate(attendance = as.numeric(attendance)) %>%
  #   
  #   mutate(r_so_far = cumsum(runs)) %>%
  #   
  #   mutate(ra_so_far = cumsum(runs_allowed)) %>%
  #   
  #   mutate(rd_so_far = cumsum(run_diff)) %>%
  #   
  #   mutate(wins_so_far = cumsum(!grepl(x = win_or_loss, pattern='L'))) %>%
  #   
  #   mutate(losses_so_far = cumsum(!grepl(x = win_or_loss, pattern='W'))) %>%
  #   
  #   mutate(record_so_far =  wins_so_far / (wins_so_far + losses_so_far)) %>%
  #   
  #   mutate(attendance_so_far = cumsum(attendance)) %>%
  # 
    mutate_at(c("runs",
                "runs_allowed",
                "innings",
                "rank",
                "attendance",
                "year",
                "games_ahead",
                "run_diff",
                "r_so_far",
                "ra_so_far",
                "rd_so_far",
                "wins_so_far",
                "losses_so_far",
                "record_so_far",
                "attendance_so_far"), as.numeric, na.rm = TRUE)

  #get min and max values for each stat
  min_val <- apply(x,2, min, na.rm = TRUE)
  max_val <- apply(x,2,max, na.rm = TRUE)
 
  #put info in a dataframe, entry in list corresponds to one team
  graph_limits_data_list[[i]] <- data.frame(team = team_choices[i], 
                                            var = names(min_val), 
                                            min_val,
                                            max_val)
  
  
  # for (j in 1:length(graph_limits_data_list[[i]]$min_val)){
  #   
  #   ifelse(graph_limits_data_list[[i]]$min_val[j] == 'Inf',
  #          graph_limits_data_list[[i]]$min_val[j] <- 0, 
  #          graph_limits_data_list[[i]]$min_val[j] <- graph_limits_data_list[[i]]$min_val[j])
  #   ifelse(graph_limits_data_list[[i]]$min_val[j] == '-Inf',
  #          graph_limits_data_list[[i]]$min_val[j] <- 0, 
  #          graph_limits_data_list[[i]]$min_val[j] <- graph_limits_data_list[[i]]$min_val[j])
  #   ifelse(graph_limits_data_list[[i]]$max_val[j] == 'Inf',
  #          graph_limits_data_list[[i]]$max_val[j] <- 0, 
  #          graph_limits_data_list[[i]]$max_val[j] <- graph_limits_data_list[[i]]$max_val[j])
  #   ifelse(graph_limits_data_list[[i]]$max_val[j] == '-Inf',
  #          graph_limits_data_list[[i]]$max_val[j] <- 0, 
  #          graph_limits_data_list[[i]]$max_val[j] <- graph_limits_data_list[[i]]$max_val[j])
  #   
  # }
  # 
  # graph_limits_data_list[[i]]$min_val <- as.numeric(graph_limits_data_list[[i]]$min_val)
  # graph_limits_data_list[[i]]$max_val <- as.numeric(graph_limits_data_list[[i]]$min_val)
   
  # graph_limits_data_list[[i]]$min_val <- ifelse('-Inf', 0, graph_limits_data_list[[i]]$min_val)
  # graph_limits_data_list[[i]]$max_val <- ifelse('Inf', 0, graph_limits_data_list[[i]]$max_val)
  # graph_limits_data_list[[i]]$max_val <- ifelse('-Inf', 0, graph_limits_data_list[[i]]$max_val)
  #  
  
  #write team dataframe to a csv (ex: "LAA_graph_limits.csv")
  write.csv(x = graph_limits_data_list[[i]],
            file = file.path(data_directory, paste(team_choices[i],
                                                   '_graph_limits.csv',
                                                   sep = '')),
            row.names = FALSE)
  
}


team_vec <- as.character(graph_limits_data_list[[1]]$team)
var_vec <- as.character(graph_limits_data_list[[1]]$var)
min_vec <- graph_limits_data_list[[1]]$min_val
max_vec <- graph_limits_data_list[[1]]$max_val
for (i in 2:length(graph_limits_data_list)){
  
  team_vec <- c(team_vec, as.character(graph_limits_data_list[[i]]$team))
  var_vec <- c(var_vec, as.character(graph_limits_data_list[[i]]$var))
  min_vec <- c(min_vec, graph_limits_data_list[[i]]$min)
  max_vec <- c(max_vec, graph_limits_data_list[[i]]$max)
  
}

min_vec <- unlist(min_vec)
min_vec

data.frame(team_vec,
          var_vec,
          min_vec,
          max_vec,
          stringsAsFactors = FALSE)


graph_limits <- bind_rows(graph_limits_data_list)


x <- full_join(graph_limits_data_list[[1]], graph_limits_data_list[[2]])
for(i in 3:length(graph_limits_data_list)){
  x <- full_join(x, graph_limits_data_list[[i]])
}
x



