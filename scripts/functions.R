library(RCurl)
library(plyr)
library(dplyr)
library(stringr)

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







#team_schedule_list[[j]] will be teams schedule for just one year
#will include all of these new stats
#got rid of games_behind to avoid issues when binding

add_columns <- function(original_team_data, year){

team_data <- original_team_data %>% 
  
  mutate(year = as.character(year)) %>%
  
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
  
  mutate(attendance = str_replace_all(attendance, ",", "")) %>%
  
  mutate(attendance = as.numeric(attendance)) %>%
  
  mutate(r_so_far = cumsum(runs)) %>%
  
  mutate(ra_so_far = cumsum(runs_allowed)) %>%
  
  mutate(rd_so_far = cumsum(run_diff)) %>%
  
  mutate(wins_so_far = cumsum(!grepl(x = win_or_loss, pattern='L'))) %>%
  
  mutate(losses_so_far = cumsum(!grepl(x = win_or_loss, pattern='W'))) %>%
  
  mutate(win_loss_differential = wins_so_far - losses_so_far) %>%
  
  mutate(record_so_far =  wins_so_far / (wins_so_far + losses_so_far)) %>%
  
  mutate(attendance_so_far = cumsum(attendance)) %>%
  
  mutate(who_and_where = ifelse(home_or_away == "@", 
                                paste(team, "@", opponent),
                                paste(opponent, "@", team))) %>%
  
  mutate(winner = ifelse(runs > runs_allowed, 
                                team,
                                opponent))

return(team_data)

}




# teamIDs <- get_data(table_name = "teamIDs", year = NULL)
# teamIDs[1,] <- c("LAA", "LAA", "Los Angeles Angels of Anaheim", 2005)
# teamIDs <- teamIDs[c(2:14,1,15:nrow(teamIDs)),]
# 
# team_choices <- teamIDs$Team_ID
# team_num_years <- 2018 - as.integer(teamIDs$First_Year)
# names(team_choices) <- teamIDs$Full_Team_Name
# names(team_num_years) <- teamIDs$Full_Team_Name


combine_years <- function(team, num_years){
  
  #num_years is number of years team has existed
  #team_schedule_list will hold the teams schedule across all years
  team_schedule_list <- rep(list(data.frame()), times = num_years)
  
  for (j in 1:num_years){
    
    #team_schedule_list[[j]] will be teams schedule for just one year
    #will include all of the new stats defined in add columns
    #got rid of games_behind to avoid issues when binding
    year <- 2018 - num_years + (j - 1)
    original_data <- get_data(team, year)
    team_schedule_list[[j]] <- add_columns(original_data, year)
    
  }
  return(bind_rows(team_schedule_list))
}


# combine_years("STL", 136)
# View(combine_years("ARI", 20))
# 
# 
# #write all team-all-years csv's
# for (i in 1:length(team_choices)){
#   
#   team <- team_choices[i]
#   num_years <- team_num_years[i]
#   
#   #write the all year data to a csv (ex: "LAA_all_years.csv")
#   write.csv(x = combine_years(team, num_years),
#             file = file.path(data_directory, paste(team,
#                                                    '_all_years.csv',
#                                                    sep = '')),
#             row.names = FALSE)
# }
# 
# 
# 
# 
# 
# teamIDs$Franchise_ID[1] <- 'LAA'
# teamIDs$Team_ID[1] <- 'LAA'
# teamIDs$First_Year[1] <- as.integer(2005)
# 
# team_choices <- teamIDs$Team_ID
# team_num_years <- 2018 - as.integer(teamIDs$First_Year)
# 
# names(team_choices) <- teamIDs$Full_Team_Name
# names(team_num_years) <- teamIDs$Full_Team_Name
# 
# #I is number of teams
# #name_vec will store names (ex: "LAA2005")
# #all_team_schedule_list[[i]] will be i-th team schedules across all years
# I <-length(team_choices)
# name_vec <- c()
# all_team_schedule_list <- list()
# #for each team...
# for (i in 1:I){
#   
#   #J is number of years team has existed
#   J <- team_num_years[[i]]
#   #team_schedule_list will hold the teams schedule across all years
#   team_schedule_list <- as.list(rep("x",
#                                     times = J))
# 
#   for (j in 1:J){
#     
#     #team_schedule_list[[j]] will be teams schedule for just one year
#     #will include all of these new stats
#     #got rid of games_behind to avoid issues when binding
#     team_schedule_list[[j]] <- get_data(team_choices[i],
#                                         2018 - team_num_years[i] + (j - 1)) %>% 
#       
#       mutate(year = (2018 - team_num_years[i] + (j - 1))) %>%
#       
#       mutate(team = team_choices[i]) %>%
#       
#       mutate(games_ahead = gsub(x = games_behind,
#                                 pattern = 'Tied',
#                                 replacement = '0')) %>%
#       mutate(games_ahead = gsub(pattern = 'up',
#                                 replacement = '-',
#                                 x = games_ahead)) %>%
#       mutate(games_ahead = gsub(pattern = ' ',
#                                 replacement = '',
#                                 x = games_ahead)) %>%
#       mutate(games_ahead = -1*as.numeric(games_ahead)) %>%
#       
#       mutate(run_diff = runs - runs_allowed) %>%
#       
#       mutate(attendance = as.character(attendance)) %>%
#       
#       mutate(attendance = str_remove_all(attendance, ",")) %>%
#       
#       mutate(attendance = as.numeric(attendance)) %>%
#       
#       mutate(r_so_far = cumsum(runs)) %>%
#       
#       mutate(ra_so_far = cumsum(runs_allowed)) %>%
#       
#       mutate(rd_so_far = cumsum(run_diff)) %>%
#       
#       mutate(wins_so_far = cumsum(!grepl(x = win_or_loss, pattern='L'))) %>%
#       
#       mutate(losses_so_far = cumsum(!grepl(x = win_or_loss, pattern='W'))) %>%
#       
#       mutate(record_so_far =  wins_so_far / (wins_so_far + losses_so_far)) %>%
#       
#       mutate(attendance_so_far = cumsum(attendance)) %>%
#       
#       select(-games_behind)
#     
#     
#     #print(team_schedule_list[[j]])
#     
#     #add the name to this list entry
#     names(team_schedule_list)[j] <- paste(team_choices[i],
#                                           as.character(2018 - team_num_years[i] + (j - 1)),
#                                           sep = '')
#     
#   }
#   #make sure we got correct team-year schedules
#   print(names(team_schedule_list))
#   
#   #add all years schedule to all_teams_schedule_list
#   all_team_schedule_list[[i]] <- (bind_rows(team_schedule_list))
#   
#   #write the all year data to a csv (ex: "LAA_all_years.csv")
#   write.csv(x = bind_rows(team_schedule_list),
#             file = file.path(data_directory, paste(team_choices[i],
#                                                    '_all_years.csv',
#                                                    sep = '')),
#             row.names = FALSE)
#   
#   print('.....')
# }

# write.csv(x = bind_rows(all_team_schedule_list),
#           file = file.path(data_directory, 'all_teams_all_years.csv'),
#           row.names = FALSE)



