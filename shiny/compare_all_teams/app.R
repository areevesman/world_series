
### Compare All Teams App ###


library(shiny)
library(shinythemes)
library(RCurl)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
#source('~/Documents/github/world_series/scripts/functions.R')


################





################



teamIDs <- get_data(table_name = "teamIDs", year = NULL)
teamIDs$Franchise_ID[1] <- 'LAA'
teamIDs$Team_ID[1] <- 'LAA'
teamIDs$First_Year[1] <- as.integer(2005)

team_choices <- teamIDs$Team_ID
team_num_years <- 2018 - as.integer(teamIDs$First_Year)

names(team_choices) <- teamIDs$Full_Team_Name
names(team_num_years) <- teamIDs$Full_Team_Name






# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
   
   # Application title
   titlePanel("A History of Major League Baseball's Active Franchises"),
   
   verticalLayout(
     wellPanel(
       #make a select box 
       selectInput("select_team", label = h3("Team"), 
                   choices = team_choices, 
                   selected = team_choices[1]),
       uiOutput("team"),
       
       selectInput("select_stat", label = h3("Statistic"),
                   choices = list("Record" = "record_so_far",
                                  "Runs" = "runs", 
                                  "Runs Allowed" = "runs_allowed", 
                                  "Run Differential" = "run_diff",
                                  "Attendance" = "attendance",
                                  "Cumulative Runs" = "r_so_far", 
                                  "Cumulative Runs Allowed" = "ra_so_far",
                                  "Cumulative Run Differential" = "rd_so_far",
                                  "Cumulative Wins" = "wins_so_far",
                                  "Cumulative Attendance" = "attendance_so_far",
                                  "Games Ahead/Behind in Division" = "games_ahead"),
                   selected = "Runs"),
       plotOutput("ts_plot"),
       textOutput("test_print")
     )
       
    #    # Copy the line below to make a slider range 
    #    sliderInput("slider", label = h3("Years"), min = uiOutput("year"), 
    #                max = 2017, value = c(2001, 2010)),
    #    uiOutput("year")
    #  )
    # )
   #    
   #    # Show a plot of the generated distribution
   #    mainPanel(
   #       plotOutput("distPlot")
   #    )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$team <- renderUI({
    
    year <- teamIDs[which(teamIDs$Team_ID == input$select_team),"First_Year"][[1,1]]
    sliderInput("slider", label = h3("Year"), 
                min = year, 
                max = 2017, 
                value = year,
                sep = "",
                round = TRUE,
                step = 1,
                animate = animationOptions(interval = 900))
    
  })
  
  team_data <- reactive({
    
    get_data(input$select_team, 
                          input$slider,
                          extention = 'csv') %>%
      
      mutate(year = input$slider) %>%
      
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
      
      mutate(attendance_so_far = cumsum(attendance))
    
  })
  
  limit_data <- reactive({
    
    get_data(paste(input$select_team,
                   "_graph_limits",
                   sep = ""))
    
  })
  
  output$test_print <- renderPrint({

    #team_data()$attendance
    # c(-1.1 * abs(limit_data()[which(limit_data()$var == input$select_stat), "min_val"][[1,1]]),
    #   1.1 * limit_data()[which(limit_data()$var == input$select_stat), "max_val"][[1,1]])
    
  })
  
  output$ts_plot <- renderPlot({
    
    team_data() %>%
    ggplot(mapping = aes(x = 1:nrow(team_data()),
                         y = team_data()[,input$select_stat])) +
      geom_point(size = 2) +
      geom_line(size = 1) +
      xlim(c(-5,165)) +
      ylim(c(-q.q * abs(as.numeric(limit_data()[which(limit_data()$var == input$select_stat), "min_val"][[1,1]])),
             1.1 * as.numeric(limit_data()[which(limit_data()$var == input$select_stat), "max_val"][[1,1]]))) +
      xlab("Game Number in Season") +
      ylab("")
    
  })
    

  # output$year <- reactive({
  #   a <- as.integer(teamIDs$teamIDs[which(teamIDs$teamIDs$Team_ID == input$select),"First_Year"][[1,1]])
  #   return(a)
  # })
  
  

 }

# Run the application 
shinyApp(ui = ui, server = server)


#blah <- get_data("ATL", years = 2000:2005, extention = "txt")
# lapply(team_data_list,
#        mutate,
#        year = names(team_data_list))

#nrow(blah[[1]])




# x[[1]] <- x[[1]] %>%
#   mutate(year = str_sub(names(x)[[1]],-4,-1),
#          attendance = ifelse("None",NA,as.integer(str_replace(attendance,",","")))) %>%
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
#   mutate(run_diff = runs - runs_allowed)

