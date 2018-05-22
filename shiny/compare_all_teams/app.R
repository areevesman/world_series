
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
teamIDs[1,] <- c("LAA", "LAA", "Los Angeles Angels of Anaheim", 2005)
teamIDs <- teamIDs[c(2:14,1,15:nrow(teamIDs)),]

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
       selectInput("select_team", 
                   label = h3("Team"), 
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
    
    start_year <- as.numeric(teamIDs[which(teamIDs$Team_ID == input$select_team),"First_Year"][[1,1]])
    sliderInput("slider", 
                label = h3("Year"), 
                min = start_year, 
                max = 2017, 
                value = start_year,
                sep = "",
                round = TRUE,
                step = 1,
                animate = animationOptions(interval = 900))
    
  })
  
  team_data <- reactive({
    
    get_data(input$select_team, 
             input$slider,
             extention = 'csv') %>%
      
      add_columns(year = input$slider)
    

    
  })
  
  limit_data <- reactive({
    
    stat <- input$select_stat
    team_all_years <- get_data(paste(input$select_team,
                   "_all_years",
                   sep = ""))
    
    var <- team_all_years[,c("year", stat)] %>%

      mutate_all(function(col){ 
        col[is.na(col)] <- "0"
        col <- as.numeric(col)
        col}) %>%

      summarise(minimum = min(as.numeric(eval(parse(text = input$select_stat))), na.rm = TRUE),
                maximum = max(as.numeric(eval(parse(text = input$select_stat))), na.rm = TRUE)) 
      
      

    c(var[[1,"minimum"]], var[[1,"maximum"]])
    
  })
  
  output$test_print <- renderPrint({

    limit_data()
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
<<<<<<< HEAD
      ylim(limit_data()) +
=======
      ylim(c(-q.q * abs(as.numeric(limit_data()[which(limit_data()$var == input$select_stat), "min_val"][[1,1]])),
             1.1 * as.numeric(limit_data()[which(limit_data()$var == input$select_stat), "max_val"][[1,1]]))) +
>>>>>>> 1d1bf60d396c23bfb64e6af26d410625e79b1b24
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

