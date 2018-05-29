library(shiny)
library(shinythemes)
library(RCurl)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
#source('~/Documents/github/world_series/scripts/functions.R')



teamIDs <- get_data(table_name = "teamIDs", year = NULL)
teamIDs[1,] <- c("LAA", "LAA", "Los Angeles Angels of Anaheim", 2005)
teamIDs <- teamIDs[c(2:14,1,15:nrow(teamIDs)),]

team_choices <- teamIDs$Team_ID
team_num_years <- 2018 - as.integer(teamIDs$First_Year)
names(team_choices) <- teamIDs$Full_Team_Name
names(team_num_years) <- teamIDs$Full_Team_Name

divisions <- c('NLW', 'NLE', 'ALE', 'ALE', 'NLC', 'ALC', 'NLC', 'ALC', 'NLW',
               'ALC', 'NLE', 'ALW', 'ALC', 'ALW', 'NLW', 'NLC', 'ALC', 'NLE',
               'ALE', 'ALW', 'NLE', 'NLC', 'NLW', 'ALW', 'NLW', 'NLC', 'ALE',
               'ALW', 'ALE', 'NLE')

teamIDs$division <- divisions

names(divisions) <- str_replace_all(divisions, 
                                    c("ALW" = "American League West",
                                      "ALC" = "American League Central",
                                      "ALE" = "American League East",
                                      "NLW" = "National League West", 
                                      "NLC" = "National League Central", 
                                      "NLE" = "National League East"))
division_choices <- sort(unique(divisions))
names(division_choices) <- sort(unique(names(divisions)))




# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   wellPanel(
     
     #make a select box 
     selectInput("select_division", 
                 label = h3("Division"), 
                 choices = division_choices, 
                 selected = division_choices[1]),
     
     uiOutput("division"),
     
     selectInput("select_stat", label = h3("Statistic"),
                 choices = list("Record" = "record_so_far",
                                "Win-Loss Differential" = "win_loss_differential",
                                "Games Ahead/Behind in Division" = "games_ahead",
                                "Cumulative Wins" = "wins_so_far",
                                "Runs" = "runs", 
                                "Runs Allowed" = "runs_allowed",
                                "Run Differential" = "run_diff",
                                "Cumulative Runs" = "r_so_far", 
                                "Cumulative Runs Allowed" = "ra_so_far",
                                "Cumulative Run Differential" = "rd_so_far"),
                 selected = "Runs"),
     
     plotlyOutput("division_stats")
   )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$division <- renderUI({
    
    start_year <- max(as.numeric(teamIDs$First_Year[which(teamIDs$division %in% input$select_division)]))
    sliderInput("division_slider", 
                label = h3("Year"), 
                min = start_year, 
                max = 2017, 
                value = start_year,
                sep = "",
                round = TRUE,
                step = 1,
                animate = animationOptions(interval = 900, playButton = "Play Animation"))
    #animate = TRUE)
  })
  
  division_team_data <- reactive({
    
    x <- teamIDs$Team_ID[which(teamIDs$division %in% input$select_division)]
    x <- lapply(x, function(team, year){
      get_data(team, input$division_slider) %>%
        add_columns(year = input$division_slider)
    })
    bind_rows(x)
  })
  
  output$division_stats <- renderPlotly({
    
    division_team_data() %>%
      # plot_ly(x = ~1:nrow(division_team_data()[which(division_team_data()$team == "ARI"),]),
      #         y = ~1:nrow(division_team_data()[which(division_team_data()$team == "ARI"),]),
      #         text = rep("Hello!", nrow(division_team_data()[which(division_team_data()$team == "ARI"),])),
      #         hoverinfo = 'text')
      
      ggplot(aes(x = runs, y = runs_allowed, color = team)) +
      geom_point()
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

