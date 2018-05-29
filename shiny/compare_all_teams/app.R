
### Compare All Teams App ###


library(shiny)
library(shinythemes)
library(RCurl)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
source('~/Documents/github/world_series/scripts/functions.R')


################





################



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



select_stat_choices <- list("Record" = "record_so_far",
                            "Win-Loss Differential" = "win_loss_differential",
                            "Games Ahead/Behind in Division" = "games_ahead",
                            "Cumulative Wins" = "wins_so_far",
                            "Runs" = "runs", 
                            "Runs Allowed" = "runs_allowed",
                            "Run Differential" = "run_diff",
                            "Cumulative Runs" = "r_so_far", 
                            "Cumulative Runs Allowed" = "ra_so_far",
                            "Cumulative Run Differential" = "rd_so_far")
  


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
  
  
   
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
                   choices = select_stat_choices,
                   selected = "Runs"),
       plotlyOutput("detailed_plot")
       #,
       #plotOutput("ts_plot"),
       #textOutput("test_print")
     )
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
                animate = animationOptions(interval = 900, playButton = "Play Animation"))
                #animate = TRUE)
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
      
      

    c(var[[1,"minimum"]], 1.1*var[[1,"maximum"]])
    
  })
  
  mid_line <- reactive({
    if (input$select_stat == "record_so_far"){
      intercept <- 0.5
    }
    else {
      intercept <- 0
    }
    intercept
  })
  
  output$detailed_plot <- renderPlotly({
    
    x <- 1:nrow(team_data())
    y <- team_data()[[input$select_stat]]
    hover <- paste(team_data()[["date"]], "\n",
                   team_data()[["who_and_where"]], "\n",
                   "Score: ", team_data()[["runs"]], "-", team_data()[["runs_allowed"]],
                   ", ", team_data()[["winner"]], "\n",
                   sep = '')
    
    team_data() %>%
      plot_ly(x = ~x,
              y = ~y, 
              hovertext = hover,
              hoverinfo = "text",
              type = 'scatter', 
              mode = 'lines+markers') %>%
      layout(xaxis = list(title = "Game Number in Season"),
             yaxis = list(title = names(select_stat_choices[select_stat_choices==input$select_stat]),
                          range = limit_data()))

  })
  
  # output$ts_plot <- renderPlot({
  #   
  #   team_data() %>%
  #   ggplot(mapping = aes(x = 1:nrow(team_data()),
  #                        y = team_data()[,input$select_stat])) +
  #     geom_point(size = 2) +
  #     geom_line(size = 1) +
  #     geom_hline(yintercept = mid_line(), color = "red", size = 1) +
  #     xlim(c(-5,165)) +
  #     ylim(limit_data()) +
  #     xlab("Game Number in Season") +
  #     ylab("")
  #   
  # })
  
  output$test_print <- renderPrint({
    
    head(team_data(), 1)
    
  })

 }

# Run the application 
shinyApp(ui = ui, server = server)




