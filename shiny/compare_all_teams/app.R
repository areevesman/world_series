
### Compare All Teams App ###


library(shiny)
library(shinythemes)
library(RCurl)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)

#URLs to data on github


teamIDs <- get_data(table_name = "teamIDs", years = NULL)
teamIDs$teamIDs$Franchise_ID[1] <- 'LAA'
teamIDs$teamIDs$Team_ID[1] <- 'LAA'
teamIDs$teamIDs$First_Year[1] <- as.integer(2005)

team_choices <- teamIDs$teamIDs$Team_ID
names(team_choices) <- teamIDs$teamIDs$Full_Team_Name

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
                   choices = list("Runs" = "runs", 
                                  "Runs Allowed" = "runs_allowed", 
                                  "Games Ahead/Behind" = "games_ahead",
                                  "Attendance" = "attendance"),
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
    
    year <- teamIDs$teamIDs[which(teamIDs$teamIDs$Team_ID == input$select_team),"First_Year"][[1,1]]
    sliderInput("slider", label = h3("Years"), 
                min = year, 
                max = 2017, 
                value = c(2016, 2017),
                sep = "",
                round = TRUE,
                step = 1)
    
  })
  
  team_data <- reactive({
    
    team_data_list <- get_data(input$select_team, 
                               input$slider[1]:input$slider[2],
                               extention = 'txt')
    
    for (i in 1:length(team_data_list)){
      
      team_data_list[[i]] <- team_data_list[[i]] %>%
        mutate(year = str_sub(names(team_data_list)[[i]],-4,-1))
      
    }
    return(team_data_list)
    
  })
  
  output$test_print <- renderPrint({

    team_data()[[1]]

  })
  
  output$ts_plot <- renderPlot({
    
    ggplot() +
    team_data()[[1]] %>%
      geom_line(mapping = aes(x = 1:nrow(team_data()[[1]]),
                              y = team_data()[[1]][,input$select_stat]))
    
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
