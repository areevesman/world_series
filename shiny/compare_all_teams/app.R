
### Compare All Teams App ###


library(shiny)
library(shinythemes)
library(RCurl)
library(plyr)
library(dplyr)
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
       selectInput("select", label = h3("Team"), 
                   choices = team_choices, 
                   selected = team_choices[1]),
       
       # Copy the line below to make a date range selector
       dateRangeInput("yearRange", 
                      label = h3("Years"),
                      start = as.Date(teamIDs$teamIDs[which(
                        teamIDs$teamIDs$Full_Team_Name == uiOutput("select")),
                                      "First_Year"],
                        format = 'yyyy'),
                      end = as.Date('2017',
                                    format = 'yyyy'),
                      format = 'yyyy'),
       
       plotOutput("plot1")
     )
   ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

