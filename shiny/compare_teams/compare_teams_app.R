#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RCurl)
library(plyr)
library(dplyr)
library(ggplot2)



d17_URL <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/dodgers2017.csv")
a17_URL <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/astros2017.csv")

d17 <- read.csv(text = d17_URL) %>%
  as_tibble() %>%
  mutate(wins_so_far = cumsum(!grepl(x=W.L,pattern='L')),
         runs_so_far = cumsum(R),
         RA_so_far = cumsum(RA),
         streak = Streak)


a17 <- read.csv(text = a17_URL) %>%
  as_tibble() %>%
  mutate(wins_so_far = cumsum(!grepl(x=W.L,pattern='L')),
         runs_so_far = cumsum(R),
         RA_so_far = cumsum(RA),
         streak = Streak)

combined_init <- bind_rows(d17,a17) 

combined <- tibble(team = combined_init$Tm, 
                   game_num = combined_init$Gm., 
                   games_ahead = combined_init$GB,
                   runs = combined_init$R,
                   runs_allowed = combined_init$RA,
                   record = combined_init$W.L.1,
                   w_or_l = combined_init$W.L,
                   wins_so_far = combined_init$wins_so_far,
                   runs_so_far = combined_init$runs_so_far,
                   RA_so_far = combined_init$RA_so_far,
                   streak = combined_init$streak) %>%
  mutate(games_ahead = gsub(x = games_ahead,
                            pattern = 'Tied',
                            replacement = '0')) %>%
  mutate(games_ahead = gsub(pattern = 'up',
                            replacement = '-',
                            x = games_ahead)) %>%
  mutate(games_ahead = gsub(pattern = ' ',
                            replacement = '',
                            x = games_ahead)) %>%
  mutate(games_ahead = -1*as.numeric(games_ahead)) 

attach(combined)

# Define UI for application that draws a histogram
ui <- fluidPage(

     
   # Application title
   titlePanel("Compare Opposing World Series Teams"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot")
      )
      
   ),
   
   
   # Sidebar with a checkbox with boxes for statitsic
   sidebarLayout(
     sidebarPanel(
       radioButtons("radio", label = h3("Statistic"), 
                          choices = list("Wins per Games Played" = 'wins_so_far', 
                                         "Runs per Game" = 'runs_so_far',
                                         "Runs Allowed Per Game" = 'RA_so_far',
                                         "Games Ahead in Division" = 'games_ahead'),
                          selected = 'wins_so_far')
     ),
     
     # Show a plot of the generated statistic
     mainPanel(
       plotOutput("statPlot"),
       plotOutput("statPlot2")
     )
     
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
   
   
   
   # You can access the values of the widget (as a vector)
   # with input$checkGroup, e.g.
   output$statPlot <- renderPlot({
     
     combined %>% 
       ggplot(aes(x = game_num, y = combined[,input$radio[1]], color = team)) +
       geom_line(size = 1.5)
      
     
     
     })

   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

