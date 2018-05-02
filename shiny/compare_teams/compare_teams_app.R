#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(RCurl)
library(plyr)
library(dplyr)
library(ggplot2)


### Season Data


#read in all of the data
d17_URL <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/dodgers2017.csv")
a17_URL <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/astros2017.csv")
c16_URL <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/cubs2016.csv")
i16_URL <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/indians2016.csv")
r15_URL <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/royals2015.csv")
m15_URL <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/mets2015.csv")
g14_URL <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/giants2014.csv")
r14_URL <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/royals2014.csv")
c13_URL <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/cardinals2013.csv")
r13_URL <- getURL("https://raw.githubusercontent.com/areevesman/world_series/master/data/redsox2013.csv")


all_urls <- c(d17_URL,a17_URL,
              c16_URL,i16_URL,
              r15_URL,m15_URL,
              g14_URL,r14_URL,
              c13_URL,r13_URL)

#store all 10 dataframes in a list
all_data_frames <- list()
for (i in 1:length(all_urls)){
  
  if (i %in% 1:2){
    year <- 2017
  }
  else if (i %in% 3:4){
    year <- 2016
  }
  else if (i %in% 5:6){
    year <- 2015
  }
  else if (i %in% 7:8){
    year <- 2014
  }
  else {
    year <- 2013
  }
  
  all_data_frames[[i]] <- read.csv(text = all_urls[i], 
                                   stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    mutate(Attendance = ifelse(is.na(Attendance), 
                                     mean(Attendance, na.rm = TRUE),
                                     Attendance)) %>%
    mutate(wins_so_far = cumsum(!grepl(x=W.L,pattern='L')),
           runs_so_far = cumsum(R),
           RA_so_far = cumsum(RA),
           year = year,
           total_attendance = cumsum(Attendance))
  
}
names(all_data_frames) <- c('d17','a17',
                            'c16','i16',
                            'r15','m15',
                            'g14','r14',
                            'c13','r13')



#combine all data frames
combined_init <- bind_rows(all_data_frames) 

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
                   year = combined_init$year,
                   total_attendance = combined_init$total_attendance) %>%
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



### Columns spelled out

labels_df <- data.frame(abrv = c('game_num',
                              'wins_so_far',
                              'runs_so_far',
                              'RA_so_far',
                              'games_ahead',
                              'total_attendance'),
                     spelled_out = c("Games Played", 
                                     "Number of Wins", 
                                     "Number of Runs Scored",
                                     "Number of Runs Allowed", 
                                     "Number of Games Ahead",
                                     "Total Number of Attendees"),
                     stringsAsFactors = FALSE)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),

     
   # Application title
   titlePanel("Compare Opposing World Series Teams"),
   
   
   
   #Sidebar with
   #a select box for year
   #a checkbox with boxes for statitsic
   #a slider range for range of games
   sidebarLayout(
     sidebarPanel(
       
       #make a select box 
       selectInput("select", label = h3("Year"), 
                   choices = list("2017" = 2017, "2016" = 2016, "2015" = 2015,
                                  "2014" = 2014, "2013" = 2013), 
                   selected = 2017),
       
       radioButtons("radio", label = h3("Statistic"), 
                          choices = list("Wins per Games Played" = 'wins_so_far', 
                                         "Runs per Game" = 'runs_so_far',
                                         "Runs Allowed Per Game" = 'RA_so_far',
                                         "Games Ahead in Division" = 'games_ahead',
                                         'Total Attendance throughout Season' = 'total_attendance'),
                          selected = 'wins_so_far'),
       
       sliderInput("slider", label = h3("Range of Regular Season Games"), min = 0, 
                   max = 162, value = c(0,162))
     ),
     
     # Show a plot of the generated statistic
     mainPanel(
       plotOutput("statPlot")
     )
     
   )
     
   
)



# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
  combined_year <- reactive({
    a <- combined %>% filter(year == input$select)
    return(a)
  })
  
  output$statPlot <- renderPlot({
    
    combined_year() %>% 
      ggplot(aes(x = game_num, y = combined_year()[,input$radio], color = team)) +
      geom_line(size = 1.5) +
      xlim(input$slider) +
      xlab((labels_df %>% filter(abrv == 'game_num') %>% select(spelled_out))[1,1]) +
      ylab((labels_df %>% filter(abrv == input$radio) %>% select(spelled_out))[1,1]) +
      ggtitle(paste("Total Number of", 
                    ylab((labels_df %>% filter(abrv == input$radio) %>% select(spelled_out))[1,1])$y,
                    "by Number of", 
                    xlab((labels_df %>% filter(abrv == 'game_num') %>% select(spelled_out))[1,1])$x))
    
  })

   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

