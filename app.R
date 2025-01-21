## SHINY APP LEADERBOARD
## this web app will allow the user to filter through subjects and choose their
## subject id to view their statistics plus some bonus visualizations like 
## the paths they took


## -----------------------------------------------------------------------------
##      load libraries
## -----------------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(DBI) # connect to db
library(RMariaDB) # access mysql
library(dplyr) # wrangling
library(readr) # read in files
library(ggplot2) # plots

## -----------------------------------------------------------------------------
##      source functions and create data to be used by app on startup
## -----------------------------------------------------------------------------

# connect to db and pull location and forage data
con <- dbConnect(
  MariaDB(),
  dbname = "psych270_coconuts", 
  host = "s161.servername.online",
  port = 3306,
  username = "psych270_data_access", 
  password = "K=2vtcy1W3wq"
)

# write data to dataframes
forage_data <- dbReadTable(con, "ForageData")
location_data <- dbReadTable(con, "LocationData")

# close connection
dbDisconnect(con)

# grab object location data
obj_location_data <- read_csv("data/arrangements/object-location-data.csv")

source("scripts/fun/find-who-completed.R") # find completed levels and runs
source("scripts/fun/calc-perform-metrics.R") # find distance and time
source("scripts/fun/plot-path.R") # create nice path plot

# create performance summary to create plots and calculate stats, will calculate
# steps and time in each level, but does not indicate whether they completed
# that level
performance_summary <- calc_perform_metrics(location_data)

complete_perform_summary <- tibble(
  subject = numeric(), level = character(), total_dist_in_lvl = numeric(), 
  total_time_in_lvl = numeric()
)

# loop through levels to see who has completed what and return usable, 
# comparable level runs
for(i in 1:10) {
  lvl_string <- paste("_level_", i, sep = "")
  
  subj_to_keep <- who_completed(forage_data, lvl_string)
  
  lvl_completed <- performance_summary |> 
    dplyr::filter(subject == subj_to_keep & level == lvl_string)
  
  complete_perform_summary <- rbind(complete_perform_summary, lvl_completed)
}


## -----------------------------------------------------------------------------
##      UI
## -----------------------------------------------------------------------------

# this is the ui that the user interacts with, chooses subject and level

ui <- fluidPage(
  # set theme
  theme = shinytheme("flatly"),
  
  # set navbar
  navbarPage(
    # set 1st tab of navbar
    tabPanel(
      # set title of tab
      title = "Player stats", 
      # create sidebar panel of input widgets
      sidebarPanel(
        # create heading w html h3 tags
        tags$h3("Find a player"),
        # create text input for subject/player id
        textInput("subject", "Player:", 1), 
        # create dropdown input
        selectInput(
          "lvl", "Choose level",
          choices = list(
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10 
          ), 
          selected = 1
        ) 
      ), 
      
      # create main panel with cards
      mainPanel(
        h1("Performance Summary"), 
        
        # written report of percentile score on that level
        verbatimTextOutput("txtout")
        
      )
    ), 
    # other tab panels to fill if you want
    tabPanel(
      title = "Navbar 2", "This panel is intentionally left blank."
    ), 
    tabPanel(
      title = "Navbar 3", "This is another panel intentionally left blank."
    )
  )
) # end ui

## -----------------------------------------------------------------------------
##      server
## -----------------------------------------------------------------------------

# define server function, where the ui input is turned into analyses and plots
server <- function(input, output) {

  # filter performance summary for subject and level
  
  # create the text output from percentile score
  output$txtout <- renderPrint({
    performance_summary |>
      dplyr::filter(subject == input$subject & level == paste("_level_", input$lvl)) |> 
      pull(total_dist_in_lvl)
  })
}

## -----------------------------------------------------------------------------
##      create shiny object
## -----------------------------------------------------------------------------

# create app
shinyApp(ui = ui, server = server)