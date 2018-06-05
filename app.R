library(dplyr)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(purrr)
library(shiny)
library(shinydashboard)

# Define UI for application that draws dashboard
header <- dashboardHeader(disable = TRUE)
sidebar <- dashboardSidebar(collapsed = FALSE,
                            sidebarMenu(
                              id = "tabs",                              
                              menuItem("Twitter Data", tabName = "twitter"),
                              menuItem("Govt-Matatu Data", tabName = "matatu"),
                              menuItem("Govt-Fatalities Data", tabName = "govt")
                            ),
                            sidebarMenu(
                              conditionalPanel("input.tabs === 'twitter'",
                                               checkboxGroupInput("years_twitter", label = "Year",
                                                                  choices = c(
                                                                    "2013" = 2013,
                                                                    "2014" = 2015,
                                                                    "2015" = 2015,
                                                                    "2016" = 2016,
                                                                    "2017" = 2017,
                                                                    "2018" = 2018),
                                                                  selected = "2018"),
                                               checkboxGroupInput("hours_twitter", label = "Time of Day",
                                                                  choices = c(
                                                                    "01:00-04:59" = "01:00-04:59",
                                                                    "05:00-09:59" = "05:00-09:59",
                                                                    "10:00-13:59" = "10:00-13:59",
                                                                    "14:00-17:59" = "14:00-17:59",
                                                                    "18:00-21:59" = "18:00-21:59",
                                                                    "22:00-00:59" = "22:00-00:59"
                                                                  ),
                                                                  selected = c("01:00-04:59", "05:00-09:59",
                                                                               "10:00-13:59", "14:00-17:59",
                                                                               "18:00-21:59", "22:00-00:59")
                                                                  )),
                              conditionalPanel("input.tabs == 'matatu'",
                                               checkboxGroupInput("years_matatu", label = "Year",
                                                                  choices = c(
                                                                    "2012" = 2012,
                                                                    "2013" = 2013,
                                                                    "2014" = 2014
                                                                  ),
                                                                  selected = c(2012, 2013, 2014)
                                               ),
                                               sliderInput("passengers", label = "Number of Passengers", min = 0, max = 70, value = c(10,20)),
                                               checkboxGroupInput("injuries", label = "Were there Injuries?",
                                                                  choices = c(
                                                                    "Injuries" = "Injuries",
                                                                    "No Injuries" = "No Injuries"
                                                                  ),
                                                                  selected = c("Injuries", "No Injuries")
                                               )
                                               ),
                              conditionalPanel("input.tabs == 'govt'",
                                               checkboxGroupInput("years_govt", label = "Year",
                                                                  choices = c(
                                                                    "2017" = 2017,
                                                                    "2018" = 2018),
                                                                  selected = c("2018", "2017")
                                            )
                            )
                          )
)

body <- dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  tabItems(
    tabItem(tabName = "twitter",
      fluidRow(
        box(width = 12, height = 800, title = "Map of Geocoded Twitter-Reported Crashes",
            leafletOutput("map_twitter", height = 700)
        ))
      ),
    tabItem(tabName = "matatu",
            fluidRow(
              box(width = 12, height = 800, title = "Map of Police Stations Reporting Matatu Crashes",
                  leafletOutput("map_matatu", height = 700)
              )
            )
          ),
    tabItem(tabName = "govt",
            fluidRow(
              box(width = 12, height = 800, title = "Map of Geocoded Government Fatality Reports",
                  leafletOutput("map_govt", height = 700)
              )
            )
          )
    )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
    )

# Define server logic
server <- function(input, output) {
  
  binded_data <- readRDS("data/binded_data.rds")
  
  twitter_data <- reactiveFileReader(
    session = NULL,
    intervalMillis = 100,
    filePath = 'data/twitter_data.rds',
    readFunc = readRDS
  )

  observe({
    print(input$tabs)
  })
   
  output$map_twitter <- renderLeaflet({
    
    leaflet() %>% setView(lng = 36.8219, lat = -1.2921, zoom = 11) %>% 
      addTiles() %>% 
      addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>%
      addSearchOSM(options = searchOptions(position = "topleft"))

  })
  
  output$map_matatu <- renderLeaflet({
    
    leaflet() %>% setView(lng = 36.8219, lat = -1.2921, zoom = 11) %>% 
      addTiles() %>% 
      addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>%
      addSearchOSM(options = searchOptions(position = "topleft"))
    
  })
  
  output$map_govt <- renderLeaflet({
    
    leaflet() %>% setView(lng = 36.8219, lat = -1.2921, zoom = 11) %>% 
      addTiles() %>% 
      addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>%
      addSearchOSM(options = searchOptions(position = "topleft"))
    
  })
  
  observe ({
    if(input$tabs == "twitter") {
      filtered_data <- twitter_data() %>%
        filter(time_day %in% input$hours_twitter) %>%
        filter(year %in% input$years_twitter)
      
      leafletProxy("map_twitter") %>% 
        clearGroup("markers") %>%
        addCircleMarkers(data = filtered_data, ~long, ~lat, 
                         clusterOptions = markerClusterOptions(), group = "markers")
    } 
    if(input$tabs == "matatu") {
      filtered_data <- binded_data %>%
        filter(year %in% input$years_matatu) %>%
        filter(passengers >= input$passengers[1] & passengers <= input$passengers[2]) %>%
        filter(injuries %in% input$injuries) %>%
        filter(source == "matatu")
      
      leafletProxy("map_matatu") %>% 
        clearGroup("markers") %>%
        addCircleMarkers(data = filtered_data, ~long, ~lat, 
                         clusterOptions = markerClusterOptions(), group = "markers")
    }
    if(input$tabs == "govt") {
      filtered_data <- binded_data %>%
        filter(source == "govt") %>%
        filter(year %in% input$years_govt)
      
      leafletProxy("map_govt") %>% 
        clearGroup("markers") %>%
        addCircleMarkers(data = filtered_data, ~long, ~lat, 
                         clusterOptions = markerClusterOptions(), group = "markers")
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

