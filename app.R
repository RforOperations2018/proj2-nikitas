#Project 2 - Nikita Setia

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(shinyjs)
library(httr)
library(jsonlite)
library(ggplot2)
library(shinydashboard)
library(reshape)
library(leaflet)
library(leaflet.extras)
library(raster)
library(rgdal)
library(tidyr)

#Function to bring in GeoJson
ckanGeoSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  readOGR(json)
}

# Function to load data from API
ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

neighborhoods <- sort(ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "NEIGHBORHOOD")$NEIGHBORHOOD)
types <- sort(ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "REQUEST_TYPE")$REQUEST_TYPE)
departments <- sort(ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "DEPARTMENT")$DEPARTMENT)

#pdf(NULL)

# Creating dashboard header
header <- dashboardHeader(title = "Pittsburgh 311 Dashboard")

# Creating dashboard sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    # Setting dashboard menu items / pages
    menuItem("Trends by Request Type", icon = icon("hand-o-up"), tabName = "type"),
    menuItem("Neighborhood Insight", icon = icon("group"), tabName = "neighborhood"),
    menuItem("Department Workflow", icon = icon("building-o"), tabName = "department"),
    menuItem("Data Exploration", icon = icon("table"), tabName = "data"),
    # no global inputs needed
    # Adding reset button to reset ALL filters
    actionButton("reset", "Reset Filters", icon = icon("refresh"))
  )
)

# Creating dashboard body
body <- dashboardBody(tabItems(
  # Adding elements to the 'neighborhood' menu item
  tabItem("type",
          fluidRow(
            selectInput("type_select",
                        "Request Type:",
                        choices = types,
                        multiple = FALSE,
                        selectize = TRUE,
                        selected = "Potholes"),
            tabBox(title = "What are the overall request trends?",
                   width = 12,
                   # adding local inputs and corresponding plots in multiple tabs
                   # creating new tab
                   tabPanel("Visualizations by Type", 
                            plotlyOutput("type_plot"), 
                            br(),
                            br(),
                            plotlyOutput("status_plot")),
                   tabPanel("Mapping Requests by Type", 
                            leafletOutput("type_map")),
                   tabPanel("Neighborhood Report by Type", 
                            DT::dataTableOutput("type_report")))
            )),
  tabItem("neighborhood",
          fluidRow(
            selectInput("nbhd_select",
                               "Neighborhood:",
                               choices = neighborhoods,
                               multiple = FALSE,
                               selectize = TRUE,
                               selected = "Brookline"),
            tabBox(title = "What is happening in each neighborhood?",
                   width = 12,
                   tabPanel("Visualizations by Neighborhood",
                            plotlyOutput("nbhd_plot"),
                            br(),
                            br(),
                            plotlyOutput("nbhd_type_plot")),
                   tabPanel("Request Type Report by Neighborhood",
                            DT::dataTableOutput("nbhd_report")))
          )),
  tabItem("department",
          fluidRow(
            selectInput("dept_select",
                        "Department:",
                        choices = departments,
                        multiple = FALSE,
                        selectize = TRUE,
                        selected = "DPW - Forestry Division"),
            tabBox(title = "How is each city department performing?",
                   width = 12,
                   tabPanel("Visualizations by Department",
                            plotlyOutput("dept_plot"),
                            br(),
                            br(),
                            plotlyOutput("dept_type_plot")),
                   tabPanel("Request Type Report by Department",
                            DT::dataTableOutput("dept_report")))
          ))
  )
)


ui <- dashboardPage(header, sidebar, body, skin = "purple", useShinyjs())

# Defining server logic
server <- function(input, output, session) {
  # Creating filtered pitt 311 data
  typeFiltered <- reactive({
    # Build API Query with proper encodes
    # Building an IN selector
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22REQUEST_TYPE%22%20=%20%27", input$type_select, "%27")
    url <- gsub(pattern = " ", replacement = "%20", x = url)
    
    data <- ckanSQL(url)
    
    data_YEAR <- data %>% separate(CREATED_ON, "YEAR", sep = '-') %>% subset(select = c(REQUEST_ID, YEAR))
    data_DATE <- data %>% mutate(DATE = as.Date(CREATED_ON), STATUS = ifelse(STATUS == 1, "Closed", "Open"))
    data <- merge(data_DATE, data_YEAR, key = "REQUEST_ID")
    data <- data %>% arrange(DATE)
      
    return(data) 
  })
  
  mapFiltered <- reactive({
    # Build API Query with proper encodes
    # Building an IN selector
    url <- paste0("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/dbd133a206cc4a3aa915cb28baa60fd4_0.geojson")
    data <- ckanGeoSQL(url)
    return(data) 
  })
  
  nbhdFiltered <- reactive({
    # Build API Query with proper encodes
    # Building an IN selector
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22NEIGHBORHOOD%22%20=%20%27", input$nbhd_select, "%27")
    url <- gsub(pattern = " ", replacement = "%20", x = url)
    
    data <- ckanSQL(url)
    
    # Load and clean data
    data_YEAR <- data %>% separate(CREATED_ON, "YEAR", sep = '-') %>% subset(select = c(REQUEST_ID, YEAR))
    data_DATE <- data %>% mutate(DATE = as.Date(CREATED_ON), STATUS = ifelse(STATUS == 1, "Closed", "Open"))
    data <- merge(data_DATE, data_YEAR, key = "REQUEST_ID")
    data <- data %>% arrange(DATE)
    return(data) 
  })

  deptFiltered <- reactive({
    # Build API Query with proper encodes
    # Building an IN selector
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22DEPARTMENT%22%20=%20%27", input$dept_select, "%27")
    url <- gsub(pattern = " ", replacement = "%20", x = url)

    data <- ckanSQL(url)

    # Load and clean data
    data_YEAR <- data %>% separate(CREATED_ON, "YEAR", sep = '-') %>% subset(select = c(REQUEST_ID, YEAR))
    data_DATE <- data %>% mutate(DATE = as.Date(CREATED_ON), STATUS = ifelse(STATUS == 1, "Closed", "Open"))
    data <- merge(data_DATE, data_YEAR, key = "REQUEST_ID")
    data <- data %>% arrange(DATE)
    return(data)
  })
  
  # Point and smooth graph showing count of requests by type over time
  output$type_plot <- renderPlotly({
    dat <- typeFiltered() 
    dat <- dat %>% group_by(DATE) %>% summarise(COUNT = n())
    
    withProgress(message = 'Making plot', value = 0, {
      n <- 10
      for (i in 1:n) {
        incProgress(1/n, detail = paste("Doing part", i))
        Sys.sleep(0.1)
      }
    })
    ggplotly(
      ggplot(data = dat, aes(x = DATE, y = COUNT)) +
        geom_point(colour = "red") + geom_smooth() + 
        labs(title = "Trend Over Time", x = "Date", y = "Number of Requests") + 
       theme_classic()
      )
    })
   # Bar chart showing count by status for each request
   output$status_plot <- renderPlotly({
     dat <- typeFiltered() 
     dat <- dat %>% group_by(STATUS) %>% summarise(COUNT = n())
     ggplotly(
       ggplot(data = dat, aes(x = STATUS, y = COUNT, fill = STATUS)) +
         geom_bar(stat = "identity") + 
         labs(title = "Status Overview", x = "Request Status", y = "Number of Requests") + 
         theme_bw()
       )
     })
   
   output$type_map <- renderLeaflet({
     pal311 <- colorFactor(c("red", "green"), c("Closed", "Open"))
     map_data <- typeFiltered() %>% filter(CREATED_ON >= Sys.Date()-30 & CREATED_ON <= Sys.Date())
     closed <- map_data %>% filter(STATUS == "Closed") 
     open <- map_data %>% filter(STATUS == "Open")
     
     withProgress(message = 'Making map', value = 0, {
       n <- 10
       for (i in 1:n) {
         incProgress(1/n, detail = paste("Doing part", i))
         Sys.sleep(0.1)
       }
     })
     
     leaflet() %>%
       setView(lng = -79.9973317, lat = 40.4320679, zoom = 13) %>%
       # Basemaps
       addTiles(group = "OpenStreetMap.HOT") %>%
       # Adding polylines, polygons, points and legend
       addPolygons(data = mapFiltered(), fill = FALSE) %>%
       #addCircleMarkers(data = map_data, lng = ~X, lat = ~Y, radius = 1.2, color = ~pal311(STATUS)) %>%
       addLegend(position = "bottomright" , pal = pal311, values = map_data$STATUS, title = "Status") %>%
       addAwesomeMarkers(data = closed, 
                         lng = ~X, lat = ~Y, 
                         icon = makeAwesomeIcon(icon = "check", library = "fa", markerColor = "red"), popup = ~NEIGHBORHOOD) %>%
       addAwesomeMarkers(data = open, 
                         lng = ~X, lat = ~Y, 
                         icon = makeAwesomeIcon(icon = "times", library = "fa", markerColor = "green"), popup = ~NEIGHBORHOOD)
   })
   
   output$type_report <- DT::renderDataTable({
     neigh_year <- typeFiltered() 
     neigh_year %>% group_by(REQUEST_TYPE, NEIGHBORHOOD, YEAR) %>%
       summarise(COUNT_BY_YEAR = n()) %>%
       group_by(REQUEST_TYPE, NEIGHBORHOOD) %>%
       mutate(NBHD_TOTAL = sum(COUNT_BY_YEAR))
   })
   
   output$nbhd_plot <- renderPlotly({
     dat <- nbhdFiltered() 
     dat <- dat %>% group_by(DATE) %>% summarise(COUNT = n())
     
     withProgress(message = 'Making plot', value = 0, {
       n <- 10
       for (i in 1:n) {
         incProgress(1/n, detail = paste("Doing part", i))
         Sys.sleep(0.1)
       }
     })
     
     ggplotly(
       ggplot(data = dat, aes(x = DATE, y = COUNT)) +
         geom_point(colour = "red") + geom_smooth() + 
         labs(title = "Trend Over Time", x = "Date", y = "Number of Requests") + theme_classic()
     )
   })
   
   output$nbhd_type_plot <- renderPlotly({
     dat <- nbhdFiltered() 
     dat <- dat %>% group_by(REQUEST_TYPE) %>% summarise(COUNT = n()) %>% arrange(desc(COUNT)) %>% top_n(10)
     ggplotly(
       ggplot(data = dat, aes(x = reorder(REQUEST_TYPE, -COUNT), y = COUNT, fill = REQUEST_TYPE)) +
         geom_bar(stat = "identity") + 
         labs(title = "Top 10 Request Types", x = "Request Type", y = "Number of Requests") + 
         theme_bw()
     )
   })
   
   output$nbhd_report <- DT::renderDataTable({
     type_year <- nbhdFiltered() 
     type_year %>% group_by(NEIGHBORHOOD, REQUEST_TYPE, YEAR) %>%
       summarise(COUNT_BY_YEAR = n()) %>%
       group_by(NEIGHBORHOOD, REQUEST_TYPE) %>%
       mutate(REQUEST_TOTAL = sum(COUNT_BY_YEAR))
   })
   
   output$dept_plot <- renderPlotly({
     dat <- deptFiltered() 
     dat <- dat %>% group_by(DATE) %>% summarise(COUNT = n())
     
     withProgress(message = 'Making plot', value = 0, {
       n <- 10
       for (i in 1:n) {
         incProgress(1/n, detail = paste("Doing part", i))
         Sys.sleep(0.1)
       }
     })
     
     ggplotly(
       ggplot(data = dat, aes(x = DATE, y = COUNT)) +
         geom_point(colour = "red") + geom_smooth() + 
         labs(title = "Trend Over Time", x = "Date", y = "Number of Requests") + theme_classic()
     )
   })
   
   output$dept_type_plot <- renderPlotly({
     dat <- deptFiltered() 
     dat <- dat %>% group_by(REQUEST_TYPE) %>% summarise(COUNT = n()) %>% arrange(desc(COUNT)) %>% top_n(10)
     ggplotly(
       ggplot(data = dat, aes(x = reorder(REQUEST_TYPE, -COUNT), y = COUNT, fill = REQUEST_TYPE)) +
         geom_bar(stat = "identity") + 
         labs(title = "Top 10 Request Types", x = "Request Type", y = "Number of Requests") + 
         theme_bw()
     )
   })
   
   output$dept_report <- DT::renderDataTable({
     type_year <- deptFiltered() 
     type_year %>% group_by(DEPARTMENT, REQUEST_TYPE, YEAR) %>%
       summarise(COUNT_BY_YEAR = n()) %>%
       group_by(DEPARTMENT, REQUEST_TYPE) %>%
       mutate(REQUEST_TOTAL = sum(COUNT_BY_YEAR))
   })

}

# Running the application 
shinyApp(ui = ui, server = server)
     