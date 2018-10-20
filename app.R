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

#Function to load GeoJson
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

# Using ckanUniques to pull unique values of neighborhoods, types, and departments
neighborhoods <- sort(ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "NEIGHBORHOOD")$NEIGHBORHOOD)
types <- sort(ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "REQUEST_TYPE")$REQUEST_TYPE)
departments <- sort(ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "DEPARTMENT")$DEPARTMENT)

# Creating dashboard header
header <- dashboardHeader(title = "Pittsburgh 311")

# Creating dashboard sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    # Setting dashboard menu items / pages
    menuItem("Trends by Request Type", icon = icon("line-chart"), tabName = "type"),
    menuItem("Neighborhood Insight", icon = icon("group"), tabName = "neighborhood"),
    menuItem("Department Workflow", icon = icon("building-o"), tabName = "department"),
    # Adding reset button to reset ALL filters
    actionButton("reset", "Reset Filters", icon = icon("refresh"))
  )
)

# Creating dashboard body
body <- dashboardBody(tabItems(
  # Adding elements to the 'type' menu item
  tabItem("type",
          fluidRow(
            #adding a select input for request type to select across all tabBoxes
            selectInput("type_select",
                        "Request Type:",
                        choices = types,
                        multiple = FALSE,
                        selectize = TRUE,
                        selected = "Potholes"),
            tabBox(title = "What are the overall request trends?",
                   width = 12,
                   # creating new tab and adding plots
                   tabPanel("Visualizations by Type", 
                            plotlyOutput("type_plot"), 
                            br(),
                            br(),
                            plotlyOutput("status_plot")),
                   # creating new tab and adding map
                   tabPanel("Mapping Requests by Type", 
                            leafletOutput("type_map")),
                   # creating new tab and adding data
                   tabPanel("Neighborhood Report by Type", 
                            inputPanel(
                              downloadButton("download_typeReport", "Download the Report"),
                              downloadButton("download_typeRaw", "Download the Raw Data")
                            ),
                            DT::dataTableOutput("type_report")))
            )),
  # Adding elements to the 'neighborhood' menu item
  tabItem("neighborhood",
          fluidRow(
            #adding a select input for neighborhood to select across all tabBoxes
            selectInput("nbhd_select",
                               "Neighborhood:",
                               choices = neighborhoods,
                               multiple = FALSE,
                               selectize = TRUE,
                               selected = "Brookline"),
            tabBox(title = "What is happening in each neighborhood?",
                   width = 12,
                   # new tab and plots
                   tabPanel("Visualizations by Neighborhood",
                            plotlyOutput("nbhd_plot"),
                            br(),
                            br(),
                            plotlyOutput("nbhd_type_plot")),
                   # new tab and data
                   tabPanel("Request Type Report by Neighborhood",
                            inputPanel(
                              downloadButton("download_nbhdReport", "Download the Report"),
                              downloadButton("download_nbhdRaw", "Download the Raw Data")
                            ),
                            DT::dataTableOutput("nbhd_report")))
          )),
  # new menu item for departments
  tabItem("department",
          fluidRow(
            # select input for departments
            selectInput("dept_select",
                        "Department:",
                        choices = departments,
                        multiple = FALSE,
                        selectize = TRUE,
                        selected = "DPW - Forestry Division"),
            tabBox(title = "How is each city department performing?",
                   width = 12,
                   # new tab with plots
                   tabPanel("Visualizations by Department",
                            plotlyOutput("dept_plot"),
                            br(),
                            br(),
                            plotlyOutput("dept_type_plot")),
                   # new tab with data
                   tabPanel("Request Type Report by Department",
                            inputPanel(
                              downloadButton("download_deptReport", "Download the Report"),
                              downloadButton("download_deptRaw", "Download the Raw Data")
                            ),
                            DT::dataTableOutput("dept_report")))
          ))
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "purple", useShinyjs())

# Defining server logic
server <- function(input, output, session) {
  # Creating filtered type data
  typeFiltered <- reactive({
    # Build API Query with proper encodes
    # Building an IN selector
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22REQUEST_TYPE%22%20=%20%27", input$type_select, "%27")
    url <- gsub(pattern = " ", replacement = "%20", x = url)
    
    data <- ckanSQL(url)
    
    #loading and cleaning data
    data_YEAR <- data %>% separate(CREATED_ON, "YEAR", sep = '-') %>% subset(select = c(REQUEST_ID, YEAR))
    data_DATE <- data %>% mutate(DATE = as.Date(CREATED_ON), STATUS = ifelse(STATUS == 1, "Closed", "Open"))
    data <- merge(data_DATE, data_YEAR, key = "REQUEST_ID")
    data <- data %>% arrange(DATE)
      
    return(data) 
  })
  
  # creating data for the map to be used later
  mapFiltered <- reactive({
    # Build API Query with proper encodes
    # Building an IN selector
    url <- paste0("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/dbd133a206cc4a3aa915cb28baa60fd4_0.geojson")
    data <- ckanGeoSQL(url)
    return(data) 
  })
  
  # creating data for neighborhood analysis
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

  # creating data for department analysis
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
  
  # request type report
  type_report_Filtered <- reactive({
    dat <- typeFiltered()
    dat <- dat %>% group_by(REQUEST_TYPE, NEIGHBORHOOD, YEAR) %>%
      summarise(COUNT_BY_YEAR = n()) %>%
      group_by(REQUEST_TYPE, NEIGHBORHOOD) %>%
      mutate(NBHD_TOTAL = sum(COUNT_BY_YEAR))
    return(dat)
  })
  
  # neighborhood report
  nbhd_report_Filtered <- reactive({
    dat <- nbhdFiltered() 
    dat <- dat %>% group_by(NEIGHBORHOOD, REQUEST_TYPE, YEAR) %>%
      summarise(COUNT_BY_YEAR = n()) %>%
      group_by(NEIGHBORHOOD, REQUEST_TYPE) %>%
      mutate(REQUEST_TOTAL = sum(COUNT_BY_YEAR))
    return(dat)
  })
  
  # department report
  dept_report_Filtered <- reactive({
    dat <- deptFiltered()
    dat <- dat %>% group_by(DEPARTMENT, REQUEST_TYPE, YEAR) %>%
      summarise(COUNT_BY_YEAR = n()) %>%
      group_by(DEPARTMENT, REQUEST_TYPE) %>%
      mutate(DEPT_TOTAL = sum(COUNT_BY_YEAR))
    return(dat)
  })
  
  # Point and smooth graph for type menu item
  output$type_plot <- renderPlotly({
    dat <- typeFiltered() 
    dat <- dat %>% group_by(DATE) %>% summarise(COUNT = n())
    
    # Progress indicator taken from https://shiny.rstudio.com/articles/progress.html
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
        labs(title = "Trend Over Time", x = "Date", y = "Number of Requests")
      )
    })
  
   # Bar chart for type menu item
   output$status_plot <- renderPlotly({
     dat <- typeFiltered() 
     dat <- dat %>% group_by(STATUS) %>% summarise(COUNT = n())
     ggplotly(
       ggplot(data = dat, aes(x = STATUS, y = COUNT, fill = STATUS)) +
         geom_bar(stat = "identity") + 
         labs(title = "Status Overview", x = "Request Status", y = "Number of Requests", fill = "Status") + 
         theme_bw(),  
       tooltip = "text"
       )
     })
   
   # Leaflet map for type menu item
   output$type_map <- renderLeaflet({
     pal311 <- colorFactor(c("red", "green"), c("Closed", "Open"))
     map_data <- typeFiltered() %>% filter(CREATED_ON >= Sys.Date()-30 & CREATED_ON <= Sys.Date())
     
     closed <- map_data %>% filter(STATUS == "Closed") 
     open <- map_data %>% filter(STATUS == "Open")
     
     leaflet() %>%
       setView(lng = -79.9973317, lat = 40.4320679, zoom = 13) %>%
       # Basemaps
       addTiles(group = "OpenStreetMap.HOT") %>%
       # Adding polygons and awesome markers (utilizing cluster options)
       addPolygons(data = mapFiltered(), fill = FALSE) %>%
       addLegend(position = "bottomright" , pal = pal311, values = map_data$STATUS, title = "Status") %>%
       addAwesomeMarkers(data = closed, 
                         lng = ~X, lat = ~Y, 
                         icon = makeAwesomeIcon(icon = "check", library = "fa", markerColor = "red"), 
                         label = ~REQUEST_TYPE,
                         clusterOptions = markerClusterOptions()) %>%
       addAwesomeMarkers(data = open, 
                         lng = ~X, lat = ~Y, 
                         icon = makeAwesomeIcon(icon = "times", library = "fa", markerColor = "green"), 
                         label = ~REQUEST_TYPE,
                         clusterOptions = markerClusterOptions())
     
   })
   
   # output for type report
   output$type_report <- DT::renderDataTable({
     dat <- type_report_Filtered()
   })
   
   # plot for neighborhood menu item
   output$nbhd_plot <- renderPlotly({
     dat <- nbhdFiltered() 
     dat <- dat %>% group_by(DATE) %>% summarise(COUNT = n())
     
     ggplotly(
       ggplot(data = dat, aes(x = DATE, y = COUNT)) +
         geom_point(colour = "red") + geom_smooth() + 
         labs(title = "Trend Over Time", x = "Date", y = "Number of Requests")
     )
   })
   
   # plot for neighborhood menu item
   output$nbhd_type_plot <- renderPlotly({
     dat <- nbhdFiltered() 
     dat <- dat %>% group_by(REQUEST_TYPE) %>% summarise(COUNT = n()) %>% arrange(desc(COUNT)) %>% top_n(10)
     ggplotly(
       ggplot(data = dat, 
              aes(x = reorder(REQUEST_TYPE, -COUNT), y = COUNT, fill = REQUEST_TYPE, 
                              text = paste0("<b>", "<br> Request Type: ", REQUEST_TYPE, 
                                            "<b>", "<br> Count: ", COUNT))) +
         geom_bar(stat = "identity") + 
         labs(title = "Top 10 Request Types", x = "Request Type", y = "Number of Requests", fill = "Request Type") + 
         theme(axis.text.x = element_text(angle = 60, hjust = 1)), 
       tooltip = "text"
     )
   })
   
   # neighborhood report output
   output$nbhd_report <- DT::renderDataTable({
     dat <- nbhd_report_Filtered()
   })
   
   # plot for department menu item
   output$dept_plot <- renderPlotly({
     dat <- deptFiltered() 
     dat <- dat %>% group_by(DATE) %>% summarise(COUNT = n())
     
     ggplotly(
       ggplot(data = dat, aes(x = DATE, y = COUNT)) +
         geom_point(colour = "red") + geom_smooth() + 
         labs(title = "Trend Over Time", x = "Date", y = "Number of Requests") + theme_classic()
     )
   })
   
   # plot for department menu item
   output$dept_type_plot <- renderPlotly({
     dat <- deptFiltered() 
     dat <- dat %>% group_by(REQUEST_TYPE) %>% summarise(COUNT = n()) %>% arrange(desc(COUNT)) %>% top_n(10)
     ggplotly(
       ggplot(data = dat, aes(x = reorder(REQUEST_TYPE, -COUNT), y = COUNT, fill = REQUEST_TYPE, 
                              text = paste0("<b>", "<br> Request Type: ", REQUEST_TYPE, 
                                            "<b>", "<br> Count: ", COUNT))) +
         geom_bar(stat = "identity") + 
         labs(title = "Top 10 Request Types", x = "Request Type", y = "Number of Requests", fill = "Request Type") + 
         theme(axis.text.x = element_text(angle = 60, hjust = 1)),
       tooltip = "text"
     )
   })
   
   # department report
   output$dept_report <- DT::renderDataTable({
     dat <- dept_report_Filtered() 
   })
   
   
   # Downloading type report
   output$download_typeReport <- downloadHandler(
     filename = function() {
       paste("type-report-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.csv(type_report_Filtered(), file)
     }
   )
   
   # Downloading raw type data
   output$download_typeRaw <- downloadHandler(
     filename = function() {
       paste("type-raw-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.csv(typeFiltered(), file)
     }
   )
   
   # Downloading neighborhood report
   output$download_nbhdReport <- downloadHandler(
     filename = function() {
       paste("nbhd-report-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.csv(nbhd_report_Filtered(), file)
     }
   )
   
   # Downloading raw neighborhood data
   output$download_nbhdRaw <- downloadHandler(
     filename = function() {
       paste("nbhd-raw-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.csv(nbhdFiltered(), file)
     }
   )
   
   # Downloading department report
   output$download_deptReport <- downloadHandler(
     filename = function() {
       paste("dept-report-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.csv(dept_report_Filtered(), file)
     }
   )
   
   # Downloading raw department data
   output$download_deptRaw <- downloadHandler(
     filename = function() {
       paste("dept-raw-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.csv(deptFiltered(), file)
     }
   )
   
   # Reseting Filter Data
   observeEvent(input$reset, {
     updateSelectInput(session, "nbhd_select", selected = "Brookline")
     updateSelectInput(session, "type_select", selected = "Potholes")
     updateSelectInput(session, "dept_select",  selected = "DPW - Forestry Division")
     alert("You have reset the application!")
   })

}

# Running the application 
shinyApp(ui = ui, server = server)
     