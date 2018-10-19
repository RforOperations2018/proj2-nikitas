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
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(readxl)
library(raster)

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
sources <- sort(ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "REQUEST_ORIGIN")$REQUEST_ORIGIN)

#pdf(NULL)

# Define UI for application
ui <- fluidPage(
  # Add Shinyjs
  useShinyjs(),
  # Application title
  titlePanel("Pittsburgh 311 Tabset"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Creating a checkbox input for the 'request origin'
      selectInput("source_select",
                  "Source Type:",
                  multiple = TRUE,
                  choices = sources,
                  selected = "Call Center"),
      # Creating a date range input for the date variable
      dateRangeInput("date_select",
                     "Select Dates",
                     start = Sys.Date()-30,
                     end = Sys.Date()),
      # Creating a select input for the 'neighborhood' variable
      selectInput("nbhd_select",
                  "Neighborhood:",
                  choices = neighborhoods,
                  selectize = TRUE,
                  selected = "Brookline"),
      # Creating a Reset button
      actionButton("reset", "Reset Filters", icon = icon("refresh"))
    ),
    # Main panel consisting of 2 tabs: one for plots and another for the data table
    mainPanel(
      tabsetPanel(
        tabPanel("Plots", 
                 plotlyOutput("count_plot"), 
                 # adding space between plots
                 br(),
                 br(),
                 plotlyOutput("type_plot"), 
                 # adding space between plots
                 br(),
                 br(),
                 plotlyOutput("status_plot")
        ),
        tabPanel("Table",
                 inputPanel(
                   downloadButton("downloadData","Download Pittsburgh 311 Data")
                 ),
                 fluidPage(DT::dataTableOutput("table"))
        )
      )
    )
  )
)

# Defining server logic
server <- function(input, output, session) {
  # Creating filtered pitt 311 data
  pittFiltered <- reactive({
    # Build API Query with proper encodes
    # If no source_select input selected
    if (length(input$source_select) == 0 ) {
      url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22CREATED_ON%22%20%3E=%20%27", 
                    input$date_select[1], "%27%20AND%20%22CREATED_ON%22%20%3C=%20%27", input$date_select[2], 
                    "%27%20AND%20%22NEIGHBORHOOD%22%20=%20%27", input$nbhd_select, "%27")
      url <- gsub(pattern = " ", replacement = "%20", x = url)
      
    # If one source_select input selected  
    } else if (length(input$source_select) == 1) {
      url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22CREATED_ON%22%20%3E=%20%27", 
                    input$date_select[1], "%27%20AND%20%22CREATED_ON%22%20%3C=%20%27", input$date_select[2], 
                    "%27%20AND%20%22NEIGHBORHOOD%22%20=%20%27", input$nbhd_select, 
                    "%27%20AND%20%22REQUEST_ORIGIN%22%20=%20%27", input$source_select, "%27")
      url <- gsub(pattern = " ", replacement = "%20", x = url)
      
      # Multiple source_select inputs selected
    } else {
      primary_desc_q <- paste0(input$source_select, collapse = "%27%20OR%20%22REQUEST_ORIGIN%22%20=%20%27")
      url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22CREATED_ON%22%20%3E=%20%27", 
                    input$date_select[1], "%27%20AND%20%22CREATED_ON%22%20%3C=%20%27", input$date_select[2], 
                    "%27%20AND%20%22NEIGHBORHOOD%22%20=%20%27", input$nbhd_select, 
                    "%27%20AND%20%22REQUEST_ORIGIN%22%20=%20%27", primary_desc_q, "%27")
      url <- gsub(pattern = " ", replacement = "%20", x = url)}
    
    data <- ckanSQL(url)
    
    # Load and clean data
    if (is.null(data[1,1])){
      alert("There is no data available for your selected inputs. Please reset filters and select different inputs.")
      # Now, if you wanna be fancy! You could have put in an updateInputs or autmatically click your button with shinyjs()
    } else {
      data <- data %>%
        mutate(DATE = as.Date(CREATED_ON),
               STATUS = ifelse(STATUS == 1, "Closed", "Open"))
      
      return(data) 
    }
    
  })
  
   # Line graph showing count of all requests over time
  output$count_plot <- renderPlotly({
    dat <- pittFiltered() %>% group_by(DATE) %>% summarise(COUNT = n())
    ggplotly(
      ggplot(data = dat, aes(x = DATE, y = COUNT)) + 
        xlab("Date") + ylab("Count") +
        ggtitle("Request Count Over Time") +
        geom_point() + 
        geom_smooth()
      )
    })
  # Bar graph showing count of requests by type over time
  output$type_plot <- renderPlotly({
    dat <- pittFiltered() %>% group_by(REQUEST_TYPE, DATE) %>% summarise(COUNT = n())
    ggplotly(
      ggplot(data = dat, aes(x = DATE, y = COUNT, color = REQUEST_TYPE)) +
        xlab("Date") + ylab("Count") + 
        ggtitle("Requests by Type Over Time") +
        geom_line(stat = "identity")
      )
    })
   # Point chart showing count of requests by status
   output$status_plot <- renderPlotly({
     dat <- pittFiltered() %>% group_by(STATUS) %>% summarise(COUNT = n())
     ggplotly(
       ggplot(data = dat, aes(x = STATUS, y = COUNT, fill = STATUS)) +
         geom_bar(stat = "identity") + 
         xlab("Status") + ylab("Count") +
         ggtitle("Status of Requests")
       )
     })
     
   # Data Table with 311 data filters
   output$table <- DT::renderDataTable({
     pitt <- pittFiltered()
     subset(pitt, select = c(REQUEST_TYPE, DATE, REQUEST_ORIGIN, STATUS, DEPARTMENT, NEIGHBORHOOD))
   })
   # Updating the URL Bar
   observe({
     print(reactiveValuesToList(input))
     session$doBookmark()
   })
   onBookmarked(function(url) {
     updateQueryString(url)
   })
   # Downloading data in the datatable
   output$downloadData <- downloadHandler(
     filename = function() {
       paste("pitt-311-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.csv(pittFiltered(), file)
     }
)
    # Reseting Filter Data
   observeEvent(input$reset, {
     updateSelectInput(session, "nbhd_select", selected = "Brookline")
     updateSelectInput(session, "source_select",  selected = "Call Center")
     updateDateRangeInput(session, "date_select", start = Sys.Date()-30, end = Sys.Date())
     alert("You have reset the application!")
})
}

 # Running the application
 shinyApp(ui = ui, server = server, enableBookmarking = "url")