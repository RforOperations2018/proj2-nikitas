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
types <- sort(ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "REQUEST_TYPE")$REQUEST_TYPE)
dates <- sort(ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "CREATED_ON")$CREATED_ON)
dates <- as.data.frame(dates)
years <- separate(dates, dates, "YEAR", '-')
years <- unique(years$YEAR)

#pdf(NULL)

# Creating dashboard header
header <- dashboardHeader(title = "Pittsburgh 311 Dashboard")

# Creating dashboard sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    # Setting dashboard menu items / pages
    menuItem("Trends by Request Type", icon = icon("hand-o-up"), tabName = "type"),
    menuItem("Neighborhood Report", icon = icon("group"), tabName = "neighborhood"),
    menuItem("Origin Analysis", icon = icon("mobile"), tabName = "origin"),
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
          # fluidRow(
            # # info boxes to appear at the top of the prescription page
            # infoBoxOutput("avg_presc"),
            # infoBoxOutput("avg_age")
          # ),
          fluidRow(
            tabBox(title = "What are the overall request trends?",
                   width = 12,
                   # adding local inputs and corresponding plots in multiple tabs
                   # creating new tab
                   tabPanel("By Year", 
                            selectInput("year_select",
                                        "Request Year:",
                                        choices = years,
                                        multiple = TRUE,
                                        selectize = TRUE,
                                        selected = c("2015", "2016", "2017")),
                            plotlyOutput("count_plot")),
                   # creating new tab
                   tabPanel("By Request", 
                            selectInput("type_select",
                                        "Request Type:",
                                        choices = types,
                                        multiple = FALSE,
                                        selectize = TRUE,
                                        selected = c("Pothole")),
                            plotlyOutput("type_plot"), 
                            br(),
                            br(),
                            plotlyOutput("status_plot")))
          )
  )#,
  # Adding elements to the 'service' menu item
  # tabItem("neighborhood",
  #         # adding info boxes at the top of the service page
  #         # fluidRow(
  #         #   infoBoxOutput("avg_mh"),
  #         #   infoBoxOutput("avg_da")
  #         # ),
  #         # adding local input and plot
  #         fluidRow(
  #           box(title = "Does service usage change relative to prescription service usage?",
  #               width = 12,
  #               selectInput(
  #                 "yAxis_select",
  #                 "Select column for Y axis",
  #                 choices = colnames(merged[c(33, 34, 38)]),
  #                 selected = colnames(merged[33])),
  #               mainPanel(width = 12,
  #                         plotlyOutput("plot_service"))
  #           )
  #         )
  # ),
  # # Adding elements to the 'cjs' menu item
  # tabItem("origin",
  #         fluidRow(
  #           # adding info boxes at the top of the page
  #           infoBoxOutput("avg_jail"),
  #           infoBoxOutput("avg_charge")
  #         ),
  #         # adding plot
  #         fluidRow(
  #           box(title = "Is there any association between prescription usage and interaction with the criminal justice system?",
  #               width = 12,
  #               mainPanel(width = 12,
  #                         plotlyOutput("plot_jail"))
  #           )
  #         )
  # ),
  # # Adding elements to the 'data' menu item
  # tabItem("data",
  #         fluidRow(
  #           box(title = "Use this Data Table to find interesting insights of your own!",
  #               width = 12,
  #               # includes download button to get a .csv of the data
  #               inputPanel(
  #                 downloadButton("downloadData","Download Pittsburgh 311 Request Data")),
  #               mainPanel(width = 12,
  #                         DT::dataTableOutput("table")))
  #         )
  )
)
#)

ui <- dashboardPage(header, sidebar, body, skin = "green", useShinyjs())

# Defining server logic
server <- function(input, output, session) {
  # Creating filtered pitt 311 data
  pittFiltered <- reactive({
    # Build API Query with proper encodes
    # Building an IN selector
    types_filter <- ifelse(length(input$type_select) > 0, 
                           paste0("%20AND%20%22REQUEST_TYPE%22%20IN%20(%27", paste(input$type_select, collapse = "%27,%27"),"%27)"),"")
    source_filter <- ifelse(length(input$source_select) > 0,
                            paste0("%20AND%20%22REQUEST_ORIGIN%22%20IN%20(%27", paste(input$source_select, collapse = "%27,%27"),"%27)"),"")
    nbhd_filter <- ifelse(length(input$source_select) > 0,
                            paste0("%20AND%20%22NEIGHBORHOOD%22%20IN%20(%27", paste(input$source_select, collapse = "%27,%27"),"%27)"),"")
    
      url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22NEIGHBORHOOD%22%20=%20%27", input$nbhd_select, 
                    "%27%20AND%20%22REQUEST_ORIGIN%22%20=%20%27", input$source_select, 
                    "%27%20AND%20%22REQUEST_TYPE%22%20=%20%27", input$type_select, "%27")
      url <- gsub(pattern = " ", replacement = "%20", x = url)
      
    data <- ckanSQL(url)
    
    # Load and clean data
    if (is.null(data[1,1])){
        # updateSelectInput(session, "nbhd_select", selected = "Brookline")
        # updateSelectInput(session, "source_select",  selected = "Call Center")
        # updateDateRangeInput(session, "date_select", start = Sys.Date()-30, end = Sys.Date())
        alert("There is no data available for your selected inputs. Your filters have been reset. Please try again.")
    } else {
      data_YEAR <- data %>% separate(CREATED_ON, "YEAR", sep = '-') %>% subset(data, select = c(REQUEST_ID, YEAR))
      data_DATE <- data %>%
        mutate(DATE = as.Date(CREATED_ON),
               STATUS = ifelse(STATUS == 1, "Closed", "Open"))
      data <- merge(data_DATE, data_YEAR, key = "REQUEST_ID")
      data <- data %>% arrange(DATE)
      return(data) 
    }
    
  })
  
   # Line graph showing count of all requests over time
  output$count_plot <- renderPlotly({
    dat <- pittFiltered() %>% group_by(YEAR) %>% summarise(COUNT = n())
    ggplotly(
      ggplot(data = dat, aes(x = YEAR, y = COUNT)) + 
        geom_histogram(stat = "identity") + 
        labs(title = "Request Count by Year", x = "Year", y = "Number of Requests") + 
        theme_bw()
      )
    })
  # Point and smooth graph showing count of requests by type over time
  output$type_plot <- renderPlotly({
    dat <- pittFiltered() %>% group_by(REQUEST_TYPE, DATE) %>% summarise(COUNT = n())
    ggplotly(
      ggplot(data = dat, aes(x = DATE, y = COUNT, color = REQUEST_TYPE)) +
        geom_point() + geom_smooth() + 
        labs(title = "Request Count Trend Over Time", x = "Date", y = "Number of Requests") + 
        theme_bw()
      )
    })
   # Bar chart showing count by status for each request
   output$status_plot <- renderPlotly({
     dat <- pittFiltered() %>% group_by(REQUEST_TYPE, STATUS) %>% summarise(COUNT = n())
     ggplotly(
       ggplot(data = dat, aes(x = STATUS, y = COUNT, fill = REQUEST_TYPE)) +
         geom_bar(stat = "identity") + 
         labs(title = "Request Status Overview", x = "Request Status", y = "Number of Requests") + 
         theme_bw()
       )
     })

   # # Adding table output (count_data)
   # output$table <- DT::renderDataTable(count_data,
   #                                     options = list(scrollX = TRUE),
   #                                     rownames = FALSE)
   # # Adding downloadData output
   # output$downloadData <- downloadHandler(
   #   filename = function() {
   #     paste("presc-summary-data-", Sys.Date(), ".csv", sep="")
   #   },
   #   content = function(file) {
   #     write.csv(count_data, file, row.names = FALSE)
   #   }
   # )
   # # Resetting all filters (global and local)
   # observeEvent(input$reset, {
   #   updateSelectInput(session, "race_select", selected = c("White", "Black/African-American", "Other"))
   #   updateSelectInput(session, "gender_select", selected = c("Female", "Male", "Other"))
   #   updateSelectInput(session, "cohort_select",  selected = c("2010", "2011", "2012", "2013", "2014"))
   #   updateSelectInput(session, "od_status_select", selected = c("No Overdose", "Non-Opiate Overdose", "Opiate Overdose"))
   #   updateSelectInput(session, "drug_form_select", selected = c("PILL", "PATCH"))
   #   updateSelectInput(session, "yAxis_select", selected = colnames(merged[33]))
   #   alert("You have reset the dashboard filters!")
   # })
}

# Running the application 
shinyApp(ui = ui, server = server)
     
#    # Data Table with 311 data filters
#    output$table <- DT::renderDataTable({
#      pitt <- pittFiltered()
#      subset(pitt, select = c(REQUEST_TYPE, DATE, REQUEST_ORIGIN, STATUS, DEPARTMENT, NEIGHBORHOOD))
#    })
#    # Updating the URL Bar
#    observe({
#      print(reactiveValuesToList(input))
#      session$doBookmark()
#    })
#    onBookmarked(function(url) {
#      updateQueryString(url)
#    })
#    # Downloading data in the datatable
#    output$downloadData <- downloadHandler(
#      filename = function() {
#        paste("pitt-311-", Sys.Date(), ".csv", sep="")
#      },
#      content = function(file) {
#        write.csv(pittFiltered(), file)
#      }
# )
#     # Reseting Filter Data
#    observeEvent(input$reset, {
#      updateSelectInput(session, "nbhd_select", selected = "Brookline")
#      updateSelectInput(session, "source_select",  selected = "Call Center")
#      updateDateRangeInput(session, "date_select", start = Sys.Date()-30, end = Sys.Date())
#      alert("You have reset the application!")
# })
# }
# 
#  # Running the application
#  shinyApp(ui = ui, server = server, enableBookmarking = "url")