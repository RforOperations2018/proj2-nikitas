library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(shinyjs)
library(httr)
library(jsonlite)
library(tidyr)

pitt <- read.csv("311-pitt.csv", sep = ",", stringsAsFactors = FALSE)

dates <- unique(pitt$CREATED_ON)
dates <- as.data.frame(dates)
years <- separate(dates, dates, "year", '-')
years <- unique(years$year)

types_filter <- ifelse(length(input$type_select) > 0, 
                       paste0("%20AND%20%22REQUEST_TYPE%22%20IN%20(%27", paste(input$type_select, collapse = "%27,%27"),"%27)"),"")


nbhd_filter <- ifelse(length(input$source_select) > 0,
                      paste0("%20AND%20%22NEIGHBORHOOD%22%20IN%20(%27", paste(input$source_select, collapse = "%27,%27"),"%27)"),"")


types_filter <- ifelse(length(input$type_select) > 0, 
                       paste0("%20AND%20%22REQUEST_TYPE%22%20IN%20(%27", paste(input$type_select, collapse = "%27,%27"),"%27)"),"")
# Line graph showing count of all requests over time
output$count_plot <- renderPlotly({
  dat <- typeFiltered() %>% group_by(YEAR) %>% summarise(COUNT = n())
  ggplotly(
    ggplot(data = dat, aes(x = YEAR, y = COUNT)) + 
      geom_histogram(stat = "identity") + 
      labs(title = "Request Count by Year", x = "Year", y = "Number of Requests") + 
      theme_bw()
  )})

tabItem("neighborhood",
        fluidRow(
          tabBox(title = "What is happening in each neighborhood?",
                 width = 12,
                 tabPanel("Visualizations by Neighborhood",
                          selectInput("nbhd_select",
                                      "Neighborhood:", 
                                      choices = neighborhoods, 
                                      multiple = FALSE, 
                                      selectize = TRUE,
                                      selected = "Brookline"), 
                          plotlyOutput("nbhd_plot"),
                          br(),
                          br(),
                          plotlyOutput("nbhd_type_plot")),
                 tabPanel("Report by"))
        ))

output$nbhd_plot <- renderPlotly({
  dat <- nbhdFiltered() %>% group_by(NEIGHBORHOOD, DATE) %>% summarise(COUNT = n())
  ggplotly(
    ggplot(data = dat, aes(x = DATE, y = COUNT, color = NEIGHBORHOOD)) +
      geom_point() + geom_smooth() + 
      labs(title = "Neighborhood: Trend Over Time", x = "Date", y = "Number of Requests") + 
      theme_bw()
  )
})

output$nbhd_type_plot <- renderPlotly({
  dat <- nbhdFiltered() %>% group_by(REQUEST_TYPE) %>% summarise(COUNT = n())
  ggplotly(
    ggplot(data = dat, aes(x = REQUEST_TYPE, y = COUNT, color = REQUEST_TYPE)) +
      geom_bar() + 
      labs(title = "Neighborhood: Trend Over Time", x = "Date", y = "Number of Requests") + 
      theme_bw()
  )
})




# Creating dashboard body
body <- dashboardBody(tabItems(
  # Adding elements to the 'neighborhood' menu item
  tabItem("type",
          fluidRow(
            tabBox(title = "What are the overall request trends?",
                   width = 12,
                   # adding local inputs and corresponding plots in multiple tabs
                   # creating new tab
                   tabPanel("Visualizations by Type", 
                            selectInput("type_select",
                                        "Request Type:",
                                        choices = types,
                                        multiple = FALSE,
                                        selectize = TRUE,
                                        selected = c("Potholes")),
                            plotlyOutput("type_plot"), 
                            br(),
                            br(),
                            plotlyOutput("status_plot")),
                   tabPanel("Neighborhood Report by Type", 
                            DT::dataTableOutput("type_report")
                   )))
            #fluidRow(
            #           box(title = "Use this Data Table to find interesting insights of your own!",
            #               width = 12,
            #               # includes download button to get a .csv of the data
            #               inputPanel(
            #                 downloadButton("downloadData","Download Pittsburgh 311 Request Data")),
            #               mainPanel(width = 12,
            #                         DT::dataTableOutput("table")))
            #         )
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
  typeFiltered <- reactive({
    # Build API Query with proper encodes
    # Building an IN selector
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22REQUEST_TYPE%22%20=%20%27", input$type_select, "%27")
    url <- gsub(pattern = " ", replacement = "%20", x = url)
    
    data <- ckanSQL(url)
    
    # Load and clean data
    if (is.null(data[1,1])){
      updateSelectInput(session, "type_select", selected = "Potholes")
      alert("There is no data available for your selected input(s). Your filters have been reset. Please try again.")
    } else {
      data_YEAR <- data %>% separate(CREATED_ON, "YEAR", sep = '-') %>% subset(select = c(REQUEST_ID, YEAR))
      data_DATE <- data %>%
        mutate(DATE = as.Date(CREATED_ON),
               STATUS = ifelse(STATUS == 1, "Closed", "Open"))
      data <- merge(data_DATE, data_YEAR, key = "REQUEST_ID")
      data <- data %>% arrange(DATE)
      return(data) 
    }
    
  })
  
  # Creating filtered pitt 311 data
  nbhdFiltered <- reactive({
    # Build API Query with proper encodes
    # Building an IN selector
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22NEIGHBORHOOD%22%20=%20%27", input$nbhd_select, "%27")
    url <- gsub(pattern = " ", replacement = "%20", x = url)
    
    data <- ckanSQL(url)
    
    # Load and clean data
    if (is.null(data[1,1])){
      updateSelectInput(session, "nbhd_select", selected = "Brookline")
      alert("There is no data available for your selected input(s). Your filters have been reset. Please try again.")
    } else {
      data_YEAR <- data %>% separate(CREATED_ON, "YEAR", sep = '-') %>% subset(select = c(REQUEST_ID, YEAR))
      data_DATE <- data %>%
        mutate(DATE = as.Date(CREATED_ON),
               STATUS = ifelse(STATUS == 1, "Closed", "Open"))
      data <- merge(data_DATE, data_YEAR, key = "REQUEST_ID")
      data <- data %>% arrange(DATE)
      return(data) 
    }
    
  })
  # 
  # deptFiltered <- reactive({
  #   # Build API Query with proper encodes
  #   # Building an IN selector  
  #   url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22DEPARTMENT%22%20=%20%27", input$dept_select, "%27")
  #   url <- gsub(pattern = " ", replacement = "%20", x = url)
  #   
  #   data <- ckanSQL(url)
  #   
  #   # Load and clean data
  #   if (is.null(data[1,1])){
  #     updateSelectInput(session, "dept_select", selected = "DPW")
  #     alert("There is no data available for your selected input(s). Your filters have been reset. Please try again.")
  #   } else {
  #     data_YEAR <- data %>% separate(CREATED_ON, "YEAR", sep = '-') %>% subset(data, select = c(REQUEST_ID, YEAR))
  #     data_DATE <- data %>%
  #       mutate(DATE = as.Date(CREATED_ON),
  #              STATUS = ifelse(STATUS == 1, "Closed", "Open"))
  #     data <- merge(data_DATE, data_YEAR, key = "REQUEST_ID")
  #     data <- data %>% arrange(DATE)
  #     return(data) 
  #   }
  #   
  # })
  
  # Line graph showing count of all requests over time
  output$count_plot <- renderPlotly({
    dat <- typeFiltered() %>% group_by(YEAR) %>% summarise(COUNT = n())
    ggplotly(
      ggplot(data = dat, aes(x = YEAR, y = COUNT)) + 
        geom_histogram(stat = "identity") + 
        labs(title = "Request Count by Year", x = "Year", y = "Number of Requests") + 
        theme_bw()
    )
  })
  # Point and smooth graph showing count of requests by type over time
  output$type_plot <- renderPlotly({
    dat <- typeFiltered() %>% group_by(REQUEST_TYPE, DATE) %>% summarise(COUNT = n())
    ggplotly(
      ggplot(data = dat, aes(x = DATE, y = COUNT, color = REQUEST_TYPE)) +
        geom_point() + geom_smooth() + 
        labs(title = "Request Count Trend Over Time", x = "Date", y = "Number of Requests") + 
        theme_bw()
    )
  })
  # Bar chart showing count by status for each request
  output$status_plot <- renderPlotly({
    dat <- typeFiltered() %>% group_by(REQUEST_TYPE, STATUS) %>% summarise(COUNT = n())
    ggplotly(
      ggplot(data = dat, aes(x = STATUS, y = COUNT, fill = REQUEST_TYPE)) +
        geom_bar(stat = "identity") + 
        labs(title = "Request Status Overview", x = "Request Status", y = "Number of Requests") + 
        theme_bw()
    )
  })
  
  output$type_report <- DT::renderDataTable({
    neigh_year <- typeFiltered() %>%
      group_by(NEIGHBORHOOD, YEAR) %>%
      summarise(COUNT_BY_YEAR = n()) %>%
      group_by(NEIGHBORHOOD) %>%
      mutate(NBHD_TOTAL = sum(count))
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
