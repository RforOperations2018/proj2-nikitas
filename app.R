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
  ),
  # Adding elements to the 'service' menu item
  tabItem("neighborhood",
          # adding info boxes at the top of the service page
          # fluidRow(
          #   infoBoxOutput("avg_mh"),
          #   infoBoxOutput("avg_da")
          # ),
          # adding local input and plot
          fluidRow(
            box(title = "Does service usage change relative to prescription service usage?",
                width = 12,
                selectInput(
                  "yAxis_select",
                  "Select column for Y axis",
                  choices = colnames(merged[c(33, 34, 38)]),
                  selected = colnames(merged[33])),
                mainPanel(width = 12,
                          plotlyOutput("plot_service"))
            )
          )
  ),
  # Adding elements to the 'cjs' menu item
  tabItem("origin",
          fluidRow(
            # adding info boxes at the top of the page
            infoBoxOutput("avg_jail"),
            infoBoxOutput("avg_charge")
          ),
          # adding plot
          fluidRow(
            box(title = "Is there any association between prescription usage and interaction with the criminal justice system?",
                width = 12,
                mainPanel(width = 12,
                          plotlyOutput("plot_jail"))
            )
          )
  ),
  # Adding elements to the 'data' menu item
  tabItem("data",
          fluidRow(
            box(title = "Use this Data Table to find interesting insights of your own!",
                width = 12,
                # includes download button to get a .csv of the data
                inputPanel(
                  downloadButton("downloadData","Download Pittsburgh 311 Request Data")),
                mainPanel(width = 12,
                          DT::dataTableOutput("table")))
          )
  )
)
)

ui <- dashboardPage(header, sidebar, body, skin = "green", useShinyjs())


# Define UI for application
# ui <- fluidPage(
#   # Add Shinyjs
#   useShinyjs(),
#   # Application title
#   titlePanel("Pittsburgh 311 Tabset"),
#   
#   # Sidebar
#   sidebarLayout(
#     sidebarPanel(
#       # Creating a checkbox input for the 'request origin'
#       selectInput("source_select",
#                   "Source Type:",
#                   multiple = TRUE,
#                   choices = sources,
#                   selected = "Call Center"),
#       # Creating a date range input for the date variable
#       dateRangeInput("date_select",
#                      "Select Dates",
#                      start = Sys.Date()-30,
#                      end = Sys.Date()),
#       # Creating a select input for the 'neighborhood' variable
#       selectInput("nbhd_select",
#                   "Neighborhood:",
#                   choices = neighborhoods,
#                   selectize = TRUE,
#                   selected = "Brookline"),
#       # Creating a Reset button
#       actionButton("reset", "Reset Filters", icon = icon("refresh"))
#     ),
#     # Main panel consisting of 2 tabs: one for plots and another for the data table
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Plots", 
#                  plotlyOutput("count_plot"), 
#                  # adding space between plots
#                  br(),
#                  br(),
#                  plotlyOutput("type_plot"), 
#                  # adding space between plots
#                  br(),
#                  br(),
#                  plotlyOutput("status_plot")
#         ),
#         tabPanel("Table",
#                  inputPanel(
#                    downloadButton("downloadData","Download Pittsburgh 311 Data")
#                  ),
#                  fluidPage(DT::dataTableOutput("table"))
#         )
#       )
#     )
#   )
# )

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
         xlab("Request Status") + ylab("Request Count") +
         ggtitle("Request Status Overview")
       )
     })
   
   # A plot showing number of prescriptions by fill year and cohort
   output$plot_presc_cohort <- renderPlotly({
     dat <- dataFiltered() %>% group_by(FILL_YEAR, start_year) %>% summarise(number = n())
     ggplot(data = dat, aes(x = as.factor(FILL_YEAR), y = number, fill = as.factor(start_year))) + 
       geom_bar(stat = "identity") + theme_bw() + 
       labs(title = "Prescription Count by Year & Cohort", x = "Fill Year", y = "Number of Prescriptions", fill = "Cohort")
     
   })
   # A plot showing number of prescriptions by fill year, filtered by dosage form
   output$plot_presc_dose <- renderPlotly({
     dat <- dataFiltered() %>% group_by(FILL_YEAR, dosage_form_clean) %>% summarise(number = n())
     ggplot(data = dat, aes(x = as.factor(FILL_YEAR), y = number, fill = dosage_form_clean)) + 
       geom_bar(stat = "identity") + theme_bw()+ 
       labs(title = "Prescription Count by Year & Dosage Form", x = "Fill Year", y = "Number of Prescriptions", fill = "Drug Form")
   })
   # A plot showing density of dosage per prescription, colored differently for different sub-populations (opiate status)
   output$plot_mme <- renderPlotly({
     dat <- dataFiltered() 
     ggplot(data = dat, aes(x = avg_MME_per_presc, fill = if_opiate_od)) + 
       geom_density(alpha = 0.5, adjust = 3) + xlim(0,7500) + theme_bw() + 
       labs(title = "Density of Dosage per Prescription", x = "Per Prescription Dosage", y = "Density", fill = "Opiate Overdose Status")
   })
   # Creating info box for average number of prescriptions per person
   output$avg_presc <- renderInfoBox({
     dat <- dataFiltered() %>% group_by(PERSON_ID) %>% summarise(ind_presc = n())
     num <- round(mean(dat$ind_presc, na.rm = T))
     # In addition in the future you might want to use prettyNum() to add commas to your numeric boxes
     infoBox("Avg # Rx per Person", 
             value = num, paste(nrow(dataFiltered()), "prescriptions"), 
             icon = icon("calculator"), color = "blue")
   })
   # Creating info box for average age of people in dataset
   output$avg_age <- renderInfoBox({
     dat <- dataFiltered() %>% group_by(PERSON_ID) %>% summarise(avg_age = max(avg_age))
     num <- round(mean(dat$avg_age, na.rm = T))
     infoBox("Avg Age", value = num, subtitle = paste(nrow(dat), "individuals"), 
             icon = icon("calculator"), color = "blue")
   })
   # Creating info box for average mental health service usage
   output$avg_mh <- renderInfoBox({
     dat <- dataFiltered() %>% group_by(PERSON_ID) %>% 
       summarise(months_activity = max(months_activity), mh_count = max(mental_health)) %>%
       group_by(PERSON_ID) %>% summarise(prop_mh_ind = mh_count/months_activity) %>% filter(prop_mh_ind > 0)
     num <- round(mean(dat$prop_mh_ind, na.rm = T),2)*100
     infoBox("Avg % Mental Health Usage", value = paste(num, "%"), subtitle = "if ever used", 
             icon = icon("calculator"), color = "blue")
   })
   # Creating info box for average drug and alcohol abuse service
   output$avg_da <- renderInfoBox({
     dat <- dataFiltered() %>% group_by(PERSON_ID) %>% 
       summarise(months_activity = max(months_activity), da_count = max(drug_alc_abuse)) %>% 
       group_by(PERSON_ID) %>% summarise(prop_da_ind = da_count/months_activity) %>% filter(prop_da_ind > 0)
     num <- round(mean(dat$prop_da_ind, na.rm = T),2)*100
     infoBox("Avg % Drug & Alc Abuse Service Use", subtitle = "if ever used", value = paste(num, "%"), 
             icon = icon("calculator"), color = "blue")
   })
   # A plot showing proportion of any service (mh, da or cyfparent) to total prescription service usage
   # This plot contains an input that allows user to select the service they are interested in viewing (changing y axis)
   output$plot_service <- renderPlotly({
     y_axis = input$yAxis_select
     filter <- dataFiltered()[c("FILL_YEAR", "num_rx", y_axis)]
     names(filter) <- c("FILL_YEAR", "num_rx", "y_selected")
     dat <- filter %>% group_by(FILL_YEAR) %>% summarise(prop = round(sum(y_selected)/sum(num_rx),2))
     ggplot(data = dat, aes(x = as.factor(FILL_YEAR), y = prop)) + 
       geom_histogram(stat = "identity", fill = "#00AFDE") + ylim(0, 1) + theme_bw() + 
       labs(title = "Service Usage Relative to Total Prescription Usage", x = "Fill Year", y = "Proportion", fill = "Service")
   })
   # Plot that showcases the association between criminal justice system elements and prescription service usage
   output$plot_jail <- renderPlotly({
     dat <- dataFiltered()
     ggplot(data = dat, aes(x = num_rx)) + 
       geom_smooth(aes(y = num_acj, color = "Months in Jail")) + 
       geom_smooth(aes(y = num_charge, color = "Number of Charges")) + 
       geom_smooth(aes(y = num_drug_charge, color = "Number of Drug Charges")) + 
       scale_colour_manual(breaks = c("num_acj", "num_charge", "num_drug_charge"), 
                           values = c("blue", "red", "purple")) + theme_bw() + 
       labs(title = "Criminal Justice System Interaction by Prescription Use", x = "Prescription Service Usage", y = "Average CJS value")
   })
   # Info box showing the proportion of their time in jail relative to their months of activity (if they have ever been jailed)
   output$avg_jail <- renderInfoBox({
     dat <- dataFiltered() %>% group_by(PERSON_ID) %>% 
       summarise(months_activity = max(months_activity), acj_count = max(num_acj)) %>%
       group_by(PERSON_ID) %>% summarise(prop_acj_ind = acj_count/months_activity) %>% filter(prop_acj_ind > 0)
     num <- round(mean(dat$prop_acj_ind, na.rm = T),2)*100
     infoBox("% Time in Jail", subtitle = "if ever jailed", value = paste(num, "%"), 
             icon = icon("calculator"), color = "blue")
   })
   # Info box showing average number of charges of an individual in the dataset (if they have ever been charged)
   output$avg_charge <- renderInfoBox({
     dat <- dataFiltered() %>% group_by(PERSON_ID) %>% summarise(num_charges = max(num_charge)) %>% filter(num_charges > 0)
     num <- round(mean(dat$num_charges, na.rm = T),2)
     infoBox("Average # of Charges", value = num, subtitle = "if ever charged", icon = icon("calculator"), color = "blue")
   })
   # Adding table output (count_data)
   output$table <- DT::renderDataTable(count_data,
                                       options = list(scrollX = TRUE),
                                       rownames = FALSE)
   # Adding downloadData output
   output$downloadData <- downloadHandler(
     filename = function() {
       paste("presc-summary-data-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.csv(count_data, file, row.names = FALSE)
     }
   )
   # Resetting all filters (global and local)
   observeEvent(input$reset, {
     updateSelectInput(session, "race_select", selected = c("White", "Black/African-American", "Other"))
     updateSelectInput(session, "gender_select", selected = c("Female", "Male", "Other"))
     updateSelectInput(session, "cohort_select",  selected = c("2010", "2011", "2012", "2013", "2014"))
     updateSelectInput(session, "od_status_select", selected = c("No Overdose", "Non-Opiate Overdose", "Opiate Overdose"))
     updateSelectInput(session, "drug_form_select", selected = c("PILL", "PATCH"))
     updateSelectInput(session, "yAxis_select", selected = colnames(merged[33]))
     alert("You have reset the dashboard filters!")
   })
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