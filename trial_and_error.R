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

