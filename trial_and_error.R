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


