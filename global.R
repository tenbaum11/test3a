# global.R
library(shiny)
# library(shinySignals)   
library(dplyr)
library(shinydashboard)
#library(bubbles)        
library(fireData)
library(jsonlite)
library(httr)
library(plotly)
library(here)
library(lubridate)
library(DT)

# Variables for firebase
fb.node.list <- c(
  # "testEC" = "testEC",
  "TestEC2" = "TestEC2",
  "TestEC3" = "TestEC3"
)

# "TestEC" = "TestEC",
# test