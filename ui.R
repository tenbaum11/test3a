# ui.R
library(plotly)
library(shinyjs)
library(lubridate)
dashboardPage(
  dashboardHeader(
    title = "cran.rstudio.com"
    ),
  dashboardSidebar(
    selectInput(inputId = "node", label = "Select FB Node", choices = fb.node.list, selected = "TestEC2"),
    selectInput(inputId = "sensor", label = "Select Sensor", choices = NULL, selected = "fakeSensor01"),
    #textInput("node", "node", "TestEC2"),
    textInput("firebase_test", "Firebase Test", "fakeSensor01"),
    sliderInput(inputId = "n", label = "n", min = 1, max = 30, value = 4),
    #selectInput(inputId = "cities", label = "Select City", choices = NULL),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Raw data", tabName = "rawdata")
    ),
    sliderInput("rateThreshold", "Warn when rate exceeds", min = 0, max = 50, value = 3, step = 0.1)
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    actionButton("toggle_btn", " Toggle sidebar "),
    HTML("<br><br>"),
    tabItems(
      tabItem("dashboard",
              fluidRow(column(12,
                              valueBoxOutput(width=3, "airtemp"),              
                              valueBoxOutput(width=3, "humidity"),
                              valueBoxOutput(width=3, "pressure"),
                              valueBoxOutput(width=3, "gas"))
              ),
              fluidRow(
                column(3, plotlyOutput("tempPlot")),
                column(3, plotlyOutput("humidityPlot")),
                column(3, plotlyOutput("pressurePlot")),
                column(3, plotlyOutput("gasPlot"))
              ),
              fluidRow(column(12,
                              valueBoxOutput(width=3, "watertemp"),              
                              valueBoxOutput(width=3, "waterph"),
                              valueBoxOutput(width=3, "turbidity"),
                              valueBoxOutput(width=3, "voltage"))
              ),
              fluidRow(
                column(3, plotlyOutput("watertempPlot")),
                column(3, plotlyOutput("waterphPlot")),
                column(3, plotlyOutput("turbidityPlot")),
                column(3, plotlyOutput("voltagePlot"))
              )
      ),
      tabItem("rawdata",
              selectInput(inputId = "firebaseNode", label = "Select Firebase Node (ex: TestEC2 )", choices = fb.node.list),
              selectInput(inputId = "sensorRun", label = "Select Sensor Run (ex: fakeSensor01 )", choices = NULL),
              #numericInput("maxrows", "Rows to show", 10),
              verbatimTextOutput("rawtable"),
              # selectInput("dataset", "Choose a dataset:", choices = c("rock", "pressure", "cars")),
              fluidRow( column(12, plotlyOutput("bigPlot"))),
              downloadButton("downloadCSV", "Download as CSV"),
              # downloadButton("downloadData", "Download"),
              fluidRow( column(12,dataTableOutput('fbtableDT'))),
              # fluidRow( column(12,dataTableOutput('fbtableDT')))
        )
    )
  )
)
