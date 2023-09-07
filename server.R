# server.R
server <- function(input, output, session) {
  
  startTime <- as.numeric(Sys.time())
  
  token <- anonymous_login(project_api = "AIzaSyDt2yl4_YFhPmaLnlowccxGJKARPfMhFjE")
  purl = "https://esp32-firebase-demo-b9d6b-default-rtdb.firebaseio.com/"
 
  
  # observeEvent(input$toggleSidebar, {
  #   shinyjs::toggle(id = "Sidebar")
  # })
  
  observeEvent(input$toggle_btn, {
    shinyjs::toggleClass(selector = "body", class = "sidebar-collapse")
  })
  
  
  observe({
    updateSelectInput(session = session, inputId = "sensor", choices = choices_sensor()$sensorNode)
  })
  
  choices_sensor <- reactive({
    purl = "https://esp32-firebase-demo-b9d6b-default-rtdb.firebaseio.com/"
    # nodename = "TestEC2"
    # nodename = input$firebaseNode
    nodename = input$node
    x = download(projectURL = purl, fileName = nodename)
    nodelist = names(x[["fakeData"]])
    df.nodelist = as.data.frame(names(x[["fakeData"]])) %>%
      setNames("sensorNode")
    
    choices_sensorRuns = df.nodelist
    
  })
  
  
  sensorInput <- reactive({
    #fname = input$firebase_test
    fname = input$sensor
    nodename = input$node

    urlPath = paste0(purl, nodename, "/", "fakeData/")
    x.df = download(projectURL = urlPath, fileName = fname)

    x.df2 = x.df %>%
      mutate(
        ID = as.integer(rownames(.))-1,
        datetime = as_datetime(time$ts/1000),
        date = as.Date(datetime),
        time1 = format(as.POSIXct(datetime), format = "%H:%M:%S"),
        # time1 = time(datetime),
        hour = hour(datetime),
        minute = minute(datetime),
        second = second(datetime),
        obs = 1
      ) %>% 
      select(
        ID,
        datetime,
        date, time1, hour, minute, second,
        everything(),
        -time
      )
    
    return(x.df2)
  })
  
  output$airtemp <- renderValueBox({
    db = sensorInput()
    x = db %>% filter(ID == max(ID))
    fb.value = x$temperature$value[1]
    valueBox(
      value = formatC(fb.value, digits = 2, format = "f"),
      subtitle = "Air Temp (F)",
      icon = icon("temperature-half"),
      color = "yellow"
    )
  })  
  
  output$humidity <- renderValueBox({
    db = sensorInput()
    x = db %>% filter(ID == max(ID))
    fb.value = x$humidity$value[1]
    valueBox(
      value = formatC(fb.value, digits = 1, format = "f"),
      subtitle = "Humidity (%)",
      icon = icon("percent"),
      color = "yellow"
    )
  })  
  
  output$pressure <- renderValueBox({
    db = sensorInput()
    x = db %>% filter(ID == max(ID))
    fb.value = x$pressure$value[1]
    valueBox(
      value = formatC(fb.value, digits = 1, format = "f"),
      subtitle = "Pressure (bar)",
      icon = icon("temperature-half"),
      color = "yellow"
    )
  }) 
  
  output$gas <- renderValueBox({
    db = sensorInput()
    x = db %>% filter(ID == max(ID))
    fb.value = x$gas$value[1]
    valueBox(
      value = formatC(fb.value, digits = 1, format = "f"),
      subtitle = "Gas (%)",
      icon = icon("fire-flame-simple"),
      color = "yellow"
    )
  }) 
  
  
  
  ### AIR PLOT  ==============================  AIR PLOTS
  # column(3, plotlyOutput("tempPlot")),
  # column(3, plotlyOutput("humidityPlot")),
  # column(3, plotlyOutput("pressurePlot")),
  # column(3, plotlyOutput("gasPlot"))
  
  ### AIR  TEMP
  output$tempPlot <- renderPlotly({
    df = sensorInput()
    plot_ly(type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~df$datetime, y = ~df$temperature$value, 
                line = list(shape = "spline", color = '#F39C12'),
                name = 'Air Temp [F]') %>%
      layout(
        title = list(text = "Air Temp [F]"),
        xaxis = list(title = 'Datetime', rangemode = "normal",
                     zerolinecolor = 'black', zerolinewidth = 6,gridcolor = 'white'
        ),
        yaxis = list(title = 'Air Temp [F]', rangemode = "normal", tickformat = ".0f",
                     zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'
        ),
        plot_bgcolor='#e5ecf6',
        showlegend = F
      )
  })   
  
  ### HUMIDITY
  output$humidityPlot <- renderPlotly({
    df = sensorInput()
    plot_ly(type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~df$datetime, y = ~df$humidity$value, 
                line = list(shape = "spline", color = '#F39C12'),
                name = 'Humidity [%]') %>%
      layout(
        title = list(text = "Humidity [%]"),
        xaxis = list(title = 'Datetime', rangemode = "normal",
                     zerolinecolor = 'black', zerolinewidth = 6,gridcolor = 'white'
        ),
        yaxis = list(title = 'Humidity [%]', rangemode = "normal", tickformat = ".0f",
                     zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'
        ),
        plot_bgcolor='#e5ecf6',
        showlegend = F
      )
  })     
  
  ### PRESSURE
  output$pressurePlot <- renderPlotly({
    df = sensorInput()
    plot_ly(type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~df$datetime, y = ~df$pressure$value, 
                line = list(shape = "spline", color = '#F39C12'),
                name = 'Pressure') %>%
      layout(
        title = list(text = "Pressure [mbar]"),
        xaxis = list(title = 'Datetime', rangemode = "normal",
                     zerolinecolor = 'black', zerolinewidth = 6,gridcolor = 'white'
        ),
        yaxis = list(title = 'Pressure [mbar]', rangemode = "normal", tickformat = ".0f",
                     zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'
        ),
        plot_bgcolor='#e5ecf6',
        showlegend = F
      )
  })   
  
  
  ### GAS
  output$gasPlot <- renderPlotly({
    df = sensorInput()
    plot_ly(type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~df$datetime, y = ~df$gas$value, 
                line = list(shape = "spline", color = '#F39C12'),
                name = 'Gas') %>%
      layout(
        title = list(text = "Gas"),
        xaxis = list(title = 'Datetime', rangemode = "normal",
                     zerolinecolor = 'black', zerolinewidth = 6,gridcolor = 'white'
        ),
        yaxis = list(title = 'Gas', rangemode = "normal", tickformat = ".0f",
                     zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'
        ),
        plot_bgcolor='#e5ecf6',
        showlegend = F
      )
  })   
  
  
  ### =================================================
  ### WATER DATA  ==============================  WATER 
  ### =================================================
  
  output$watertemp <- renderValueBox({
    db = sensorInput()
    x = db %>% filter(ID == max(ID))
    # fb.value = 99.9
    fb.value = x$extra1$value[1]
    valueBox(
      value = formatC(fb.value, digits = 1, format = "f"),
      subtitle = "Extra 1",
      icon = icon("fire-flame-simple"),
      color = "blue"
    )
  }) 
  
  output$waterph <- renderValueBox({
    db = sensorInput()
    x = db %>% filter(ID == max(ID))
    fb.value = 99.9
    valueBox(
      value = formatC(fb.value, digits = 1, format = "f"),
      subtitle = "pH",
      icon = icon("chart-simple"),
      color = "blue"
    )
  }) 
  
  output$turbidity <- renderValueBox({
    db = sensorInput()
    x = db %>% filter(ID == max(ID))
    # fb.value = 99.9
    fb.value = x$extra2$value[1]
    valueBox(
      value = formatC(fb.value, digits = 1, format = "f"),
      subtitle = "Extra 2",
      icon = icon("vial"),
      color = "blue"
    )
  }) 
  
  output$voltage <- renderValueBox({
    db = sensorInput()
    x = db %>% filter(ID == max(ID))
    fb.value = x$voltage$value[1]
    # fb.value = 99.9
    valueBox(
      value = formatC(fb.value, digits = 3, format = "f"),
      subtitle = "Voltage (V)",
      icon = icon("bolt"),
      color = "blue"
    )
  }) 
  
  
  ### PLOTS ==============================  PLOTS
  

  ### H20 TEMP
  output$watertempPlot <- renderPlotly({
    df = sensorInput()
    plot_ly(type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~df$datetime, y = ~df$extra1$value, 
                line = list(shape = "spline", color = '#0073B7'),
                name = 'H2O Temp [Extra 1]') %>%
      layout(
        title = list(text = "H20 Temp [Extra 1]"),
        xaxis = list(title = 'Datetime', rangemode = "normal",
                     zerolinecolor = 'black', zerolinewidth = 6,gridcolor = 'white'
        ),
        yaxis = list(title = 'Temp [Extra 1]', rangemode = "normal", tickformat = ".0f",
                     zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'
        ),
        plot_bgcolor='#e5ecf6',
        showlegend = F
      )
  })   
  
  ### pH
  output$waterphPlot <- renderPlotly({
    df = sensorInput()
    plot_ly(type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~df$datetime, y = ~df$extra1$value, 
                line = list(shape = "spline", color = '#0073B7'),
                name = 'pH') %>%
      layout(
        title = list(text = "pH [FAKE]"),
        xaxis = list(title = 'Datetime', rangemode = "normal",
                     zerolinecolor = 'black', zerolinewidth = 6,gridcolor = 'white'
        ),
        yaxis = list(title = 'pH', rangemode = "normal", tickformat = ".0f",
                     zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'
        ),
        plot_bgcolor='#e5ecf6',
        showlegend = F
      )
  })   
  
  ### TURBIDITY
  output$turbidityPlot <- renderPlotly({
    df = sensorInput()
    plot_ly(type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~df$datetime, y = ~df$extra2$value, 
                line = list(shape = "spline", color = '#0073B7'),
                name = 'pH') %>%
      layout(
        title = list(text = "Turbidity [Extra 2]"),
        xaxis = list(title = 'Datetime', rangemode = "normal",
                     zerolinecolor = 'black', zerolinewidth = 6,gridcolor = 'white'
        ),
        yaxis = list(title = 'Turbidity [Extra 2]', rangemode = "normal", tickformat = ".1f",
                     zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'
        ),
        plot_bgcolor='#e5ecf6',
        showlegend = F
      )
  })   
  
  
  ### VOLTAGE
  output$voltagePlot <- renderPlotly({
    df = sensorInput()
    plot_ly(type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~df$datetime, y = ~df$voltage$value, 
                line = list(shape = "spline", color = 'red'),
                name = 'Voltage') %>%
      layout(
        title = list(text = "Voltage"),
        xaxis = list(title = 'Datetime', rangemode = "normal",
                     zerolinecolor = 'black', zerolinewidth = 6,gridcolor = 'white'
        ),
        yaxis = list(title = 'Voltage [V]', rangemode = "normal", tickformat = ".2f",
                     zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'
        ),
        plot_bgcolor='#e5ecf6',
        showlegend = F
      )
  })    
  

  
  dataInput <- reactive({
  
    plot_ly(
    type = 'table',
    header = list(
      values = c("<b>Cars</b>", names(mtcars)),
      align = c('left', rep('center', ncol(mtcars))),
      line = list(width = 1, color = 'black'),
      fill = list(color = 'rgb(235, 100, 230)'),
      font = list(family = "Arial", size = 14, color = "white")
    ),
    cells = list(
      values = rbind(
        rownames(mtcars), 
        t(as.matrix(unname(mtcars)))
      ),
      align = c('left', rep('center', ncol(mtcars))),
      line = list(color = "black", width = 1),
      fill = list(color = c('rgb(235, 193, 238)', 'rgba(228, 222, 249, 0.65)')),
      font = list(family = "Arial", size = 12, color = c("black"))
    ))
    
  })
  
  
  observe({
    updateSelectInput(session = session, inputId = "sensorRun", choices = choices_sensorRuns()$sensorNode)
  })
  
  choices_sensorRuns <- reactive({
    purl = "https://esp32-firebase-demo-b9d6b-default-rtdb.firebaseio.com/"
    # nodename = "TestEC2"
    nodename = input$firebaseNode
    x = download(projectURL = purl, fileName = nodename)
    nodelist = names(x[["fakeData"]])
    df.nodelist = as.data.frame(names(x[["fakeData"]])) %>%
      setNames("sensorNode")
    
    choices_sensorRuns = df.nodelist

  })
  

  dataInput <- reactive({
    fname = input$firebase_node
    urlPath = paste0(purl,"/",fname,".json")
    data = httr::GET(url = urlPath)
    xx = jsonlite::fromJSON(httr::content(data,"text"))
    return(xx)
  })
  

    
  


  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste(input$firebaseNode, "_", input$sensorRun, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(fbtableInput(), file, row.names = FALSE)
    }
  )  
  
  
    

  sensorInput2 <- reactive({
    fname = input$sensorRun
    nodename = input$firebaseNode
    
    urlPath = paste0(purl, nodename, "/", "fakeData/")
    x.df = download(projectURL = urlPath, fileName = fname)
    
    x.df2 = x.df %>%
      mutate(
        datetime = as_datetime(time$ts/1000),
        date = as.Date(datetime),
        time1 = format(as.POSIXct(datetime), format = "%H:%M:%S"),
        obs = 1
      ) %>% 
      select(
        datetime, date, time1,
        everything()
      )
    
    return(x.df2)
  })
  
  
  fbtableInput <- reactive({
    number_of_rows = 10
    #number_of_rows = as.integer(input$maxrows)
    df = sensorInput2() 
    return(df)
  })
  
  dtInput <- reactive({
    fname = input$sensorRun
    nodename = input$firebaseNode
    # nodename = "TestEC3"
    #fname = "allDataSensor01"
    
    urlPath = paste0(purl, nodename, "/", "fakeData/")
    x.df = download(projectURL = urlPath, fileName = fname)
    df = as.data.frame(x.df)
    
    return(df)
  })
  
  
  output$fbtableDT <- renderDataTable(
    #fbtableInput(),
    dtInput(),
    #sensorInput(), 
    rownames = FALSE,
    options = list(
      pageLength = 15, 
      info = FALSE
      )
    )
  
  ### VOLTAGE
  output$bigPlot <- renderPlotly({
    df = sensorInput()
    plot_ly(type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~df$datetime, y = ~df$voltage$value, 
                line = list(shape = "spline", color = 'red'),
                name = 'Voltage') %>%
      layout(
        title = list(text = "Voltage"),
        xaxis = list(title = 'Datetime', rangemode = "normal",
                     zerolinecolor = 'black', zerolinewidth = 6,gridcolor = 'white'
        ),
        yaxis = list(title = 'Voltage [V]', rangemode = "normal", tickformat = ".2f",
                     zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'
        ),
        plot_bgcolor='#e5ecf6',
        showlegend = F
      )
  })    

  
  
}