library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(DBI)
library(tibble)
library(viridis)
library(glue)
library(rlang)
library(shiny)
library(shinydashboard)
library(data.table)
library(reactable)
library(reactablefmtr)
library(ggiraph)

source("funs.R")

DT <- readRDS("sourcedata.RDS")

start_date <- as.Date(min(DT$week_starting))
WTD <- as.Date(max(DT$week_starting))
last_week <- WTD - 7

twelve_weeks <- WTD - 91 # go back 13 weeks for table and top chart

nweeks <- 13 # for chart and other titles


ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Data Quality"),
  dashboardSidebar(sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("stats",lib = 'glyphicon')),
    selectInput("tablename", "Source table", choices = unique(DT$tablename)),
    selectInput("column", "Column", choices = NULL),
    selectInput("value", "Value", choices = NULL)
  )),
  dashboardBody(
    # first row
    fluidRow(
      valueBoxOutput("value1")
      ,valueBoxOutput("value2")
      ,valueBoxOutput("value3")
    ),
   
    
    fluidRow(
      column(8,
             box(width = 12, plotOutput("plot", height = 350)),
             box(width = 12, girafeOutput("plot2", height = 350))),
      column(4,
             box(title = uiOutput("placeholder"),
                 width = 12,
                 reactableOutput("table", height = 700)))
  ),
  
  title = "COVID-19 Data Quality Explorer"
)
)

server <- function(input, output, session) {
  tablename <- reactive({
    filter(DT, tablename == input$tablename)
  })
  
  observeEvent(tablename(), {
    choices <- sort(levels(factor(unique(tablename()$column))))
    freezeReactiveValue(input, "column")
    updateSelectInput(inputId = "column", choices = choices)
  })
  
  column <- reactive({
    req(input$column)
    filter(tablename(), column == input$column)
  })
  observeEvent(column(), {
    choices <- unique(column()$value)
    freezeReactiveValue(input, "value")
    # make the values in alphabetic order using levels(factor(choices))
    # was previously choices = choices
    updateSelectInput(inputId = "value", choices = levels(factor(choices)))
  })
  
  output$table <- renderReactable({
    req(input$tablename)
    req(input$column)
    req(input$value)
    
    gt_tbl <- DT[between(week_starting,twelve_weeks, WTD)
    ][, .N, .(week_starting, tablename, column, value)] %>%
      select(week_starting, tablename, column, value, N) %>%
      filter(tablename == input$tablename & column == input$column & value == input$value) %>%
      arrange(desc(week_starting)) %>% 
      reactable(.,
                defaultSorted = list(week_starting = "asc"), 
                highlight = TRUE,
                searchable = FALSE,
                pageSizeOptions = c(25, 50, 100), 
                defaultPageSize = 50,
                resizable = TRUE, 
                wrap = FALSE,
                bordered = TRUE,
                fullWidth = TRUE,
                columns = list(
                  N = colDef(
                    align = 'center',
                    cell = data_bars(.,
                      text_position = "outside-end")), 
                  tablename = colDef(
                    show = FALSE),
                  column = colDef(
                    show = FALSE
                  )
                  )
      ) 
  })
  
  
  output$plot <- renderPlot({
    req(input$tablename)
    req(input$column)
    plotDT <- DT %>% 
      filter(tablename == input$tablename & column == input$column)
    count_plotter(plotDT, x = input$column)
  })
  
  output$plot2 <- renderGirafe({
    req(input$tablename)
    req(input$column)
    plotDT <- DT %>% 
      filter(tablename == input$tablename & column == input$column)
    girafe(percent_plotter(plotDT, x = input$column, 
                           option = "D"), # TRY C, D, F or H 
           width_svg =  12)  # controls the width of the plot
  })
  
  output$value1 <- renderValueBox({ 
    val1 <- DT[tablename == input$tablename & column == input$column, .N, .(column, week_starting)
    ][, `:=`(median = median(N), 
             average = mean(N))
      ][, head(.SD, 1)]
    valueBox(
      formatC(val1$median, big.mark = ',', format = "fg")
      , subtitle =  uiOutput("placeholder_val1")
      , color = "navy")
  }
  )
  
  
  output$value2 <- renderValueBox({ 
    val2 <- DT[tablename == input$tablename & column == input$column & week_starting == last_week, .N, .(column)]
    valueBox(
      formatC(val2$N, big.mark = ',')
      , subtitle =  uiOutput("placeholder_val2")
      , color = "blue")
  }
  )
  
  output$value3 <- renderValueBox({
    val3 <- DT[tablename == input$tablename & column == input$column & week_starting == WTD, .N, .(column)]
    valueBox(
      formatC(val3$N, big.mark = ',')
      , subtitle = uiOutput("placeholder_val3")
      , color = "light-blue")
  }
  )
  
  # make table title dynamic
  output$placeholder = renderUI({
    req(input$value)
    paste("Number of records per week where ", input$value, " is not null")
  })
  
  
  # make value boxes dynamic
  
  
  # output value 1 for 13 week median value box text
  output$placeholder_val1 = renderUI({
    req(input$column)
    paste("Median number of records per week where ", 
          input$column, 
          " is not null,", 
          " in previous", nweeks, " weeks" )
  })
  
  
  # output value 2 for last week valuebox text
  output$placeholder_val2 = renderUI({
    req(input$column)
    paste("Number of records per week where ", 
          input$column, 
          " is not null,", 
          " last complete week (w/b ", last_week, ")" )
 
  })
  
  
  # output value 3 for WTD valuebox text
  output$placeholder_val3 = renderUI({
    req(input$column)
    paste("Number of records per week where ", 
          input$column, 
          " is not null,", 
          " this week to date (w/b ", WTD, ")" )
    
  })
  
  # stop app when browser is closed:
  session$onSessionEnded(stopApp)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
