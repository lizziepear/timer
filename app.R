## Presentation timer
## Lizzie Pearmain, September 2019

## bits of code taken from:
## Timer app: https://stackoverflow.com/questions/49250167/how-to-create-a-countdown-timer-in-shiny
## Countdown plots: https://github.com/andrewheiss/gif_timer

# rm(list=ls())

library(lubridate)
library(shiny)
library(beepr)
library(ggplot2)

## source functions
source("timer_functions.R")


ui <- fluidPage(
  
  titlePanel("Presentation Timer"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Timer options"),
      br(),
      p("Time limit:"),
      numericInput('time1','Minutes:',value=10,min=0,max=30,step=0.5),
      
      p("Extra time:"),
      numericInput('grace', 'Minutes:',value=5,min=0,max=10,step=0.5),
      
      tags$br(),
      h4("Start timer:"),
      actionButton('start','Start'),
      actionButton('stop','Stop'),
      actionButton('reset','Reset'),
      
      hr(),
      h5("test noises:"),
      actionButton('testBleep', 'Test bleep 1'),
      actionButton('testMusic', 'Test bleep 2')
      
    ),
    mainPanel(
      # textOutput('timeleft'),
      # textOutput('timer'),
      # textOutput('sounder'),
      plotOutput('timePlot'),
      plotOutput('pie', width = 500, height = 500)
    )
  )
)

server <- function(input, output, session) {
  
  # Initialize the timer, 1 seconds, not active.
  timer <- reactiveVal(1)
  active <- reactiveVal(FALSE)
  # sounder <- reactiveVal(1)
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
    
  })
  
  output$timer <- renderText({
    paste(timer())
  })
  # output$sounder <- renderText({
  #   paste(sounder())
  # })
  
  # Output the pie chart of time left
  output$pie <- renderPlot({
    makePie(input$time1 * 60, timer())
  })
  
  output$timePlot <- renderPlot({
    printTime(timer())
  })
  
  ## observer to play noise when test buttons are clicked
  observeEvent(input$testBleep, {beep(2)})
  observeEvent(input$testMusic, {beep(8)})
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active())
      {
        timer(timer()-1)

        if(timer() == 0){  #   | timer() == -2
          beep(sound = 2, expr = NULL)
        }
        
        if(timer() == -(input$grace*60))
        {
          active(FALSE)
          showModal(modalDialog(
            title = "URGENT",
            "Time's up!"
          ))
          beep(sound = 8, expr = NULL)
        }

      }
    })
  })
  
  # observers for actionbuttons
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$reset, {timer(input$time1 * 60)})
  
}

shinyApp(ui, server)