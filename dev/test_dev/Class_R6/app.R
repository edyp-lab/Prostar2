

# app.R
library(shiny); library(R6)

# you can use source("InfoBox.R"), however, using import::here makes your code more clear.
import::here(InfoBox, .from = "InfoBox.R")


# UI
ui = function(){
  fluidPage(
    uiOutput('info1'),
    tags$hr(),
    uiOutput('info2'),
    actionButton('test', 'clic')
  )
}

# server
server = function(input, output, session){
  
  
  rv <- reactiveValues(
    clic = NULL
  )
  
  observeEvent(input$test, {rv$clic <- input$test})
  
  infoBox1 = InfoBox$new("box1")
  infoBox2 = InfoBox$new("box2")
  
  output$info1 <- renderUI({infoBox1$ui()})
  output$info2 <- renderUI({infoBox2$ui()})
  
  infoBox1$call(msg = "I am groot", ind=rv$clic)
  infoBox2$call(msg = "I am Steve Rogers", ind=rv$clic)
}


shiny::shinyApp(ui, server)
