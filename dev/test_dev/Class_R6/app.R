

# app.R
library(shiny); library(R6)

# you can use source("InfoBox.R"), however, using import::here makes your code more clear.
import::here(InfoBox, .from = "InfoBox.R")

App = R6Class(
  "App",
  public = list(
    # attributes
    infoBox1 = NULL,
    infoBox2 = NULL,  
    
    # initialize
    initialize = function(){
      self$infoBox1 = InfoBox$new("box1")
      self$infoBox2 = InfoBox$new("box2")
    },
    # UI
    ui = function(){
      fluidPage(
        self$infoBox1$ui(),
        tags$hr(),
        self$infoBox2$ui()
      )
    },
    
    # server
    server = function(input, output, session){
      self$infoBox1$call(msg = "I am groot")
      self$infoBox2$call(msg = "I am Steve Rogers")
    }
  )
)

app = App$new()

shiny::shinyApp(app$ui(), app$server)