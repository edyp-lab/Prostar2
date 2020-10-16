# app.R
library(shiny); library(R6)

# you can use source("InfoBox.R"), however, using import::here makes your code more clear.
#import::here(InfoBox, .from = "InfoBox.R")




Temp = R6Class(
  "Temp",
  public = list(
    # attributes
    timeline = NULL,
    id = NULL,
    
    # initialize
    initialize = function(id){
       self$id = id
    },
    # UI
    ui = function(){
      ns = NS(self$id)
      tagList(
       uiOutput(ns('tutu'))
      )
    },
    
    # server
    server = function(input, output, session){
      ns = NS(self$id)
      self$timeline <- TimelineStyle$new('timeline')
      self$timeline$call(style=2,
                         steps = list(Description=T,
                                      Step1 = F,
                                      Step2 = T),
                         status = c(Description=  1,
                                    Step1 = -1,
                                    Step2 = 1),
                         pos = 2
      )

      output$tutu <- renderUI({ self$timeline$ui()})
    },
    
    
    call = function(input, ouput, session){
      callModule(self$server, self$id)
    }
  )
)




App = R6Class(
  "App",
  public = list(
    # attributes
    infoBox1 = NULL,
    infoBox2 = NULL,  
    temp = NULL,
    
    # initialize
    initialize = function(){
      self$infoBox1 = InfoBox$new("box1")
      self$infoBox2 = InfoBox$new("box2")
      self$temp = Temp$new('timeline')
      
    },
    # UI
    ui = function(){
      tagList(
        self$temp$ui() ,
        tags$hr(),
        self$infoBox1$ui(),
        tags$hr(),
        self$infoBox2$ui()
      )
    },
    
    # server
    server = function(input, output, session){
      self$infoBox1$call(msg = "I am groot")
      self$infoBox2$call(msg = "I am Steve Rogers")
      self$temp$call()

    }
  )
)

app = App$new()

shiny::shinyApp(app$ui(), app$server)