# app.R
library(shiny); library(R6)
source(file.path('.', 'mod_timeline.R'), local=TRUE)$value


Temp = R6Class(
  "Temp",
  public = list(
    # attributes
    temp = NULL,
    id = NULL,
    
    # initialize
    initialize = function(id){
      self$id = id
    },
    # UI
    ui = function(){
      ns = NS(self$id)
      tagList(
        mod_timeline_ui(ns("timeline"))
      )
    },
    
    # server
    server = function(input, output, session){
      ns = NS(self$id)
      mod_timeline_server("timeline", 
                          style=2,
                          steps = list(Description=T,
                                       Step1 = F,
                                       Step2 = T),
                          status = c(Description=  1,
                                     Step1 = -1,
                                     Step2 = 1),
                          pos = 1
                          )

      
      output$tutu <- renderUI({ 
        #browser()
        self$temp$ui()
        })
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
    temp = NULL,
    classA = NULL,
    
    # initialize
    initialize = function(){
      self$temp = Temp$new('timeline')

      
    },
    # UI
    ui = function(){
      tagList(
        self$temp$ui(),
        hr(),
        
        mod_timeline_ui("timeline")
      )
    },
    
    # server
    server = function(input, output, session){
      self$temp$call()
      mod_timeline_server("timeline", 
                          style=2,
                          steps = list(Description=T,
                                       Step1 = F,
                                       Step2 = T),
                          status = c(Description=  1,
                                     Step1 = -1,
                                     Step2 = 1),
                          pos = 1
                          )
    }
  )
)

app = App$new()

shiny::shinyApp(app$ui(), app$server)