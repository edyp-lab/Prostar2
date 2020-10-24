# app.R
library(shiny); library(R6)

# TODO make uiOutput work with nested classes

ClassA = R6Class(
  "ClassA",
  private = list(
    dataIn = NULL
  ),
  public = list(
    # attributes
    id = NULL,
    dataOut = NULL,

    # initialize
    initialize = function(id, dataIn){
      self$id = id
      private$dataIn <- dataIn
    },
    # UI
    ui = function(){
      ns = NS(self$id)
      tagList(
        uiOutput(ns('showBtnA'))
      )
    },
    
    # server
    server = function(id){
      
      moduleServer(id,
                   function(input, output, session){
                     #ns = session$ns
                     ns = NS(self$id)
                     
                     output$showBtnA <- renderUI({
                       actionButton(ns('btnA'), 'clic A')
                     })
                     observe({
                       input$btnA
                       print(paste0('in observe, input$bntA = ', input[['class_A-btnA']]))
                     })
                     observeEvent(input[['class_A-btnA']], ignoreInit = F,{
                       print('observeEvent on btn_A')
                       self$dataOut <- input$btnA
                     })
                     
                     reactive({self$dataOut})
                   }
      )
    },
    
    GetDataOut = function(){
      self$dataOut
      },
    
    call = function(input, ouput, session){
      print('call to function server of Class_A')
      self$server(self$id)
      print(self$server(self$id))

     # reactive({self$server(self$id)})
    }
  )
)

#----------------------------------------------------------


Pipeline = R6Class(
  "Pipeline",
  private = list(
    dataIn = NULL
  ),
  public = list(
    # attributes
    tempA = NULL,
    id = NULL,
    dataOut = NULL,
    
    # initialize
    initialize = function(id, dataIn){
      self$id = id
      private$dataIn = dataIn
    },
    # UI
    ui = function(){
      ns = NS(self$id)
      tagList(
        uiOutput(ns('show'))
      )
    },
    
    
    # server
    server = function(id){
      moduleServer(id,
                   function(input, output, session){
                     ns = session$ns 
                     self$tempA = ClassA$new('class_A', self$dataIn)
                     self$tempA$call()
                     
                     output$show <- renderUI({
                       self$tempA$ui()
                     })
                     
                     # observeEvent(self$tempA$call(), {
                     #   print(self$tempA$call())
                     # })
                     
                     reactive({self$dataOut})
                   })
    },
    
    call = function(input, ouput, session){
      self$server(self$id)
    }
  )
)

#----------------------------------------------------------

data = 3
pipe = Pipeline$new('pipe', data)

    # UI
ui <- function(){
      tagList(
        pipe$ui()
      )
}
    
    # server
server <- function(input, output, session){
      pipe$call()
}


shiny::shinyApp(ui, server)