
library(shiny)



Timeline = R6Class(
  "Timeline",
  private = list(
    chut = NULL
    
    
    ),
  
  public = list(
    # attributes
    id = NULL,
    rv = reactiveValues(
      rst_btn = NULL,
      position = 0
     ),
    rst_btn = NULL,
    position = 0,
    # initializer
    initialize = function(id){
      self$id = id
    },
    
    # UI
    ui = function(){
      
      # the ns function here will prepend a prefix to all the ids in the app.
      ns = NS(self$id)
      
      tagList(
        actionButton(ns('rst_btn'), 'Reset'),
        selectInput(ns('position'), 'Position', choices = 1:4)
      )
    },
    
    # server
    server = function(input, output, session, config){
      #values <- reactiveValues(
      #  res = NULL
      #)
      
      observeEvent(input$position,{ self$position <- input$position})
      
      observeEvent(input$rst_btn,{ 
        self$rv$rst_btn <- input$rst_btn
        })
      
      observeEvent(config$status,{
        print(paste0(config$status, collapse=' '))
      })
      
    },
    

    GetCurrentPosition = function(){invisible(self$position)},
    
    GetResetAction = function(){invisible(self$rv$rst_btn)},
    
    Get_DataOut = function(){invisible(self$rv$dataOut)},
    
    # call
    call = function(input, ouput, session, config){
      callModule(self$server, self$id, config)
    }
  )
)





###----------------------------------------------------
ui <- shinyUI(fluidPage(
  tagList(
    uiOutput('test_D'),
    actionButton('change_status', 'Change status'),
    textOutput('out'),
    textOutput('out_ret')
  )
))

server <- shinyServer(function(input, output, session) {
  
  r <- reactiveValues(
    ret = NULL,
    mod_D = NULL,
    tmp = reactiveValues(),
    dataOut = reactiveValues(
      name = NULL,
      trigger = NULL,
      obj = NULL
    )
  )
  
  config <- reactiveValues(
    name = NULL,
    steps = list(Description = T,
                 Step1 = T,
                 Step2 = T),
    status = setNames(rep(F,3), c('Description', 'Step1', 'Step2'))
  )
 
  timeline <- Timeline$new('timeline')
  timeline$call(config=config)
  
  observeEvent(input$change_status, {
    config$status <-  setNames(runif(3,0,1), c('Description', 'Step1', 'Step2'))
  })
  
  
  output$test_D <- renderUI({timeline$ui()})
  
  # observeEvent(pipe$Get_DataOut(), {
  #   print(paste(pipe$Get_DataOut()$name,' : ', pipe$Get_DataOut()$obj))
  # })
  # 
  
  observeEvent(timeline$GetCurrentPosition(), {
    print(paste('New current position = ', timeline$GetCurrentPosition))
  })
 
  observeEvent(timeline$GetResetAction(), {
    print(paste('Action button activated = ', timeline$GetResetAction()))
  })
  
  observeEvent(config$status, {
    print(paste('New config status = ', paste0(config$status, collapse=' ')))
  })
  
  observeEvent(timeline$GetChut(), {
    print(paste('New chut = ',timeline$GetChut()))
  })
})

## run app 
runApp(list(ui=ui, server=server))

