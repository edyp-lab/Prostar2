
library(shiny)

# InfoBox.R
mod_D = R6Class(
  "mod_D",
  public = list(
    # attributes
    id = NULL,
    toto = 0,
    
    # initializer
    initialize = function(id){
      self$id = id
    },
    
    # UI
    ui = function(){
      
      # the ns function here will prepend a prefix to all the ids in the app.
      ns = NS(self$id)
      
      tagList(
        tagList(actionButton(ns('btn'), 'D - Click me !')  )
      )
    },
    
    # server
    server = function(input, output, session, dataOut){
      #values <- reactiveValues(
      #  res = NULL
      #)
      
      observeEvent(input$btn,{
        
        self$toto <- 2 * input$btn
        dataOut$obj <- input$btn
        dataOut$name <- 'module D'
        dataOut$trigger <- runif(1,0,1)
      })
      
     

    },
    
    
    Get_toto = function(){
      print(paste0('toto = ', self$toto))
    },
    
    # call
    call = function(input, ouput, session, dataOut){
      callModule(self$server, self$id, dataOut)
    }
  )
)



mod_A_ui <- function(id){
  ns <- NS(id)
  tagList(actionButton(ns('btn'), 'A - Click me !')  )
}

mod_A_server <- function(input, output, session, dataOut){
  ns <- session$ns
  values <- reactiveValues(
    res = NULL
  )
  
  ## Handsontable
  observeEvent(input$btn,{
    dataOut$obj <- input$btn
    dataOut$name <- 'module A'
    dataOut$trigger <- runif(1,0,1)
  })
  
  
  # list(res=reactive({paste0(values$res,runif(1, 0,1))}),
  #      n = reactive({runif(1, 0,1)}))
}



###-------------------------------------------------------------------
mod_B_ui <- function(id){
  ns <- NS(id)
  tagList(actionButton(ns('btn'), 'B - Click me !')  )
}

mod_B_server <- function(input, output, session, dataOut){
  ns <- session$ns
  
  observeEvent(input$btn,{
    dataOut$obj <- input$btn
    dataOut$name <- 'module B'
    dataOut$trigger <- runif(1,0,1)
  })
  
  # list(res=reactive({paste0(values$res,runif(1, 0,1))}),
  #      n = reactive({runif(1, 0,1)}))
}


###-------------------------------------------------------------------
mod_C_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns('btn'), 'C - Click me !')
  )
}

mod_C_server <- function(input, output, session, dataOut){
  ns <- session$ns
  
  observeEvent(input$btn,{
    dataOut$obj <- input$btn
    dataOut$name <- 'module C'
    dataOut$trigger <- runif(1,0,1)
  })
  
  
  # list(res=reactive({paste0(values$res,runif(1, 0,1))}),
  #      n = reactive({runif(1, 0,1)}))
}






###----------------------------------------------------
ui <- shinyUI(fluidPage(
  tagList(
    mod_A_ui('test_A'),
    mod_B_ui('test_B'),
    mod_C_ui('test_C'),
    uiOutput('test_D'),
    textOutput('out'),
    textOutput('out_ret')
  )
))

server <- shinyServer(function(input, output, session) {
  
  r <- reactiveValues(
    ret = NULL,
    mod_D = NULL,
    # var = list(A= NULL,
    #            B = NULL,
    #            C = NULL)
    tmp = reactiveValues(),
    dataOut = reactiveValues(
      name = NULL,
      trigger = NULL,
      obj = NULL
    )
  )
  
  dataOut <- reactiveValues(
    name = NULL,
    trigger = NULL,
    obj = NULL
  )
  
  output$test_D <- renderUI({
    mod_D$ui()
  })
  # output$out <- renderText({
  #   req(r$var)
  #   #print(paste0(r$var$A(), ' ', r$var$B(), ' ', r$var$C()))
  #   print(paste0(r$var()))
  #   
  # })
  
  
  # output$out_ret <- renderText({
  #   req(r$ret)
  #   print(r$ret)
  # })
  
  # observeEvent(r$var$A(),{ r$ret <- r$var$A()})
  # observeEvent(r$var$B(),{ r$ret <- r$var$B()})
  # observeEvent(r$var$C(),{ r$ret <- r$var$C()})
  # 
  # r$var <- list(A = callModule(mod_A_server, 'test_A'),
  #               B = callModule(mod_B_server, 'test_B'),
  #               C = callModule(mod_C_server, 'test_C')
  #               )
  # 
  callModule(mod_A_server, 'test_A', dataOut=r$dataOut)
  callModule(mod_B_server, 'test_B', dataOut=r$dataOut)
  callModule(mod_C_server, 'test_C', dataOut=r$dataOut)
  mod_D <- mod_D$new('mod_D')
  mod_D$call(dataOut = r$dataOut)
  
  observeEvent(r$dataOut$trigger, {
    print(paste(r$dataOut$name,' : ', r$dataOut$obj))
  })
  
  observeEvent(mod_D$Get_toto(), {
    print(mod_D$Get_toto())
  })
})

## run app 
runApp(list(ui=ui, server=server))

