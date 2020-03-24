
library(shiny)



mod_A_ui <- function(id){
  ns <- NS(id)
  tagList(
   actionButton(ns('btn'), 'A - Click me !')
  )
}

mod_A_server <- function(input, output, session){
  ns <- session$ns
  values <- reactiveValues(
    res = NULL
  )
  
  ## Handsontable
  observeEvent(input$btn,{
    values$res<- input$btn
  })
  
  return(reactive({values$res}))
}



###-------------------------------------------------------------------
mod_B_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns('btn'), 'B - Click me !')
  )
}

mod_B_server <- function(input, output, session){
  ns <- session$ns
  values <- reactiveValues(
    res = NULL
  )
  
  ## Handsontable
  observeEvent(input$btn,{
    values$res<- input$btn
  })
  
  return(reactive({values$res}))
}


###-------------------------------------------------------------------
mod_C_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns('btn'), 'C - Click me !')
  )
}

mod_C_server <- function(input, output, session){
  ns <- session$ns
  values <- reactiveValues(
    res = NULL
  )
  
  ## Handsontable
  observeEvent(input$btn,{
    values$res<- input$btn
  })
  
  return(reactive({values$res}))
}






###----------------------------------------------------
ui <- shinyUI(fluidPage(
  tagList(
    mod_A_ui('test_A'),
    mod_B_ui('test_B'),
    mod_C_ui('test_C'),
    textOutput('out'),
    textOutput('out_ret')
  )
))

server <- shinyServer(function(input, output, session) {
  
  r <- reactiveValues(
    ret = NULL,
    var = list(A= NULL,
               B = NULL,
               C = NULL)
  )
  

  output$out <- renderText({
    req(r$var)
    print(paste0(r$var$A(), ' ', r$var$B(), ' ', r$var$C()))

  })
  
  
  output$out_ret <- renderText({
    req(r$ret)
    print(r$ret)
    
  })
  
  
  observeEvent(r$var$A(),{ r$ret <- r$var$A()})
  observeEvent(r$var$B(),{ r$ret <- r$var$B()})
  observeEvent(r$var$C(),{ r$ret <- r$var$C()})
  
  r$var <- list(A = callModule(mod_A_server, 'test_A'),
                B = callModule(mod_B_server, 'test_B'),
                C = callModule(mod_C_server, 'test_C')
  )
  
})

## run app 
runApp(list(ui=ui, server=server))

