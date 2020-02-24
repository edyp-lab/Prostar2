
library(shiny)



mod_return_ui <- function(id){
  ns <- NS(id)
  tagList(
   actionButton(ns('btn'), 'Click me !')
  )
}

mod_return_server <- function(input, output, session){
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
  mod_return_ui('test'),
  textOutput('out')
))

server <- shinyServer(function(input, output, session) {
  
  r <- reactiveValues(
    ret = NULL
  )
  
  observeEvent(r$ret,{
    r$settings <- data.frame(a=1:3, b=4:6)
    #print(session$userData$df)
  })
  
  output$out <- renderText({
    req(r$ret())
    print(r$ret())
  })
  
  r$ret <- callModule(mod_return_server, 'test')
  
})

## run app 
runApp(list(ui=ui, server=server))

