

library(rhandsontable)
library(shiny)



mod_hot_ui <- function(id){
  ns <- NS(id)
  tagList(
    #rHandsontableOutput(ns("hot"))
  )
}

mod_hot_server <- function(input, output, session, r=NULL){
  ns <- session$ns
  
  values <- reactiveValues(
    df = NULL
  )
  
  ## Handsontable
  observeEvent(req(r()),{
    values$df <- r()
    print(values$df)
    print(r())
  })
  
  # output$hot <- renderRHandsontable({
  #   req(values$df)
  #   tmp <- rhandsontable(values$df)
  #   tmp
  # })
  
}



###----------------------------------------------------
ui <- shinyUI(fluidPage(
  actionButton('setUser', 'Set user df'),
  mod_hot_ui('test')
))

server <- shinyServer(function(input, output, session) {
  
  r <- reactiveValues(
    settings = NULL
  )
  observeEvent(input$setUser,{
    r$settings <- data.frame(a=1:3, b=4:6)
    #print(session$userData$df)
  })
  
  callModule(mod_hot_server, 'test', r=reactive({NULL}))
  
})

## run app 
runApp(list(ui=ui, server=server))

