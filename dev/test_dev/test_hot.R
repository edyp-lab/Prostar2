library(rhandsontable)
library(shiny)



mod_hot_ui <- function(id){
  ns <- NS(id)
  tagList(
    rHandsontableOutput(ns("hot2"))
  )
}

mod_hot_server <- function(input, output, session){
  ns <- session$ns

  values <- reactiveValues(
    df = data.frame(a=1:3,b=4:6)
  )
  
  ## Handsontable
  observeEvent(input$hot2,{
    values$df <- hot_to_r(input$hot2)
    print(values$df)
  })
  
  output$hot2 <- renderRHandsontable({
    values$df
    tmp <- rhandsontable(values$df)
    tmp
  })

}



###----------------------------------------------------
  ui <- shinyUI(fluidPage(
    mod_hot_ui('test')
  ))
  
  server <- shinyServer(function(input, output) {
    
    callModule(mod_hot_server, 'test')
    
  })
  
  ## run app 
  runApp(list(ui=ui, server=server))
