library(shiny)



mod_hot_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4('resultat'),
    verbatimTextOutput(ns("hot"))
  )
}

mod_hot_server <- function(input, output, session){
  ns <- session$ns
  
  output$hot <- renderText({
    #unlist(R.Version())[1]
    unlist(session$userData$settings)[1]
  })
}



###----------------------------------------------------
ui <- fluidPage(
  actionButton('setUser', 'Set user df'),
  mod_hot_ui('test')
)


server <- function(input, output, session) {
  
  r <- reactiveValues(
    settings = list()
  )
  callModule(mod_hot_server, 'test')
  
  
  observeEvent(input$setUser,{
    r$settings <- list(a = input$setUser[1])
    session$userData$settings <- list(a = input$setUser[1])
    #attr(session$userData,"settings") <- r$settings
  })

}

## run app 
shinyApp(ui, server)

