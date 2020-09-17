source(file.path('../../R', 'mod_shinyalert.R'), local=TRUE)$value

library(shiny)
library(shinyalert)

ui <- fluidPage(
  mod_shinyalert_ui('info')
)

server <- function(input, output) {
  
  
  rv <- reactiveValues(
    value = NULL
    )
  
  rv$value <- callModule(mod_shinyalert_server, 'info')
  
  observe({
    rv$value
    
    cat(rv$value())
  })
}

shinyApp(ui, server)