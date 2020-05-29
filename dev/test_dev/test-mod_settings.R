library(shiny)
library(shinyjs)
library(dplyr)

source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path('../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    verbatimTextOutput('showSettings'),
    mod_settings_ui('settings')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  require(DAPARdata2)
  r <- reactiveValues(
    settings = NULL
  )
  #data('Exp1_R25_prot')
  #obj <- Exp1_R25_prot
  r$settings <- callModule(mod_settings_server, "settings")
  
  # observeEvent(r$settings(),{
  #   print(r$settings())
  # })
  output$showSettings <- renderText({
    #r$settings()
    HTML(r$settings()$examplePalette)
    
  })
  
}


shinyApp(ui, server)
