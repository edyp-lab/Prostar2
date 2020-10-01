library(shiny)
library(shinyjs)
library(dplyr)

source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path('../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_observe_dynamic_colourPicker_input.R'), local=TRUE)$value



ui <- fluidPage(
  tagList(
    uiOutput('showSettings'),
    mod_settings_ui('settings')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  r <- reactiveValues(
    settings = NULL
  )
  
  r$settings <- mod_settings_server("settings", 
                                    obj = reactive({Exp1_R25_prot}))
  
  # observeEvent(r$settings(),{
  #   print(r$settings())
  # })
  
  # output$showSettings <- renderUI({
  #   #r$settings()
  #   tagList(
  #     h3('Example full palette'),
  #     HTML(unlist(r$settings()$examplePalette)),
  #     h3('Example base palette'),
  #     HTML(unlist(r$settings()$basePalette))
  #   )
  #   
  # })
  
}


shinyApp(ui, server)
