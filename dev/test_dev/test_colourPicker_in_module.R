library(shiny)

source(file.path('../../R', 'mod_observe_dynamic_colourPicker_input.R'), local=TRUE)$value

ui <- fluidPage(
  mod_observe_dynamic_colourPicker_input_ui('test'),
  uiOutput('printColors')
)



server <- function(input, output,session) {
  
  rv.dyn <- reactiveValues(
    colors = NULL)
  
  conds <- LETTERS[1:4]
  rv.dyn$colors <- mod_observe_dynamic_colourPicker_input_server('test', 
                                                                 n=reactive({4}),
                                                                 label=reactive({conds}))
  
  
  output$printColors <- renderUI({
    HTML(rv.dyn$colors())
  })
  
}

shinyApp(ui = ui, server = server)