
source(file.path('../../R', 'mod_build_design_example.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    selectInput("level", "Design level", choices=1:3, width=100),
    uiOutput("viewExample")
  )

)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
 callModule(mod_build_design_example_server,'design_example', designLevel=reactive({input$level}))
  
  output$viewExample <- renderUI({
    mod_build_design_example_ui("design_example")
  })
  
}


shinyApp(ui, server)
