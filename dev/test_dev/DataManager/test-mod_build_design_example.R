library(highcharter)


source(file.path('../../R/DataManager', 'mod_build_design_example.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    selectInput("level", "Design level", choices=1:3, width=100),
    uiOutput("viewExample")
  )
  
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  mod_build_design_example_server('design_example')
  
  output$viewExample <- renderUI({
    mod_build_design_example_ui("design_example")
  })
  
}


shinyApp(ui, server)
