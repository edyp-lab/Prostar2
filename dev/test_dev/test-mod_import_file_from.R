
source(file.path('../../R', 'mod_import_file_from.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    mod_import_file_from_ui('import')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  callModule(mod_import_file_from_server, 'import')
}


shinyApp(ui, server)
