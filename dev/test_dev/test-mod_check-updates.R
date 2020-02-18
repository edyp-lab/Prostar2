
source(file.path('../../R', 'mod_check_updates.R'), local=TRUE)$value


ui <- fluidPage(
  mod_check_updates_ui('test_check')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  callModule(mod_check_updates_server,'test_check')
}


shinyApp(ui, server)
