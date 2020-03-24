options(shiny.fullstacktrace = TRUE)

source(file.path('../../R', 'mod_bug_report.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value


ui <- fluidPage(
  mod_bug_report_ui('home')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  callModule(mod_bug_report_server,'home')
  warning("Test warning message")
  print("test de print")
}


shinyApp(ui=ui, server=server)
