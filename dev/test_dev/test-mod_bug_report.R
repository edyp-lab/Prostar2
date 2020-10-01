options(shiny.fullstacktrace = TRUE)


library(shiny)

source(file.path('../../R', 'mod_bug_report.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value


ui <- fluidPage(
  mod_bug_report_ui('home')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  mod_bug_report_server('home')
  warning("Test warning message")
}


shinyApp(ui=ui, server=server)
