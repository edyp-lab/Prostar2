library(shiny)


source(file.path('../../R', 'mod_check_updates.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'utils_Prostar.R'), local=TRUE)$value


ui <- fluidPage(
  mod_check_updates_ui('test_check')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  mod_check_updates_server('test_check')
}


shinyApp(ui, server)
