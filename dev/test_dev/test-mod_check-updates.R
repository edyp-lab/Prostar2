library(shiny)
library(XML)


source(file.path('../../R', 'mod_check_updates.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
<<<<<<< HEAD
=======
source(file.path('../../R', 'commonFunc.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value


>>>>>>> f86b1b72ba89158f1626b084ab43fbdcdd1e8e89

ui <- fluidPage(
  mod_check_updates_ui('test_check')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  callModule(mod_check_updates_server,'test_check')
}


shinyApp(ui, server)
