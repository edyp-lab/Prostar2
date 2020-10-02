library(shiny)


#set max upload file size to 30 Mo
options(shiny.maxRequestSize=300*1024^2)

source(file.path('../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_import_file_from.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    mod_import_file_from_ui('import'),
    verbatimTextOutput('showHead')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  rv.import <- reactiveValues(
    data = NULL
  )
  rv.import$data <- mod_import_file_from_server('import')
  
  output$showHead <- renderText({
    req(rv.import$data())
    paste0(colnames(rv.import$data()),'\n')
    })
}


shinyApp(ui, server)
