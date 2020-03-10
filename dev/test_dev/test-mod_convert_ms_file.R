source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'mod_convert_ms_file.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../R', 'commonFunc.R'), local=TRUE)$value
source(file.path('../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../R', 'mod_insert_md.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    mod_convert_ms_file_ui('convert')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  callModule(mod_convert_ms_file_server, 'convert')
}


shinyApp(ui, server)
