  for (f in list.files('../../R', pattern='.R')){
  source(file.path('../../R', f), local=TRUE)$value
}

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
