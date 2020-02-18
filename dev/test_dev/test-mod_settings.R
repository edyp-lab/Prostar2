
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    mod_settings_ui('settings')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  require(DAPARdata)
  
  data('Exp1_R25_prot')
  obj <- Exp1_R25_prot
  callModule(mod_settings_server, "settings", dataIn=reactive({obj}))
}


shinyApp(ui, server)
