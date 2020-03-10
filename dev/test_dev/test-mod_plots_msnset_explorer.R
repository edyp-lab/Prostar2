library(MSnbase)


ui <- fluidPage(
  mod_plots_msnset_explorer_ui('msnset_explorer')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  require(DAPARdata)
  data('Exp1_R25_prot')
  callModule(mod_plots_msnset_explorer_server,'msnset_explorer',
             obj = reactive({Exp1_R25_prot}))
  
}


shinyApp(ui, server)