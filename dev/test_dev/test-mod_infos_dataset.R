library(MSnbase)
source(file.path('~/Github/2.0/Prostar2/R', 'mod_infos_dataset.R'), local=TRUE)$value



ui <- fluidPage(
  mod_infos_dataset_ui('test_infos_DT')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  # require(DAPARdata)
  # data('Exp1_R25_prot')
  # callModule(mod_infos_dataset_server,'test_infos_DT',
  #            obj = reactive({Exp1_R25_prot}))
  
  
  # dat est un objet MAE, type PipelinePeptide ou PipelineProtein
  callModule(mod_infos_dataset_server,'test_infos_DT',
             obj = reactive({dat}))
}


shinyApp(ui, server)
# library(shiny); runApp('dev/test_dev/test-mod_infos_dataset.R')