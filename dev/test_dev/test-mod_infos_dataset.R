source(file.path('../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'config.R'), local=TRUE)$value


ui <- fluidPage(
  mod_infos_dataset_ui('test_infos_DT')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  

   require(DAPARdata2)
   data('Exp1_R25_prot')
  # callModule(mod_infos_dataset_server,'test_infos_DT',
  #            obj = reactive({Exp1_R25_prot}))
  
  #ll.pipeline <- pipeline.defs$protein
  #dat <- Exp1_R25_prot[[2]]

   # dat est un objet MAE, type PipelinePeptide ou PipelineProtein
  callModule(mod_infos_dataset_server,'test_infos_DT',
             obj = Exp1_R25_prot[[2]])
}


shinyApp(ui, server)
# library(shiny); runApp('dev/test_dev/test-mod_infos_dataset.R')