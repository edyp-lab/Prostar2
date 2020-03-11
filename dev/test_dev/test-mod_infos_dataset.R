library(DAPAR)
source(file.path('../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'commonFunc.R'), local=TRUE)$value

ui <- fluidPage(
  mod_infos_dataset_ui('test_infos_DT')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
   require(DAPARdata)
   data('Exp1_R25_prot')
  # callModule(mod_infos_dataset_server,'test_infos_DT',
  #            obj = reactive({Exp1_R25_prot}))
  
   defs <- ReadPipelineConfig('../../R/pipeline.conf')
  ll.pipeline <- defs$protein
  mae <- DAPAR::PipelineProtein(analysis= 'analysis', 
                         pipelineType = 'protein', 
                         dataType ='protein',
                         processes=NULL, 
                         experiments=list(original=Exp1_R25_prot), 
                         colData=Biobase::pData(Exp1_R25_prot)
  )
                         
            
  # dat est un objet MAE, type PipelinePeptide ou PipelineProtein
  callModule(mod_infos_dataset_server,'test_infos_DT',
             obj = NULL)
}


shinyApp(ui, server)
# library(shiny); runApp('dev/test_dev/test-mod_infos_dataset.R')