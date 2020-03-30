library(DAPAR)
library(shiny)
source(file.path('../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'config.R'), local=TRUE)$value


ui <- fluidPage(
  mod_infos_dataset_ui('test_infos_DT')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  require(DAPARdata)
  data('Exp1_R25_pept')
  data('Exp1_R2_pept')

  ll.pipeline <- pipeline.defs$peptide
  mae <- DAPAR::PipelinePeptide(analysis= 'analysis', 
                                pipelineType = 'peptide', 
                                dataType ='peptide',
                                processes=NULL, 
                                experiments=list(original=Exp1_R25_pept, second=Exp1_R25_pept), 
                                colData=Biobase::pData(Exp1_R25_pept)
  )
  
  
  # dat est un objet MAE, type PipelinePeptide ou PipelineProtein
  callModule(mod_infos_dataset_server,'test_infos_DT', obj = reactive({mae}))
  #callModule(mod_infos_dataset_server,'test_infos_DT', obj = NULL)
}


shinyApp(ui, server)
# library(shiny); runApp('dev/test_dev/test-mod_infos_dataset.R')