library(shiny)
library(highcharter)

source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../R', 'config.R'), local=TRUE)$value


ui <- fluidPage(
  mod_infos_dataset_ui('test_infos_DT')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  
  utils::data(Exp1_R25_pept, package='DAPARdata2')
  
  # dat est un objet MAE, type PipelinePeptide ou PipelineProtein
  mod_infos_dataset_server('test_infos_DT',
                           obj = reactive({Exp1_R25_pept}))
}


shinyApp(ui, server)
