
library(Prostar2)
library(DAPAR)
source(file.path('../../R', 'mod_open_demo_dataset.R'), local=TRUE)$value
source(file.path('../../R', 'commonFunc.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_choose_pipeline.R'), local=TRUE)$value
source(file.path('../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value

ui <- fluidPage(
  tagList(
    mod_open_demo_dataset_ui('rl')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    demoData = NULL
  )
  
  defs <- ReadPipelineConfig('../../R/pipeline.conf')
  rv$demoData <- callModule(mod_open_demo_dataset_server, "rl", pipeline.def=reactive({defs}))
}


shinyApp(ui, server)
