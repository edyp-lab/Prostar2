library(shiny)
library(MultiAssayExperiment)


source(file.path('../../R', 'mod_choose_pipeline.R'), local=TRUE)$value
source(file.path('../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'config.R'), local=TRUE)$value
source(file.path('../../R', 'mod_open_demo_dataset.R'), local=TRUE)$value

actionBtnClass <- "btn-primary"

ui <- fluidPage(
  tagList(
    mod_open_demo_dataset_ui('rl'),
    hr(),
    
    mod_infos_dataset_ui("infos")
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    demoData = NULL
  )
  
  rv$demoData <- callModule(mod_open_demo_dataset_server, "rl", pipeline.def=reactive({pipeline.defs}))

# mod_infos_dataset prend un objet mae
callModule(mod_infos_dataset_server, 
           'infos', 
           obj = reactive({
             req(rv$demoData())
             rv$demoData()
           })
           )
}


shinyApp(ui, server)
