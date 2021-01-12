library(DAPAR2)
library(shiny)
source(file.path('../../../R/DataManager', 'mod_select_keyID.R'), local=TRUE)$value
source(file.path('../../../R/DataManager', 'mod_select_keyID_from_MSnset.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../../R/DataManager', 'mod_choose_pipeline.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../../R/DataManager', 'mod_open_dataset.R'), local=TRUE)$value
source(file.path('../../../R', 'global.R'), local=TRUE)$value
source(file.path('../../../R', 'config.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    mod_open_dataset_ui('rl'),
    hr(),
    mod_infos_dataset_ui("infos")
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    openData = NULL
  )
  
  rv$openData <- mod_open_dataset_server("rl", pipeline.def=reactive({pipeline.defs}))
  
  
  mod_infos_dataset_server('infos', 
                           obj = reactive({rv$openData() })
  )

}


shinyApp(ui, server)
