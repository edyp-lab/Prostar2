library(shiny)
library(rhandsontable)
library(QFeatures)
library(S4Vectors)
library(htmlwidgets)
library(DT)
# for (f in list.files('../../R', pattern='.R')){
#   source(file.path('../../R', f), local=TRUE)$value
# }

#library(DAPAR2)
source(file.path('../../R', 'config.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_select_keyID.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_choose_pipeline.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_open_dataset.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_import_file_from.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_build_design.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_build_design_example.R'), local=TRUE)$value
source(file.path('../../R/DataManager', 'mod_convert_ms_file.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    mod_convert_ms_file_ui('convert'),
    mod_infos_dataset_ui('infos')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    convertData = NULL
  )
  
  
  rv$convertData <- mod_convert_ms_file_server('convert', pipeline.def=reactive({pipeline.defs}))
  
  mod_infos_dataset_server('infos', 
                           obj = reactive({rv$convertData() })
  )
}


shinyApp(ui, server)
