source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../R', 'mod_pipe_pept_hypotest.R'), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path('../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../R', 'mod_observe_dynamic_colourPicker_input.R'), local=TRUE)$value
source(file.path('../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value

library(QFeatures)
library(shiny)
library(shinyjs)
library(highcharter)
library(DAPAR2)
library(tibble)
library(DT)


ui <- fluidPage(
  tagList(
    mod_pipe_pept_hypotest_ui('pipe_hypothesis_test'),
    mod_infos_dataset_ui('infos')
  )
)


server <- function(input, output, session) {
  
  utils::data(Exp1_R25_pept, package='DAPARdata2')
  obj<- addAssay(Exp1_R25_pept, filterNA(Exp1_R25_pept, i=2)[[2]], name = "filtered_log") 
  
  
  rv <-reactiveValues(
    ret = NULL,
    current.obj = obj
  )
  
  rv$ret <-
    callModule(mod_pipe_pept_hypotest_server,'pipe_hypothesis_test',
               obj = reactive({obj}),
               ind = reactive({3}))
  
  
  callModule(mod_infos_dataset_server,'infos',
             obj = reactive({rv$current.obj}))
  
  observe({
    req(rv$ret())
    rv$current.obj <- rv$ret()
  })
  
  
  
}

shinyApp(ui, server)                    
