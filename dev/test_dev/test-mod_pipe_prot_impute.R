source(file.path('../../R', 'config.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_pipe_prot_impute.R'), local=TRUE)$value
source(file.path("../../R","mod_plots_mv_for_imputation.R"), local=TRUE)$value
source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path("../../R", "mod_plots_group_mv.R"), local=TRUE)$value
source(file.path('../../R', 'mod_observe_dynamic_colourPicker_input.R'), local=TRUE)$value
source(file.path('../../R', 'mod_det_quant_impute_Values.R'), local=TRUE)$value



library(highcharter)
library(shinyjs)
library(DAPAR2)
library(DT)
library(tibble)
library(QFeatures)

options(shiny.fullstacktrace = FALSE)


ui <- fluidPage(
  tagList(
    mod_pipe_prot_impute_ui('pipe_impute'),
    mod_infos_dataset_ui('infos')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  rv <-reactiveValues(
    ret = NULL,
    current.obj = Exp1_R25_prot
  )
  
  
  # output$test <- renderHighchart({
  #   
  #   utils::data(Exp1_R25_pept, package='DAPARdata2')
  #   obj <- Exp1_R25_pept[1:1000,]
  #   conds <- colData(obj)[["Condition"]]
  #   obj <- normalizeD(obj, 2, name='norm', method='SumByColumns', conds=conds, type='overall')
  #   compareNormalizationD_HC(assay(obj, 2), assay(obj, 3), conds=conds, palette=NULL)
  # 
  # })
  # 
  
  rv$ret <- callModule(mod_pipe_prot_impute_server,
                       'pipe_impute',
                       obj = reactive({rv$current.obj})
                       )
  
  callModule(mod_infos_dataset_server,'infos',
             obj = reactive({rv$current.obj}))
  
  
  observe({
    req(rv$ret())
    rv$current.obj <- rv$ret()
  })
}


shinyApp(ui, server)
