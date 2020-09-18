source(file.path('../../../../R', 'config.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('../../../../R/PipelineCode/protein', 'mod_pipe_protein_Imputation.R'), local=TRUE)$value
source(file.path("../../../../R/Plots", "mod_plots_mv_for_imputation.R"), local=TRUE)$value
source(file.path('../../../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path("../../../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path('../../../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../../../R', 'mod_observe_dynamic_colourPicker_input.R'), local=TRUE)$value
source(file.path('../../../../R', 'mod_det_quant_impute_Values.R'), local=TRUE)$value



# library(highcharter)
# library(shinyjs)
# library(DAPAR2)
# library(DT)
# library(tibble)
# library(QFeatures)

options(shiny.fullstacktrace = FALSE)


ui <- fluidPage(
  tagList(
    mod_pipe_protein_Imputation_ui('pipe_impute'),
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
  
  
  rv$ret <- callModule(mod_pipe_protein_Imputation_server,
                       'pipe_impute',
                       obj = reactive({Exp1_R25_prot}),
                       indice = reactive({2})
                       )
  
  # callModule(mod_infos_dataset_server,'infos',
  #            obj = reactive({req(rv$ret)
  #              rv$ret}))
  # 
  
  # observe({
  #   req(rv$ret())
  #   rv$current.obj <- rv$ret()
  # })
}


shinyApp(ui, server)
