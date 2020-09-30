library(shiny)
library(highcharter)
library(SummarizedExperiment)


source(file.path("../../../R/Plots","mod_plots_corr_matrix.R"), local=TRUE)$value
source(file.path("../../../R","mod_settings.R"), local=TRUE)$value
source(file.path("../../../R","mod_popover_for_help.R"), local=TRUE)$value
source(file.path("../../../R","global.R"), local=TRUE)$value
source(file.path("../../../R","mod_observe_dynamic_colourPicker_input.R"), local=TRUE)$value



ui <- fluidPage(
  mod_plots_corr_matrix_ui('plots_corr_matrix')
)


server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  r <- reactiveValues(
    settings = NULL
  )
  obj <- Exp1_R25_prot[[length(names(Exp1_R25_prot))]]
  names <- gsub('Intensity_','',colnames(assay(Exp1_R25_prot)))
  r$settings <- mod_settings_server("settings", obj=reactive({Exp1_R25_prot}))
  
  mod_plots_corr_matrix_server('plots_corr_matrix', 
             obj = reactive({obj}),
             names = reactive({NULL}),
             gradientRate = reactive({r$settings()$defaultGradientRate})
             )
}


shinyApp(ui=ui, server=server)
