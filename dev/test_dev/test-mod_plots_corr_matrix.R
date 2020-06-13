library(shiny)
library(highcharter)
library(SummarizedExperiment)


source(file.path("../../R","mod_plots_corr_matrix.R"), local=TRUE)$value
source(file.path("../../R","mod_settings.R"), local=TRUE)$value
source(file.path("../../R","mod_popover_for_help.R"), local=TRUE)$value
source(file.path("../../R","global.R"), local=TRUE)$value

ui <- fluidPage(
  mod_plots_corr_matrix_ui('plots_corr_matrix')
)


server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  r <- reactiveValues(
    settings = NULL
  )
  obj <- Exp1_R25_prot[[2]]
  names <- gsub('Intensity_','',colnames(assay(obj)))
  r$settings <- callModule(mod_settings_server, "settings", obj=reactive({obj}))
  
  callModule(mod_plots_corr_matrix_server,'plots_corr_matrix', 
             obj = reactive({obj}),
             names = reactive({NULL}),
             gradientRate = reactive({r$settings()$defaultGradientRate})
             )
}


shinyApp(ui=ui, server=server)
