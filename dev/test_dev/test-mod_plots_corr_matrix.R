library(highcharter)
library(DAPAR)
library(shiny)
library(SummarizedExperiment)

source(file.path("../../R","mod_plots_corr_matrix.R"), local=TRUE)$value
source(file.path("../../R","mod_settings.R"), local=TRUE)$value
source(file.path("../../R","mod_popover_for_help.R"), local=TRUE)$value
source(file.path("../../R","global.R"), local=TRUE)$value

ui <- fluidPage(
  mod_plots_corr_matrix_ui('plots_corr_matrix')
)


server <- function(input, output, session) {
  
  library(DAPARdata2)
  data("Exp1_R25_prot")
  res <- cor(assay(Exp1_R25_prot[[2]]),use = 'pairwise.complete.obs')
  r <- reactiveValues(
    settings = NULL
  )
  names <- gsub('Intensity_','',colnames(assay(Exp1_R25_prot[[2]])))
  r$settings <- callModule(mod_settings_server, "settings")
  
  callModule(mod_plots_corr_matrix_server,'plots_corr_matrix', 
             res = reactive({res}),
             names = reactive({names}),
             gradientRate = reactive({r$settings()$defaultGradientRate}))
}


shinyApp(ui=ui, server=server)