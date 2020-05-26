library(highcharter)
library(DAPAR)
library(shiny)

source(file.path("../../R","mod_plots_corr_matrix.R"), local=TRUE)$value
source(file.path("../../R","mod_settings.R"), local=TRUE)$value
source(file.path("../../R","mod_popover_for_help.R"), local=TRUE)$value

ui <- fluidPage(
  mod_plots_corr_matrix_ui('plots_corr_matrix')
)


server <- function(input, output, session) {
  
  library(DAPARdata)
  data("Exp1_R25_prot")
  r <- reactiveValues(
    settings = NULL
  )
  r$settings <- callModule(mod_settings_server, "settings")
  
  callModule(mod_plots_corr_matrix_server,'plots_corr_matrix', 
             obj = reactive({Exp1_R25_prot}),
             gradientRate = reactive({r$settings()$defaultGradientRate}))
}


shinyApp(ui=ui, server=server)