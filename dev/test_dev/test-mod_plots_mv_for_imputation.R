library(highcharter)
library(DAPAR)
library(shiny)

source(file.path("../../R","mod_plots_mv_for_imputation.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_mv_for_imputation_ui('plots_mv_impute')
)


server <- function(input, output, session) {
  
  library(DAPARdata)
  data("Exp1_R25_prot")
  
  callModule(mod_plots_mv_for_imputation_server,'plots_mv_impute', obj = reactive({Exp1_R25_prot}))
}


shinyApp(ui=ui, server=server)