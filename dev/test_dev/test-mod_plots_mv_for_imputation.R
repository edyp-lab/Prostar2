library(highcharter)
library(DAPAR2)
library(shiny)
library(Features)

source(file.path("../../R","mod_plots_mv_for_imputation.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_mv_ui('plots_mv_impute')
)


server <- function(input, output, session) {
  
  library(DAPARdata2)
  data("Exp1_R25_prot")
  obj <- Exp1_R25_prot[[2]]
  
  callModule(mod_plots_mv_server,'plots_mv_impute', obj = reactive({obj}))
}


shinyApp(ui=ui, server=server)