library(shiny)
library(highcharter)
library(SummarizedExperiment)



source(file.path("../../R/Plots","mod_plots_mv_for_imputation.R"), local=TRUE)$value
source(file.path("../../R","mod_format_DT.R"), local=TRUE)$value
source(file.path("../../R","mod_det_quant_impute_Values.R"), local=TRUE)$value

ui <- fluidPage(
  mod_det_quant_impute_Values_ui('plots_mv_impute')
)


server <- function(input, output, session) {
  
  utils::data(Exp1_R25_pept, package='DAPARdata2')
  
  mod_det_quant_impute_Values_server('plots_mv_impute',
                                     qData=reactive({NULL}),
                                     quant=reactive({1}),
                                     factor=reactive({0.5})
  )
}


shinyApp(ui=ui, server=server)
