library(shiny)
library(highcharter)
library(SummarizedExperiment)



source(file.path("../../../R/Plots","mod_plots_mv_for_imputation.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_mv_for_imputation_ui('plots_mv_impute')
)


server <- function(input, output, session) {
  
  utils::data(Exp1_R25_pept, package='DAPARdata2')
 
  callModule(mod_plots_mv_for_imputation_server,'plots_mv_impute',
             obj = reactive({Exp1_R25_pept}),
             ind = reactive({2}),
             title = reactive({NULL}),
             palette = reactive({NULL})
  )
}


shinyApp(ui=ui, server=server)
