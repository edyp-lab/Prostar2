library(shiny)
library(highcharter)
library(SummarizedExperiment)



source(file.path("../../R","mod_plots_mv_for_imputation.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_mv_ui('plots_mv_impute')
)


server <- function(input, output, session) {
  
  utils::data(Exp1_R25_pept, package='DAPARdata2')
  obj <- Exp1_R25_pept[[2]]
  conds <- colData(Exp1_R25_pept)[['Condition']]
  
  callModule(mod_plots_mv_server,'plots_mv_impute',
             obj=reactive({obj}),
             conds=reactive({conds}),
             title=reactive({NULL}),
             palette=reactive({NULL})
  )
}


shinyApp(ui=ui, server=server)
