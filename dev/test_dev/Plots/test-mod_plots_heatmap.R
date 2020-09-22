library(shiny)
library(SummarizedExperiment)

source(file.path("../../../R/Plots","mod_plots_heatmap.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_heatmap_ui('plots_heatmap')
)


server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  obj <- Exp1_R25_prot[[2]]
  conds <- colData(Exp1_R25_prot)[['Condition']]
  
  callModule(mod_plots_heatmap_server,'plots_heatmap',
             obj = reactive({obj}),
             conds = reactive({conds})
             )
  
}


shinyApp(ui, server)
