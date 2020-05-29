library(DAPAR2)
library(SummarizedExperiment)

source(file.path("../../R","mod_plots_heatmap.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_heatmap_ui('plots_heatmap')
)


server <- function(input, output, session) {
  
  require(DAPARdata2)
  data('Exp1_R25_prot')
  obj <- Exp1_R25_prot[[2]]
  
  callModule(mod_plots_heatmap_server,'plots_heatmap', obj = reactive({obj}))
  
}


shinyApp(ui, server)