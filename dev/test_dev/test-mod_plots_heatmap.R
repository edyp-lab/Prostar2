library(DAPAR)

source(file.path("../../R","mod_plots_heatmap.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_heatmap_ui('plots_heatmap')
)


server <- function(input, output, session) {
  
  require(DAPARdata)
  data('Exp1_R25_prot')
  
  callModule(mod_plots_heatmap_server,'plots_heatmap', obj = Exp1_R25_prot)
  # callModule(mod_plots_heatmap_server,'plots_heatmap', obj = NULL)
  # callModule(mod_plots_heatmap_server,'plots_heatmap', obj = mae)
  
}


shinyApp(ui, server)