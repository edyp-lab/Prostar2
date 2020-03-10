library(DAPAR)

ui <- fluidPage(
  mod_plots_heatmap_ui('plots_heatmap')
)


server <- function(input, output, session) {
  
  require(DAPARdata)
  data('Exp1_R25_prot')
  
  callModule(mod_plots_heatmap_server,'plots_heatmap',
             obj = Exp1_R25_prot)
  
}


shinyApp(ui, server)