library(highcharter)
library(DAPAR)

source(file.path("../../R","mod_plots_group_mv.R"), local=TRUE)$value

ui <- fluidPage(
  mod_plots_group_mv_ui('plots_group_mv')
)



server <- function(input, output, session) {
  
  require(DAPARdata)
  data('Exp1_R25_prot')
  
  callModule(mod_plots_group_mv_server,'plots_group_mv', obj = Exp1_R25_prot)
  # callModule(mod_plots_group_mv_server,'plots_group_mv', obj = NULL)
  # callModule(mod_plots_group_mv_server,'plots_group_mv', obj = mae)
  
}


shinyApp(ui, server)