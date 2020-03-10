library(highcharter)
library(DAPAR)

ui <- fluidPage(
  mod_plots_group_mv_ui('plots_group_mv')
)



server <- function(input, output, session) {
  
  require(DAPARdata)
  data('Exp1_R25_prot')
  
  callModule(mod_plots_group_mv_server,'plots_group_mv',
             obj = Exp1_R25_prot)
  
}


shinyApp(ui, server)