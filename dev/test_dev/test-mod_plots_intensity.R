library(highcharter)
library(DAPAR)

ui <- fluidPage(
  mod_plots_intensity_ui('plots_intensity')
)



server <- function(input, output, session) {
  
  require(DAPARdata)
  data('Exp1_R25_prot')
  
  callModule(mod_plots_intensity_server,'plots_intensity',
             obj = Exp1_R25_prot)
  
}


shinyApp(ui, server)