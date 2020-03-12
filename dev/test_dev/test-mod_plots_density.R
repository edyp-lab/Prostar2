library(highcharter)
library(DAPAR)


source(file.path("../../R","mod_plots_density.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_density_ui('plots_density')
)



server <- function(input, output, session) {
  
  
  library(DAPARdata)
  data("Exp1_R25_prot")
  
  
  # obj est un msnset
  callModule(mod_plots_density_server,'plots_density', obj = Exp1_R25_prot)
  #callModule(mod_plots_density_server,'plots_density', obj = NULL)
  #callModule(mod_plots_density_server,'plots_density', obj = mae)
}


shinyApp(ui=ui, server=server)
