library(highcharter)
library(shinycssloaders)
library(MSnbase)
library(DAPAR)

source(file.path("../../R","mod_plots_var_dist.R"), local=TRUE)$value

ui <- fluidPage(
  mod_plots_var_dist_ui('varDistPlot')
)



server <- function(input, output, session) {
  
  
  library(DAPARdata)
  data("Exp1_R25_prot")

  # obj est un msnset
  callModule(mod_plots_var_dist_server,'varDistPlot', obj = Exp1_R25_prot)
  #callModule(mod_plots_var_dist_server,'varDistPlot', obj = NULL)
  #callModule(mod_plots_var_dist_server,'varDistPlot', obj = mae)
}


shinyApp(ui=ui, server=server)