library(highcharter)
library(DAPAR)
library(shiny)

source(file.path("../../R","mod_plots_corr_matrix.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_corr_matrix_ui('plots_corr_matrix')
)


server <- function(input, output, session) {
  
  library(DAPARdata)
  data("Exp1_R25_prot")
  
  # obj est un msnset
  callModule(mod_plots_corr_matrix_server,'plots_corr_matrix', obj = Exp1_R25_prot)
  #callModule(mod_plots_corr_matrix_server,'plots_corr_matrix', obj = NULL)
  #callModule(mod_plots_corr_matrix_server,'plots_corr_matrix', obj = mae)
}


shinyApp(ui=ui, server=server)