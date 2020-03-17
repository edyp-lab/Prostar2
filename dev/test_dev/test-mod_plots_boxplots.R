library(highcharter)
library(DAPAR)


source(file.path("../../R","mod_plots_boxplots.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_boxplots_ui('plots_density')
)



server <- function(input, output, session) {
  
  
  library(DAPARdata)
  data("Exp1_R25_prot")
  
  # obj est un msnset
  callModule(mod_plots_boxplots_server,'plots_density', obj = Exp1_R25_prot)
}


shinyApp(ui=ui, server=server)
