library(dplyr)
library(DT)
library(shiny)


source(file.path("../../R","mod_plots_msnset_explorer.R"), local=TRUE)$value
source(file.path("../../R","mod_plots_legend_colored_exprs.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_msnset_explorer_ui('msnset_explorer')
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  require(DAPARdata)
  data('Exp1_R25_prot')
  
  callModule(mod_plots_msnset_explorer_server,'msnset_explorer', obj = reactive({Exp1_R25_prot}))
  #callModule(mod_plots_msnset_explorer_server,'msnset_explorer', obj = NULL)
  #callModule(mod_plots_msnset_explorer_server,'msnset_explorer', obj = mae)

  
}


shinyApp(ui, server)
