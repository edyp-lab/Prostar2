
library(shiny)
library(DAPAR)


###----------------------------------------------------
ui <- shinyUI(fluidPage(
  mod_plots_missing_values_ui('test')
))

server <- shinyServer(function(input, output, session) {
 
  library(DAPARdata)
  data("Exp1_R25_prot")
  callModule(mod_plots_missing_values_server, 'test', dataIn = Exp1_R25_prot)
  
})

## run app 
runApp(list(ui=ui, server=server))

