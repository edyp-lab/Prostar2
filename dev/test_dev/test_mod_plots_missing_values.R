
library(shiny)
library(DAPAR2)


###----------------------------------------------------
ui <- shinyUI(fluidPage(
  mod_plots_missing_values_ui('test')
))

server <- shinyServer(function(input, output, session) {
 
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  callModule(mod_plots_missing_values_server, 'test', dataIn = Exp1_R25_prot)

})

## run app 
runApp(list(ui=ui, server=server))

