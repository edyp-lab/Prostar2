library(shinyjs)

source(file.path('../../R', 'mod_plots_tracking.R'), local=TRUE)$value


ui <- fluidPage(
  mod_plots_tracking_ui('plots_tracking')
)


server <- function(input, output, session) {
  
  
  require(DAPARdata)
  data('Exp1_R25_prot')
  
  callModule(mod_plots_tracking_server,'plots_tracking', obj = Exp1_R25_prot, params=reactive({NULL}), reset=({FALSE}) )
  #callModule(mod_plots_tracking_server,'plots_tracking', obj = NULL, params=NULL, )
  
}


shinyApp(ui, server)
