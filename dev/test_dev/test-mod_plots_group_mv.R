library(highcharter)
library(DAPAR)
library(shiny)

source(file.path("../../R","mod_plots_group_mv.R"), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path('../../R', 'plots_mv_perLine_histo.R'), local=TRUE)$value

ui <- fluidPage(
  mod_plots_group_mv_ui('plots_group_mv'),
  mod_settings_ui('settings')
)



server <- function(input, output, session) {
  
  require(DAPARdata)
  data('Exp1_R25_prot')
  r <- reactiveValues(
    settings = NULL
  )
  r$settings <- callModule(mod_settings_server, "settings")
  callModule(mod_plots_group_mv_server,'plots_group_mv', 
             obj = Exp1_R25_prot, 
             settings=reactive({r$settings()})
  )

}


shinyApp(ui, server)
