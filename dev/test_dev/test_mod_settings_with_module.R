library(highcharter)
library(DAPAR)

source(file.path("../../R","mod_popover_for_help.R"), local=TRUE)$value
source(file.path("../../R","mod_plots_group_mv.R"), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value

ui <- fluidPage(
  tagList(
    #mod_settings_ui('settings'),
    mod_plots_group_mv_ui('plots_group_mv')
  )
)



server <- function(input, output, session) {
  
  r <- reactiveValues(
    settings = NULL
  )
  require(DAPARdata)
  data('Exp1_R25_prot')
  
   r$settings <- callModule(mod_settings_server, "settings")
  callModule(mod_plots_group_mv_server,'plots_group_mv', 
             obj = reactive({Exp1_R25_prot}), 
             base_palette=reactive({r$settings()$examplePalette})
             )
  # callModule(mod_plots_group_mv_server,'plots_group_mv', obj = NULL)
  # callModule(mod_plots_group_mv_server,'plots_group_mv', obj = mae)
  
}


shinyApp(ui, server)