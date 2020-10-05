library(highcharter)
library(DAPAR2)

source(file.path("../../R","mod_popover_for_help.R"), local=TRUE)$value
source(file.path("../../R/Plots","mod_plots_group_mv.R"), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_observe_dynamic_colourPicker_input.R'), local=TRUE)$value

ui <- fluidPage(
  tagList(
    mod_settings_ui('settings'),
    mod_plots_group_mv_ui('plots_group_mv')
  )
)



server <- function(input, output, session) {
  
  r <- reactiveValues(
    settings = NULL
  )
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  r$settings <- mod_settings_server("settings",
                                    obj = reactive({Exp1_R25_prot}))
  
  mod_plots_group_mv_server('plots_group_mv', 
                            obj = reactive({Exp1_R25_prot}),
                            conds = reactive({colData(Exp1_R25_prot)}),
                            base_palette=reactive({r$settings()$examplePalette})
  )
  # callModule(mod_plots_group_mv_server,'plots_group_mv', obj = NULL)
  # callModule(mod_plots_group_mv_server,'plots_group_mv', obj = mae)
  
}


shinyApp(ui, server)
