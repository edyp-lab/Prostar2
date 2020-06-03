library(shiny)
library(highcharter)
library(SummarizedExperiment)
library(DAPAR2)


source(file.path("../../R", "mod_plots_group_mv.R"), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path('../../R', 'mod_popover_for_help.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value

ui <- fluidPage(
  mod_plots_group_mv_ui('plots_group_mv'),
  mod_settings_ui('settings')
)



server <- function(input, output, session) {
  
  require(DAPARdata2)
  data('Exp1_R25_prot')
  obj <- Exp1_R25_prot[[2]]
  r <- reactiveValues(
    settings = NULL
  )
  conds <- SummarizedExperiment::colData(Exp1_R25_prot)
  r$settings <- callModule(mod_settings_server, "settings")
  callModule(mod_plots_group_mv_server,'plots_group_mv', 
             obj = reactive({obj}),
             conds = reactive({conds}),
             base_palette=reactive({r$settings()$examplePalette})
  )

}


shinyApp(ui, server)
