
library(shiny)
library(shinyBS)
library(shinyjqui)
library(highcharter)
library(DAPAR)
library(DT)
library(shinyjs)

source(file.path(".","module_plots.R"), local=TRUE)$value
source(file.path("..", "mod_settings.R"), local = TRUE)$value
source(file.path("..", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path("..", "mod_plots_intensity.R"), local = TRUE)$value
source(file.path("..", "mod_plots_tracking.R"), local = TRUE)$value
source(file.path("..", "mod_plots_legend_colored_exprs.R"), local = TRUE)$value
source(file.path("..", "mod_plots_corr_matrix.R"), local = TRUE)$value
source(file.path("..", "mod_plots_heatmap.R"), local = TRUE)$value
source(file.path("..", "mod_plots_group_mv.R"),  local = TRUE)$value
source(file.path("..", "mod_plots_msnset_explorer.R"),  local = TRUE)$value
source(file.path("..", "mod_plots_var_dist.R"), local = TRUE)$value
source(file.path("..", "global.R"), local = TRUE)$value



ui <- fluidPage(
  module_plots_ui('plots')
  #mod_settings_ui('settings')
)


server <- function(input, output, session) {
  
  require(DAPARdata)
  data('Exp1_R25_prot')
  
  r <- reactiveValues(
    settings = NULL
  )
  
  r$settings <- callModule(mod_settings_server, "settings")
  
  callModule(module_plots_server,'plots',
             dataIn = reactive({Exp1_R25_prot}),
             llPlots = reactive({c("intensity", "pca", "var_dist",  "heatmap", "mv", "quanti","corr_matrix", "quanti")}),
             base_palette = reactive({r$settings()$examplePalette})
             ) 

}


shinyApp(ui, server)
