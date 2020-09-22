library(shiny)
library(highcharter)

# library(shinyBS)
# library(shinyjqui)
# 
# library(DAPAR2)
# library(DT)
# library(shinyjs)

source(file.path("../../../R/Plots", "mod_all_plots.R"), local=TRUE)$value
source(file.path("../../../R", "mod_settings.R"), local = TRUE)$value
source(file.path("../../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path("../../../R/Plots", "mod_plots_intensity.R"), local = TRUE)$value
source(file.path("../../../R/Plots", "mod_plots_tracking.R"), local = TRUE)$value
source(file.path("../../../R/Plots", "mod_plots_legend_colored_exprs.R"), local = TRUE)$value
source(file.path("../../../R/Plots", "mod_plots_corr_matrix.R"), local = TRUE)$value
source(file.path("../../../R/Plots", "mod_plots_heatmap.R"), local = TRUE)$value
source(file.path("../../../R/Plots", "mod_plots_group_mv.R"),  local = TRUE)$value
source(file.path("../../../R/Plots", "mod_plots_se_explorer.R"),  local = TRUE)$value
source(file.path("../../../R/Plots", "mod_plots_var_dist.R"), local = TRUE)$value
source(file.path("../../../R", "global.R"), local = TRUE)$value
source(file.path("../../../R", "mod_format_DT.R"), local = TRUE)$value
source(file.path("../../../R/Plots", "mod_plots_pca.R"), local = TRUE)$value
source(file.path('../../../R', 'mod_observe_dynamic_colourPicker_input.R'), local=TRUE)$value


ui <- fluidPage(
  mod_all_plots_ui('plots')
)


server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  r <- reactiveValues(
    settings = NULL
  )
  
  r$settings <- callModule(mod_settings_server, "settings", obj=reactive({Exp1_R25_prot}))
  
  obj <- QFeatures::addAssay(Exp1_R25_prot, (QFeatures::filterNA(Exp1_R25_prot,i=2))[[2]], "original_log_NAfiltered")
  
  callModule(mod_all_plots_server,'plots',
             dataIn = reactive({obj}),
             indice = reactive({2}),
             settings = reactive({r$settings()})
  ) 
  
}


shinyApp(ui, server)
