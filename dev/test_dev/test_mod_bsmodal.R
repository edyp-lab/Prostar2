library(shiny)
library(shinyjqui)
library(shinyBS)
library(DAPAR2)

source(file.path("../../R", "mod_bsmodal.R"), local=TRUE)$value


library(highcharter)
library(DT)
library(shinyjs)

source(file.path("../../R", "mod_settings.R"), local = TRUE)$value
source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path("../../R", "global.R"), local = TRUE)$value
source(file.path("../../R", "mod_observe_dynamic_colourPicker_input.R"), local=TRUE)$value
source(file.path("../../R", "mod_format_DT.R"), local = TRUE)$value
source(file.path("../../R/Plots", "mod_all_plots.R"), local=TRUE)$value
source(file.path("../../R/Plots", "mod_plots_intensity.R"), local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_tracking.R"), local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_legend_colored_exprs.R"), local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_corr_matrix.R"), local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_heatmap.R"), local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_group_mv.R"),  local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_se_explorer.R"),  local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_var_dist.R"), local = TRUE)$value
source(file.path("../../R/Plots", "mod_plots_pca.R"), local = TRUE)$value

#### test modal ####
ui <- fluidPage(
  mod_bsmodal_ui('exemple')
)


server <- function(input, output, session) {
  
  # dans body de modal
  r <- reactiveValues(
    settings = NULL
  )
  
  
  #datasets <- utils::data(package="DAPARdata2")$results[,"Item"]
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  r$settings <- mod_settings_server("settings", obj=reactive({Exp1_R25_prot}))
  
  mod_all_plots_server('exemple_plot',
                       dataIn = reactive({Exp1_R25_prot}),
                       indice = reactive({2}),
                       settings = reactive({r$settings()}) ) 
  
  
  
  mod_UI <- mod_all_plots_ui('exemple_plot')
  title <- "Plots"
  
  # module d'affichage modal contenant ci-dessus
  mod_bsmodal_server('exemple',
                     title = title,
                     mod_UI = mod_UI,
                     width="75%" # en px ou % de largeur
  )
}

shinyApp(ui=ui, server=server)
