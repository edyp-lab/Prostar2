library(shiny)
library(shinyjqui)
library(shinyBS)
library(DAPAR)

library(highcharter)
library(DT)
library(shinyjs)

source(file.path("../../R", "Drafts/mod_bsmodal.R"), local=TRUE)$value
source(file.path("../../R", "global.R"), local = TRUE)$value
source(file.path("../../R", "mod_format_DT.R"), local = TRUE)$value
source(file.path("../../R", "mod_settings.R"), local = TRUE)$value
source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path("../../R", "mod_plots_legend_colored_exprs.R"), local = TRUE)$value
source(file.path("../../R", "mod_plots_tracking.R"), local = TRUE)$value
source(file.path("../../R", "mod_plots_intensity.R"), local = TRUE)$value
source(file.path("../../R", "mod_plots_corr_matrix.R"), local = TRUE)$value
source(file.path("../../R", "mod_plots_heatmap.R"), local = TRUE)$value
source(file.path("../../R", "mod_plots_group_mv.R"),  local = TRUE)$value
source(file.path("../../R", "mod_plots_msnset_explorer.R"),  local = TRUE)$value
source(file.path("../../R", "mod_plots_var_dist.R"), local = TRUE)$value
source(file.path("../../R", "mod_plots_pca.R"), local = TRUE)$value
source(file.path("../../R", "mod_all_plots.R"), local=TRUE)$value

#### test modal ####
ui <- fluidPage(
  mod_bsmodal_ui('exemple')
)


server <- function(input, output, session) {

 
  require(DAPARdata)
  
  r <- reactiveValues(
    settings = NULL
  )

  r$settings <- callModule(mod_settings_server, "settings")
  
  datasets <- utils::data(package="DAPARdata")$results[,"Item"]
  data('Exp1_R25_pept')
  data('Exp1_R25_prot')
  obj <- Exp1_R25_pept
  samples <- Biobase::pData(obj)
  mae <- PipelineProtein(analysis= 'test',
                         pipelineType = 'peptide',
                         dataType = 'peptide',
                         processes='original',
                         experiments=list(original=Exp1_R25_prot,
                                          test = Exp1_R25_pept),
                         colData=samples)

  
  callModule(mod_all_plots_server,'exemple_plot',
             dataIn = reactive({mae}),
             settings = reactive({r$settings()})
  ) 
  
  mod_UI <- mod_all_plots_ui('exemple_plot')
  title <- "Exemple"
  
  callModule(mod_bsmodal_server,'exemple',
             title = title,
             mod_UI = mod_UI
             ,width="95%" # en px ou en % de largeur
  ) 
  
}

shinyApp(ui=ui, server=server)
