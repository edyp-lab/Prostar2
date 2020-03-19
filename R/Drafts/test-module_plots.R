setwd("~/TELETRAVAIL/github_2.0/Prostar2/R/Drafts/")


library(shiny)
library(shinyBS)
library(shinyjqui)
library(highcharter)
library(DAPAR)
library(DT)


source(file.path(".","module_plots.R"), local=TRUE)$value
source(file.path("..", "mod_plots_boxplots.R"), local = TRUE)$value
source(file.path("..", "mod_plots_legend_colored_exprs.R"), local = TRUE)$value
source(file.path("..", "mod_plots_corr_matrix.R"), local = TRUE)$value
source(file.path("..", "mod_plots_heatmap.R"), local = TRUE)$value
source(file.path("..", "mod_plots_intensity.R"), local = TRUE)$value
source(file.path("..", "mod_plots_group_mv.R"),  local = TRUE)$value
source(file.path("..", "mod_plots_msnset_explorer.R"),  local = TRUE)$value
source(file.path("..", "mod_plots_var_dist.R"), local = TRUE)$value
source(file.path("../", "global.R"), local = TRUE)$value



ui <- fluidPage(
  modulePlotsUI('plots')
)


server <- function(input, output, session) {
  
  require(DAPARdata)
  data('Exp1_R25_prot')
  
  callModule(modulePlots,'plots',
             dataIn = Exp1_R25_prot,
             llPlots = reactive({c("intensity", "pca", "varDist", "corrMatrix", "heatmap", "mv", "quantiTable")})
             ) 

}


shinyApp(ui, server)
