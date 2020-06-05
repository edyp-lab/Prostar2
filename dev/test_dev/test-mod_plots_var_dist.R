library(highcharter)
library(shinycssloaders)
library(shiny)
library(SummarizedExperiment)
library(DAPAR2)

source(file.path("../../R","mod_plots_var_dist.R"), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path("../../R","mod_popover_for_help.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_var_dist_ui('varDistPlot')
)



server <- function(input, output, session) {
  
  
  library(DAPARdata2)
  data("Exp1_R25_prot")
  obj <- Exp1_R25_prot[[2]][1:1000,]
  conds <- SummarizedExperiment::colData(Exp1_R25_prot)[["Condition"]]
  r <- reactiveValues(
    settings = NULL
  )
  r$settings <- callModule(mod_settings_server, "settings")
  
  
  callModule(mod_plots_var_dist_server,'varDistPlot', 
             obj = reactive({obj}),
             conds = reactive({conds}),
             base_palette = reactive({r$settings()$examplePalette}))
}


shinyApp(ui=ui, server=server)
