library(highcharter)
library(DAPAR2)
library(shiny)
library(SummarizedExperiment)


source(file.path("../../R","mod_plots_density.R"), local=TRUE)$value
source(file.path("../../R","mod_settings.R"), local=TRUE)$value
source(file.path("../../R","mod_popover_for_help.R"), local=TRUE)$value

ui <- fluidPage(
  mod_plots_density_ui('plots_density')
)



server <- function(input, output, session) {
  
  
  library(DAPARdata2)
  data("Exp1_R25_prot")
  obj <- Exp1_R25_prot[[2]]
  
  r <- reactiveValues(
    settings = NULL
  )
  r$settings <- callModule(mod_settings_server, "settings")
  
  # obj est un msnset
  callModule(mod_plots_density_server,'plots_density', 
             obj = reactive({obj}),
             base_palette = reactive({r$settings()$examplePalette}))
}


shinyApp(ui=ui, server=server)
