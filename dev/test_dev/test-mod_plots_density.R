library(highcharter)
library(DAPAR)


source(file.path("../../R","mod_plots_density.R"), local=TRUE)$value
source(file.path("../../R","mod_settings.R"), local=TRUE)$value
source(file.path("../../R","mod_popover_for_help.R"), local=TRUE)$value

ui <- fluidPage(
  mod_plots_density_ui('plots_density')
)



server <- function(input, output, session) {
  
  
  library(DAPARdata)
  data("Exp1_R25_prot")
  
  r <- reactiveValues(
    settings = NULL
  )
  r$settings <- callModule(mod_settings_server, "settings")
  
  
  # obj est un msnset
  callModule(mod_plots_density_server,'plots_density', 
             obj = reactive({Exp1_R25_prot}),
             base_palette = reactive({r$settings()$examplePalette}))
}


shinyApp(ui=ui, server=server)
