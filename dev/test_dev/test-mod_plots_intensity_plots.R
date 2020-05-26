library(highcharter)
library(DAPAR)

source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path('../../R', 'mod_plots_tracking.R'), local=TRUE)$value
source(file.path("../../R","mod_plots_intensity.R"), local=TRUE)$value
source(file.path("../../R","mod_popover_for_help.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_intensity_ui('plots_boxplots')
)



server <- function(input, output, session) {
  
  r <- reactiveValues(
    settings = NULL
  )
  r$settings <- callModule(mod_settings_server, "settings")
  
  
  require(DAPARdata)
  data('Exp1_R25_pept')
  obj <- Exp1_R25_pept
  Biobase::fData(obj) <- cbind(Biobase::fData(obj), ProtOfInterest=rep(0,nrow(obj)))
  Biobase::fData(obj)$ProtOfInterest[10:20] <- 1
  
  callModule(mod_plots_intensity_server,'plots_boxplots', 
             dataIn = reactive({obj}),
             params = reactive({NULL}),
             reset = reactive({FALSE}),
             base_palette = reactive({r$settings()$examplePalette})
  )
  
}


shinyApp(ui, server)