library(highcharter)
library(DAPAR2)
library(shiny)
library(SummarizedExperiment)

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
  
  
  require(DAPARdata2)
  data('Exp1_R25_pept')
  metadata <- metadata(Exp1_R25_pept)
  conds <- colData(Exp1_R25_pept)[['Conditon']]
  obj <- Exp1_R25_pept[[2]]
  SummarizedExperiment::rowData(obj) <- cbind(SummarizedExperiment::rowData(obj), ProtOfInterest=rep(0,nrow(obj)))
  SummarizedExperiment::rowData(obj)$ProtOfInterest[10:20] <- 1
  
  callModule(mod_plots_intensity_server,'plots_boxplots', 
             dataIn = reactive({obj}),
             metadata = reactive({metadata}),
             conds = reactive({conds}),
             params = reactive({NULL}),
             reset = reactive({FALSE}),
             base_palette = reactive({r$settings()$examplePalette})
  )
  
}


shinyApp(ui, server)
