library(shiny)
library(highcharter)
library(SummarizedExperiment)


source(file.path('../../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path('../../../R/Plots', 'mod_plots_tracking.R'), local=TRUE)$value
source(file.path("../../../R/Plots","mod_plots_intensity.R"), local=TRUE)$value
source(file.path("../../../R","mod_popover_for_help.R"), local=TRUE)$value
source(file.path("../../../R","mod_observe_dynamic_colourPicker_input.R"), local=TRUE)$value


ui <- fluidPage(
  checkboxInput('sync', 'sync Slave with Master', value=FALSE),
  mod_plots_tracking_ui('master_tracking'),
  mod_plots_intensity_ui('plots_boxplots')
)



server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  keyId <- metadata(Exp1_R25_prot)[['keyId']]
  
  r <- reactiveValues(
    settings = NULL,
    master = NULL
  )
  
  r$settings <- callModule(mod_settings_server, "settings", obj=reactive({Exp1_R25_prot}))
  
                      
  
  metadata <- metadata(Exp1_R25_prot)
  conds <- colData(Exp1_R25_prot)[['Condition']]
  obj <- Exp1_R25_prot[[2]]
  SummarizedExperiment::rowData(obj) <- cbind(SummarizedExperiment::rowData(obj), 
                                              ProtOfInterest=rep(0,nrow(obj)))
  SummarizedExperiment::rowData(obj)$ProtOfInterest[10:20] <- 1
  
  r$master <- callModule(mod_plots_tracking_server,'master_tracking', 
                         obj = reactive({obj}),
                         keyId=reactive({keyId}),
                         params=reactive({NULL}),
                         reset=reactive({FALSE}),
                         slave = reactive({FALSE})
                        )
  
  callModule(mod_plots_intensity_server,'plots_boxplots', 
             dataIn = reactive({obj}),
             meta = reactive({metadata}),
             conds = reactive({conds}),
             base_palette = reactive({r$settings()$examplePalette}),
             params = reactive({if(input$sync) 
               r$master() else NULL
               }),
             reset = reactive({FALSE}),
             slave = reactive({input$sync})
             
  )
  
}


shinyApp(ui, server)
