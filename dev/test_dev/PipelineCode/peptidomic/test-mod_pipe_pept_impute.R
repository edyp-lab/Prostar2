source(file.path('../../R', 'config.R'), local=TRUE)$value
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path('../../R', 'mod_pipe_pept_impute.R'), local=TRUE)$value
source(file.path("../../R", "mod_plots_mv_for_imputation.R"), local=TRUE)$value
source(file.path('../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../R', 'mod_settings.R'), local=TRUE)$value
source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path('../../R', 'mod_format_DT.R'), local=TRUE)$value
source(file.path('../../R', 'mod_infos_dataset.R'), local=TRUE)$value
source(file.path('../../R', 'mod_observe_dynamic_colourPicker_input.R'), local=TRUE)$value
source(file.path('../../R', 'mod_det_quant_impute_Values.R'), local=TRUE)$value



library(highcharter)
library(shinyjs)
library(DAPAR2)
library(DT)
library(tibble)
library(QFeatures)

options(shiny.fullstacktrace = FALSE)


ui <- fluidPage(
  tagList(
    mod_pipe_pept_impute_ui('pipe_impute')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_pept, package='DAPARdata2')
  
  
  rv <- reactiveValues(
    dataIn = Exp1_R25_pept
  )
  
  toto <- reactive({
    
    isolate({
      rv$dataIn <- MVrowsTagToOne(object =rv$dataIn, 
                                       type = 'EmptyLines', 
                                       percent = FALSE)
    
    ## keep rows where tagNA==0
    na_filter <- VariableFilter(field = "tagNA", value = "0", condition = "==")
    
    rv$dataIn <- DAPAR2::filterFeaturesSam(object = rv$dataIn, 
                                                  i = length(names(rv$dataIn)), 
                                                  name = 'na_filter_', 
                                                  filter=na_filter)
    
    rv$dataIn <- removeAdditionalCol(rv$dataIn, "tagNA")
    
    })
    rv$dataIn
  })
  
  
  
  callModule(mod_pipe_pept_impute_server,
                       'pipe_impute',
                       obj = reactive({toto()})
  )
  
  
}


shinyApp(ui, server)
