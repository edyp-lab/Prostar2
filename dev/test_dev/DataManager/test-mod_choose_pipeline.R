source(file.path('../../../R/DataManager', 'mod_choose_pipeline.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    #uiOutputselectInput('dataType', 'DataType', choices = c('protein', 'peptide'), selected=character(0)),
    mod_choose_pipeline_ui('pipe')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    res = NULL
   )
  
  rv$res <- mod_choose_pipeline_server('pipe', 
                                       dataType = reactive({'protein'}), 
                                       package = 'MSPipelines')

}


shinyApp(ui, server)
