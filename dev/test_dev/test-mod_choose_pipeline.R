source(file.path('../../R', 'config.R'), local=TRUE)$value
source(file.path('../../R', 'mod_choose_pipeline.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    selectInput('dataType', 'DataType', choices = c('protein', 'peptide'), selected=character(0)),
    mod_choose_pipeline_ui('pipe'),
    verbatimTextOutput('showPipeline')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  #defs <- ReadPipelineConfig('../../R/pipeline.conf')
  
  rv <- reactiveValues(
    res = NULL
   )
  
  rv$res <- callModule(mod_choose_pipeline_server,'pipe', pipeline.def=reactive({pipeline.defs}))
  
  output$showPipeline <- renderText({
    req(rv$res())
    paste0(unlist(rv$res()),"\n")
  })
  
}


shinyApp(ui, server)
