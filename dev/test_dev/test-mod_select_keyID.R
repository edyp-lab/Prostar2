# for (f in list.files('../../R', pattern='.R')){
#   source(file.path('../../R', f), local=TRUE)$value
# }

library(shiny)

source(file.path('../../R/DataManager', 'mod_select_keyID.R'), local=TRUE)$value
source(file.path('../../R', 'mod_popover_for_help.R'), local=TRUE)$value

ui <- fluidPage(
  tagList(
    mod_select_keyID_ui('selectID'),
    uiOutput('state')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    IDs = NULL,
    dataIn = NULL
  )
  
  utils::data(Exp1_R25_pept, package='DAPARdata2')
  
  rv$IDs <- mod_select_keyID_server('selectID', 
                                    dataIn = reactive({SummarizedExperiment::rowData(Exp1_R25_pept[['original']])}),
                                    typeOfData = reactive({'peptide'}))
  
  #rv$IDs <- callModule(mod_select_keyID_server, 'selectID', dataIn=reactive({NULL}))
  
  observe({
    rv$dataIn <- rv$IDs()
  })
  
  output$state <- renderUI({
    req(rv$dataIn)
    tagList(
      p(paste0('keyId = ',rv$dataIn$keyId)),
      br(),
      p(paste0('parentProtId = ',rv$dataIn$parentProtId)),
      br(),
      p(paste0('nb lines in dataset = ', nrow(rv$dataIn$data)))
    )
  })
  
}


shinyApp(ui, server)
