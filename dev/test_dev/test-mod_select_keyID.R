for (f in list.files('../../R', pattern='.R')){
  source(file.path('../../R', f), local=TRUE)$value
}

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
  
  require(DAPARdata)
  data('Exp1_R25_pept')
 
  rv$IDs <- callModule(mod_select_keyID_server, 'selectID', dataIn=reactive({Exp1_R25_pept}))
  
  observe({
    rv$dataIn <- rv$IDs()
  })
  
  output$state <- renderUI({
    req(rv$dataIn)
    print(str(rv$dataIn))
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
