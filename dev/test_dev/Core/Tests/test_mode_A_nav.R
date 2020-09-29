library(shinyjs)


source(file.path('.', 'mod_timeline.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_A.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_B.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_C.R'), local=TRUE)$value


options(shiny.fullstacktrace = F)

ui <- fluidPage(
  tagList(
    actionButton('testclic', 'Remote reset'),
    mod_wf_wf1_A_ui('mod_A_nav')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
rv <- reactiveValues(
  current.obj = Exp1_R25_prot[1:10,,],
  tmp = NULL,
  remoteReset = 0,
  dataOut = NULL
)

  observeEvent(input$testclic, {rv$remoteReset <- input$tesclic})
  
  rv$tmpA <- mod_wf_wf1_A_server("mod_A_nav", 
                                dataIn = rv$current.obj, 
                                remoteReset = NULL,
                                forcePosition = FALSE
                                )
  
  observeEvent(rv$tmpA$dataOut(),{print('toto')
    isolate({
      rv$dataOut <- rv$tmpA$dataOut()
    })
    
    })
}


shinyApp(ui, server)

