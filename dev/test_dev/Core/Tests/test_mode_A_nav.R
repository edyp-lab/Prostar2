
source(file.path('../../../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_A.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_B.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_C.R'), local=TRUE)$value


ui <- fluidPage(
  tagList(
    actionButton('testclic', 'test clic'),
    mod_wf_wf1_A_ui('mod_A_nav')
    #mod_wf_wf1_B_ui('mod_B_nav'),
    #mod_wf_wf1_C_ui('mod_C_nav')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  utils::data(Exp1_R25_prot, package='DAPARdata2')
rv <- reactiveValues(
  current.obj = Exp1_R25_prot[1:10,,],
  tmp = NULL,
  remoteReset = 0
)
  observeEvent(input$testclic, { 
    print(paste0('new value for testclic = ', input$testclic))
    rv$remoteReset <-input$tesclic})
  
  rv$tmp <- mod_wf_wf1_A_server("mod_A_nav", 
                                dataIn = reactive({rv$current.obj}), 
                                remoteReset = reactive({input$testclic}))
  #mod_wf_wf1_B_server("mod_B_nav", dataIn = reactive({rv$current.obj}) )
  #mod_wf_wf1_C_server("mod_C_nav", dataIn = reactive({rv$current.obj}) )
  
  observeEvent(rv$tmp(),{rv$current.obj <- rv$tmp()  })
}


shinyApp(ui, server)
