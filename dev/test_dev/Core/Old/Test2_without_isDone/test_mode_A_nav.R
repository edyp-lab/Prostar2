library(shinyjs)


source(file.path('.', 'mod_timeline.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('.', 'mod_wf_wf1_Filtering.R'), local=TRUE)$value
source(file.path('.', 'formal_funcs.R'), local=TRUE)$value
source(file.path('../../../../R', 'mod_insert_md.R'), local=TRUE)$value

options(shiny.fullstacktrace = T)

ui <- fluidPage(
  tagList(
    actionButton('testclic', 'Remote reset'),

    
    mod_wf_wf1_Filtering_ui('mod_Filtering_nav')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  source(file.path('.', 'debug_ui.R'), local=TRUE)$value
  source(file.path('.', 'code_general.R'), local=TRUE)$value
  
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
rv <- reactiveValues(
  current.obj = Exp1_R25_prot,
  tmp = NULL,
  remoteReset = 0
)


  observeEvent(input$testclic, {rv$current.obj <- NULL})
  
  mod_wf_wf1_Filtering_server(id = "mod_Filtering_nav", 
                                dataIn = reactive({rv$current.obj}), 
                                remoteReset = reactive({input$testclic}),
                                isSkipped = reactive({input$testclic==1})
                                )
  
  #observeEvent(rv$tmp$dataOut(),{rv$current.obj <- rv$tmp$dataOu()  })
}


shinyApp(ui, server)

