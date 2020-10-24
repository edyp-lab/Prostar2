library(shinyjs)


source(file.path('.', 'mod_timeline.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('.', 'mod_wf_wf1_Filtering_R6.R'), local=TRUE)$value
source(file.path('.', 'formal_funcs.R'), local=TRUE)$value
source(file.path('../../../../R', 'mod_insert_md.R'), local=TRUE)$value

options(shiny.fullstacktrace = T)

ui <- fluidPage(
  tagList(
    actionButton('testclic', 'Remote reset'),
uiOutput('show_process'))
  )


# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  source(file.path('.', 'debug_ui.R'), local=TRUE)$value
  source(file.path('.', 'private_methods.R'), local=TRUE)$value
  
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
rv <- reactiveValues(
  current.obj = Exp1_R25_prot,
  tmp = NULL,
  remoteReset = 0
)

dataOut <- reactiveValues(
  name = NULL,
  trigger = NULL,
  obj = NULL
)

  observeEvent(input$testclic, {rv$current.obj <- NULL})
  
  proc <- Process$new('Filtering')
  proc$call(dataIn = reactive({rv$current.obj}), 
            dataOut = dataOut,
            remoteReset = reactive({input$testclic}),
            isSkipped = reactive({input$testclic==1})
            )
  
  output$show_process <- renderUI({proc$ui()})
}


shinyApp(ui, server)

