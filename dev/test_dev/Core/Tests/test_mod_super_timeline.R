library(shinyjs)

source(file.path('.', 'mod_tl_engine.R'), local=TRUE)$value
source(file.path('.', 'mod_timeline.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_A.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_B.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_C.R'), local=TRUE)$value

options(shiny.fullstacktrace = F)

ui <- fluidPage(
  tagList(
    mod_super_timeline_ui("super_nav")
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
  
  rv$tmp <- mod_super_timeline_server("super_nav", 
                                dataIn = reactive({rv$current.obj}) )
  
  observeEvent(rv$tmp(),{rv$current.obj <- rv$tmp()  })

}


shinyApp(ui, server)
