library(shinyjs)

source(file.path('.', 'mod_tl_engine.R'), local=TRUE)$value
source(file.path('.', 'mod_timeline.R'), local=TRUE)$value
source(file.path('.', 'mod_super_timeline.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_A.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_B.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_C.R'), local=TRUE)$value

options(shiny.fullstacktrace = F)
options(shiny.reactlog=TRUE) 

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
    tmp = NULL
  )
  
  rv$tmp <- mod_super_timeline_server("super_nav", 
                                dataIn = reactive({rv$current.obj}) )
  
  observeEvent(rv$tmp(), {
    print('TEST SUPER_TIMELINE : retour du module mod_super_timeline_server')
    rv$current.obj <- rv$tmp()
    print(paste0("      names(dataIn()) = ", paste0(names(rv$current.obj), collapse=' - ')))
    })

}


shinyApp(ui, server)
