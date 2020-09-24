source(file.path('../../../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_A.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_B.R'), local=TRUE)$value
source(file.path('../Workflows/wf1', 'mod_wf_wf1_C.R'), local=TRUE)$value
source(file.path('.', 'mod_super_timeline.R'), local=TRUE)$value

options(shiny.fullstacktrace = T)

ui <- fluidPage(
  tagList(
    mod_super_timeline_ui("super_nav"),
    uiOutput('obj')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    current.obj = list(original = 0),
    tmp = NULL
  )
  

  rv$tmp <- mod_super_timeline_server("super_nav", 
                                   dataIn = reactive({rv$current.obj}))

}


shinyApp(ui, server)
