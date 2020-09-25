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
    wellPanel(
      p('rv$current.obj :'),
      verbatimTextOutput('obj'),
      p('rv$tmp()'),
      verbatimTextOutput('tmp')
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  rv <- reactiveValues(
    current.obj = Exp1_R25_prot[1:10,,],
    tmp = NULL
  )
  
  
  output$obj <- renderPrint({rv$current.obj})
  output$tmp <- renderPrint({rv$tmp()})
  
  rv$tmp <- mod_super_timeline_server("super_nav", dataIn = reactive({rv$current.obj}))

}


shinyApp(ui, server)
