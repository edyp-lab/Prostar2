
source(file.path('../../../../R', 'mod_navigation.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value

ui <- fluidPage(
  tagList(
    mod_wf_wf1_A_ui('mod_A_nav'),
    mod_wf_wf1_B_ui('mod_B_nav'),
    mod_wf_wf1_C_ui('mod_C_nav')
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
rv <- reactiveValues(
  current.obj = 'original'
)
  
  mod_wf_wf1_A_server("mod_A_nav", dataIn = reactive({rv$current.obj}) )
  mod_wf_wf1_B_server("mod_B_nav", dataIn = reactive({rv$current.obj}) )
  mod_wf_wf1_C_server("mod_C_nav", dataIn = reactive({rv$current.obj}) )
}


shinyApp(ui, server)
