source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('../../../../R', 'config.R'), local=TRUE)$value
source(file.path('.', 'mod_timeline.R'), local=TRUE)$value

options(shiny.fullstacktrace = T)
ui <- fluidPage(
  tagList(
    mod_timeline_ui("timeline")
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {

  
  
  rv <- reactiveValues(
    tl = NULL
  )
  config <- reactiveValues(
    type = 'pipeline',
    process.name = 'Pipeline',
    steps = append(list(Original = T), pipeline.defs$protein )
  )
  

  
  rv$tl <- mod_timeline_server("timeline", 
                      style = 2, 
                      config = config, 
                      wake = reactive({0})
  )
}


shinyApp(ui, server)
