library(R6)


#source(file.path('../../../../R', 'global.R'), local=TRUE)$value
source(file.path('.', 'timeline_R6.R'), local=TRUE)$value

btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

options(shiny.fullstacktrace = T)
ui <- fluidPage(
  tagList(
    uiOutput("timeline")
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {

  
  output$timeline <- renderUI({
    timeline$ui()
  })
  
  rv <- reactiveValues(
    tl = NULL
  )
  config <- reactiveValues(
    type = 'pipeline',
    process.name = 'Pipeline',
    steps = append(list(Original = T), pipeline.defs$protein )
  )
  

  
  timeline <- Timeline$new('timeline', style=2)
  timeline$call(config=config, wake = reactive({NULL}))

}


shinyApp(ui, server)
