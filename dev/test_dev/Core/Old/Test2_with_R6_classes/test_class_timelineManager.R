library(R6)
library(shiny)
source(file.path('.', 'class_TimelineForProcess.R'), local=TRUE)$value
source(file.path('.', 'class_abstract_TimelineManager.R'), local=TRUE)$value
source(file.path('.', 'class_TimelineDraw.R'), local=TRUE)$value


btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

options(shiny.fullstacktrace = T)

ui = function(){ 
  tagList(
    actionButton('valid', 'Simulate change on config'),
    uiOutput('show')
  )
  }


# Define server logic to summarize and view selected dataset ----
server = function(input, output, session) {
  
  config <- reactiveValues(
    process.name = 'foo',
    steps = LETTERS[1:3],
    mandatory = setNames(c(F,F,F), LETTERS[1:3]),
    status = setNames(c(0,0,0), LETTERS[1:3])
    )
  
  timelineManager <- TimelineForProcess$new(NS('App')('timeline'), 
                                            mandatory = config$mandatory)
  
  

  timelineManager$server(config = config,
                         wake = reactive({NULL}),
                         remoteReset = reactive({NULL})
                         )
  
  observeEvent(input$valid, {config$status[1] <- input$valid%%2 ==0})
  
  output$show <- renderUI({
    timelineManager$ui()
  })
  
  
  GetConfig <- reactive({config})
}


shinyApp(ui, server)