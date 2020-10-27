library(R6)

source(file.path('.', 'class_TimelineForProcess.R'), local=TRUE)$value
source(file.path('.', 'class_TimelineStyle.R'), local=TRUE)$value
source(file.path('.', 'class_TimelineManager.R'), local=TRUE)$value


btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"

#timelineManager <- TimelineForProcess$new(NS('App')('timeline'), style=2)
timelineManager <- TimelineManager$new(NS('App')('Filtering'), style=2)

options(shiny.fullstacktrace = T)

ui = function(){ timelineManager$ui()}


# Define server logic to summarize and view selected dataset ----
server = function(input, output, session) {
  source(file.path('.', 'private_methods.R'), local=TRUE)$value
  
  verbose <- T
  
  rv <- reactiveValues(
    tl = NULL,
    wake = NULL
  )
  
  wake <- reactiveVal(3)
  
  config <- reactiveValues(
    type = 'process',
    process.name = 'Pipeline',
    steps = list(Description=T, Step1 = F, Step2 = T),
    screens = NULL,
    status = NULL
  )
  
  
  observe({
    req(config)
    Initialize_Status_Process()
    rv$screens <- InitScreens(nbSteps())
    # Must be placed after the initialisation of the 'config$stepsNames' variable
    config$screens <- setNames(
      lapply(1:length(names(config$steps)), 
             function(x){
               do.call(uiOutput, list(outputId=names(config$steps)[x]))}),
      paste0('screen_', names(config$steps)))
    
})
  

  
  timelineManager$server(config = config, 
                         wake = wake)
 
 # output$timeline <- renderUI({ timelineManager$ui() })
  
observeEvent(input$testWake,{wake(input$testWake)})

  output$Description <- renderUI({
    tagList(
      h3('Description'),
    actionButton('Description_validate_btn', 'Validate')
    )
  })
  
  observeEvent(input$Description_validate_btn, {
    config$status[timelineManager$GetCurrentPosition()] <- VALIDATED
  })
  
  output$Step1 <- renderUI({
    tagList(
      h3('Step1'),
      selectInput('select_Step1', 'Select', choices=1:4),
      actionButton('Step1_validate_btn', 'Validate')
    )
  })
  
  observeEvent(input$Step1_validate_btn, {
    config$status[timelineManager$GetCurrentPosition()] <- VALIDATED
  })
  
  output$Step2 <- renderUI({
    tagList(
      h3('Step2'),
      selectInput('select_Step2', 'Select', choices=1:4),
      actionButton('Step2_validate_btn', 'Validate')
    )
  })
  
  observeEvent(input$Step2_validate_btn, {
    config$status[timelineManager$GetCurrentPosition()] <- VALIDATED
  })
  
  
  
  #######################################################################
  Initialize_Status_Process <- function(){
   # browser()
    req(config)
    if(verbose)
      print(paste0(config$process.name, ' : Initialize_Status_Process() : '))
    
    config$status <- setNames(rep(UNDONE,length(config$steps)),names(config$steps))
  }
  
  VALIDATED <- 1
  UNDONE <- 0
  SKIPPED <- -1
  RESETED <- 2
  
  verbose <- T
  
  
  InitScreens <- function(n){
    setNames(lapply(1:n,
                    function(x){T}),
             paste0('screen', 1:n)
    )
  }
  

  
}


shinyApp(ui, server)
