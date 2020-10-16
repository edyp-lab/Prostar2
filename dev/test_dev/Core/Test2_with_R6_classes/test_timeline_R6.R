library(R6)



btn_style <- "display:inline-block; vertical-align: middle; padding: 7px"



options(shiny.fullstacktrace = T)
ui <- fluidPage(
  tagList(
    uiOutput("timeline")
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  source(file.path('.', 'private_methods.R'), local=TRUE)$value
  source(file.path('.', 'timeline_R6.R'), local=TRUE)$value
  source(file.path('.', 'timeline_Pipeline_R6.R'), local=TRUE)$value
  source(file.path('.', 'timeline_Process_R6.R'), local=TRUE)$value
  source(file.path('.', 'timeline_Style_R6.R'), local=TRUE)$value
  
  verbose <- T
  output$timeline <- renderUI({ timeline$ui() })
  
  rv <- reactiveValues(
    tl = NULL,
    wake = NULL
  )
  wake <- reactiveVal(3)
  config <- reactiveValues(
    type = 'process',
    process.name = 'Pipeline',
    steps = list(Description=T,
                 Step1 = F,
                 Step2 = T)
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
  
  
  #timeline <- Timeline$new('timeline', style=2)
  #timeline$call(config=config, wake = reactive({NULL}))
  
  timeline <- TimelineProcess$new('timeline', style=2)
  timeline$call(config=config, wake = wake)
 
  
observeEvent(input$testWake,{wake(input$testWake)})

  output$Description <- renderUI({
    tagList(
      h3('Description'),
    actionButton('Description_validate_btn', 'Validate')
    )
  })
  
  observeEvent(input$Description_validate_btn, {
    config$status[timeline$GetCurrentPosition()] <- VALIDATED
  })
  
  output$Step1 <- renderUI({
    tagList(
      h3('Step1'),
      selectInput('select_Step1', 'Select', choices=1:4),
      actionButton('Step1_validate_btn', 'Validate')
    )
  })
  
  observeEvent(input$Step1_validate_btn, {
    config$status[timeline$GetCurrentPosition()] <- VALIDATED
  })
  
  output$Step2 <- renderUI({
    tagList(
      h3('Step2'),
      selectInput('select_Step2', 'Select', choices=1:4),
      actionButton('Step2_validate_btn', 'Validate')
    )
  })
  
  observeEvent(input$Step2_validate_btn, {
    config$status[timeline$GetCurrentPosition()] <- VALIDATED
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
