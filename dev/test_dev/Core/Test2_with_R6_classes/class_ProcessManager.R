source(file.path('.', 'class_TimelineManager.R'), local=TRUE)$value

ProcessManager <- R6Class(
  "ProcessManager",
  private = list(
    timelineManager = NULL
  ),
  public = list(id = NULL,
                length = 3,
                config = reactiveValues(
                  steps = tibble(
                    name = c("Description", "Step1", "Step2"),
                    mandatory = c(T, F, T),
                    status = c(0, 0, 0)),
                  screens = list()
                  ),
                
                
                rv = reactiveValues(
                  dataIn = NULL,
                  current.pos = 1,
                  selectData = 0),
                
                
                res = reactiveValues(
                  name = NULL,
                  obj = NULL,
                  trigger = NULL
                ),
                
                initialize = function(id) {
                  self$id <- id
                  #
                  
                  
                },
                
                ui = function() {
                  ns <- NS(self$id)
                  fluidPage(
                    wellPanel(
                      tagList(h3('ProcessManager'),
                              uiOutput(ns("select")),
                              actionButton(ns("dataOut_btn"), "Simulate validate (change in dataOut)"),
                              actionButton(ns("pos_btn"), "Simulate change in pos"),
                              hr(),
                              uiOutput(ns('show_timeline_ui')),
                              uiOutput(ns('show_dataIn'))
                      )
                    )
                  )
                },
  Initialize_Status_Process = function(){
    self$config$steps$status <- setNames(rep(UNDONE,length(self$config$steps$status)),
                                                       names(self$config$steps$status))
    },
  Wake = function(){ runif(1,0,1)},
  InitScreens = function(n){
    setNames(lapply(1:n,
                    function(x){T}),
             paste0('screen', 1:n)
    )
  },
  server = function(dataIn, dataOut) {
                  ns <- NS(self$id)
                  
                  source(file.path('.', 'private_methods.R'), local=TRUE)$value
                  browser()
                  private$timelineManager <- TimelineManager$new(NS(self$id)('timeline'),
                                                                 steps = reactive({self$config$steps$mandatory}))
                 # observe({
                   
                 #   self$Initialize_Status_Process()
                 #   self$config$screens <- InitScreens(self$length)
                    # Must be placed after the initialisation of the 'config$stepsNames' variable
                 #   self$config$screens <- setNames(
                 #     lapply(1:length(names(self$config$steps)), 
                 #            function(x){
                 #              do.call(uiOutput, list(outputId=names(self$config$steps)[x]))}),
                 #     paste0('screen_', names(self$config$steps)))
                 # })
                  
                  
                  
                  
                  
                    
                  current.pos = reactiveVal()
                  res <- private$timelineManager$server(config = reactive({self$config}),
                                                        wake = reactive({self$Wake()}))
                  observeEvent(dataIn(), {self$rv$dataIn <- dataIn })
                  
                  ######
                  ###
                  ###
                  moduleServer(self$id, function(input, output, session) {
                    
                  
                    output$show_timeline_ui <- renderUI({
                      private$timelineManager$ui()
                    })
                    
                    output$Description <- renderUI({
                      tagList(
                        h3('Description'),
                        actionButton('Description_validate_btn', 'Validate')
                      )
                    })
                    
                    observeEvent(input$Description_validate_btn, {
                      self$config$status[timelineManager$GetCurrentPosition()] <- VALIDATED
                    })
                    
                    output$Step1 <- renderUI({
                      tagList(
                        h3('Step1'),
                        selectInput('select_Step1', 'Select', choices=1:4),
                        actionButton('Step1_validate_btn', 'Validate')
                      )
                    })
                    
                    observeEvent(input$Step1_validate_btn, {
                      self$config$status[timelineManager$GetCurrentPosition()] <- VALIDATED
                    })
                    
                    output$Step2 <- renderUI({
                      tagList(
                        h3('Step2'),
                        selectInput('select_Step2', 'Select', choices=1:4),
                        actionButton('Step2_validate_btn', 'Validate')
                      )
                    })
                    
                    observeEvent(input$Step2_validate_btn, {
                      self$config$status[timelineManager$GetCurrentPosition()] <- VALIDATED
                    })
                    
                           observeEvent(input$testWake,{wake(input$testWake)})
                    
                    output$Description <- renderUI({
                      tagList(
                        h3('Description'),
                        actionButton('Description_validate_btn', 'Validate')
                      )
                    })
                    
                    observeEvent(input$Description_validate_btn, {
                      self$config$status[timelineManager$GetCurrentPosition()] <- VALIDATED
                    })
                    
                    output$Step1 <- renderUI({
                      tagList(
                        h3('Step1'),
                        selectInput('select_Step1', 'Select', choices=1:4),
                        actionButton('Step1_validate_btn', 'Validate')
                      )
                    })
                    
                    observeEvent(input$Step1_validate_btn, {
                      self$config$status[timelineManager$GetCurrentPosition()] <- VALIDATED
                    })
                    
                    output$Step2 <- renderUI({
                      tagList(
                        h3('Step2'),
                        selectInput('select_Step2', 'Select', choices=1:4),
                        actionButton('Step2_validate_btn', 'Validate')
                      )
                    })
                    
                    observeEvent(input$Step2_validate_btn, {
                      self$config$status[timelineManager$GetCurrentPosition()] <- VALIDATED
                    })

                    output$select <- renderUI({selectInput(ns('selectData'), 'Mother : Select data', 1:4) })
                    
                    output$show_dataIn <- renderUI({paste0('received dataIn = ', dataIn())})
                    
                    observeEvent(dataOut$trigger,{print(paste0('receive new dataOut : ', paste0(lapply(reactiveValuesToList(dataOut),function(x){ x}), collapse=' ')))})
                    
                    observeEvent(input$selectData,{self$rv$data <- input$selectData})
                    
                    observeEvent(self$rv$data,{print(paste0('new value for self$rv$data : ', self$rv$data)) })
                    
                    observeEvent(input$dataOut_btn,{
                      dataOut$name = self$id
                      dataOut$obj = input$do
                      dataOut$trigger = runif(1,0,1)
                      print(paste0('send dataOut :', paste0(lapply(reactiveValuesToList(dataOut),function(x){ x}), collapse=' ')))
                    })
                    
                    observeEvent(input$pos_btn,{current.pos(input$pos_btn)})
                    
                    
                    observeEvent(lapply(res,function(x){x()}),{
                      print(paste0('retour de la timeline : ', res$reset(), ' ', res$current.pos()))
                    })
                    
                  }
                  ) }
  )
)
