ProcessManager <- R6Class(
  "ProcessManager",
  private = list(
    timelineForProcess = NULL,
    global = list(VALIDATED = 1,
                  SKIPPED = -1,
                  UNDONE = 1
    )
  ),
  public = list(id = NULL,
                config = reactiveValues() ,
                steps = NULL,
                dataOut = reactiveValues(),
                rv = reactiveValues(
                  dataIn = NULL,
                  current.pos = NULL,
                  wake = F,
                  timeline.res = NULL),
                
                initialize = function(id, config = NULL) {
                  self$id <- id
                  self$steps <- names(config$status)
                  lapply(names(config), function(x){self$config[[x]] <- config[[x]]})
                },
                
                GetStringStatus = function(status){
                  if (status==private$global$VALIDATED) "Validated"
                  else if (status==private$global$UNDONE) "Undone"
                  else if (status==private$global$SKIPPED) 'Skipped'
                },
                
                # UI
                ui = function() {
                  ns <- NS(self$id)
                  fluidPage(
                    wellPanel(style="background-color: yellow;",
                              h3('ProcessManager'),
                              uiOutput(ns('show_timeline_ui')),
                              hr(),
                              fluidRow(
                                column(width=2,
                                       tags$b(h4(style = 'color: blue;', "Input")),
                                       uiOutput(ns('show_dataIn'))),
                                column(width=2,
                                       tags$b(h4(style = 'color: blue;', "Output")),
                                       uiOutput(ns('show_rv_dataOut'))),
                                column(width=4,
                                       tags$b(h4(style = 'color: blue;', "status")),
                                       uiOutput(ns('show_status')))
                              ),
                              uiOutput(ns('screens'))
                    )
                  )
                },
                
                Wake = function(){ runif(1,0,1)},
                
                Initialize_Status_Process = function(){
                  self$config$status <- setNames(rep(0,length(self$config$status)),
                                                 names(self$config$status))
                },
                
                ValidateCurrentPos = function(){
                  self$config$status[self$rv$current.pos] <- private$global$VALIDATED
                  self$Set_Skipped_Status()
                  #browser()
                  if (self$config$status[length(self$config$status)] == private$global$VALIDATED)
                    # Either the process has been validated, one can prepare data to ben sent to caller
                    # Or the module has been reseted
                    self$Send_Result_to_Caller()
                },
                
                
                Set_Skipped_Status = function(){
                  for (i in 1:length(self$config$mandatory))
                    if (self$config$status[i] != private$global$VALIDATED && self$GetMaxValidated_AllSteps() > i)
                      self$config$status[i] <- private$global$SKIPPED
                },
                
                Send_Result_to_Caller = function(){
                  self$rv$wake <- self$Wake()
                  
                  self$dataOut$obj <- self$rv$dataIn
                  self$dataOut$name <- self$config$process.name
                  self$dataOut$trigger <- self$rv$wake
                },
                
                
                GetMaxValidated_AllSteps = function(){
                  val <- 0
                  ind <- which(self$config$status == private$global$VALIDATED)
                  if (length(ind) > 0)
                    val <- max(ind)
                  val
                },
                
                GetMaxValidated_BeforeCurrentPos = function(){
                  ind.max <- 0
                  indices.validated <- which(self$config$status == private$global$VALIDATED)
                  if (length(indices.validated) > 0){
                    ind <- which(indices.validated < self$rv$current.pos)
                    if(length(ind) > 0)
                      ind.max <- max(ind)
                  }
                  
                  if (ind.max == 0)
                    ind.max <- 1
                  
                  ind.max
                },
                
                
                # Test if a process module (identified by its name) has been skipped.
                # This function is called each time the list config$isDone is updated
                # because one can know the status 'Skipped' only when a further module
                # has been validated
                is.skipped = function(name){
                  pos <- which(name == names(config$mandatory))
                  return(GetStatusPosition(pos) == private$global$SKIPPED)
                },
                

                
                GetCurrentStepName = reactive({ names(config$mandatory)[self$rv$current.pos] }),
                
                Unskip = function(pos){config$status[pos] <- private$global$UNDONE},
                
                GetStatusPosition = function(pos){config$status[pos]},
                
                
                
                ##########################################################################"
                #####################################################################
                ## screens of the module
                ##
                ############### SCREEN 1 ######################################
                ProcessLogics = function(id, input, output){
                  ns <- NS(id)
                  output$Description <- renderUI({
                      tagList(
                        actionButton(ns('start_btn'), 
                                     paste0('Start ', self$config$process.name),
                                     class = btn_success_color),
                        mod_insert_md_ui(ns(paste0(self$config$process.name, "_md")))
                      )
                    })
                    
                    
                    observe({
                      mod_insert_md_server(paste0(self$config$process.name, "_md"), 
                                           paste0('./md/', self$config$process.name, '.md'))
                    })
                    
                    # observeEvent(input$start_btn, {
                    #   self$InitializeDataIn()
                    #   ValidateCurrentPos()
                    # })
                    
                    ############### SCREEN 2 ######################################
                    
                    output$Step1 <- renderUI({
                      name <- 'Step1'
                      
                      tagList(
                        div(id=ns(name),
                            div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                                tags$h2(name)),
                            div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                                selectInput(ns('select1'), 'Select step 1', 
                                            choices = 1:5, 
                                            selected = 1,
                                            width = '150px')
                            ),
                            div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                                actionButton(ns(paste0('perform_', name, '_btn')), 'Perform'))
                        )
                      )
                    })
                    
                    
                    observeEvent(input$perform_Step1_btn, {
                      self$ValidateCurrentPos()
                    })
                    
                    ############### SCREEN 3 ######################################
                    output$Step2 <- renderUI({
                      name <- 'Step2'
                      tagList(
                        div(id=ns(name),
                            div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                                tags$h3(name)),
                            div(style="display:inline-block; vertical-align: middle;padding-right: 40px;",
                                selectInput(ns('select2'), 'Select step 2',
                                            choices = 1:5,
                                            selected = 1,
                                            width = '150px')),
                            div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                                actionButton(ns(paste0('perform_', name, '_btn')), 'Perform'))
                        )
                      )
                    })
                    
                    ## Logics to implement: here, we must take the last data not null
                    # in previous datas. The objective is to take account
                    # of skipped steps
                    observeEvent(input$perform_Step2_btn, {
                      self$ValidateCurrentPos()
                    })
                    
                    
                    
                    
                    ############### SCREEN 4 ######################################
                    output$Step3 <- renderUI({
                      name <- 'Step3'
                      
                      tagList(
                        div(id=ns(name),
                            div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                                tags$h3(name)),
                            div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                                actionButton(ns('validate_btn'), 'Validate'))
                        )
                      )
                      
                    })
                    
                    
                    observeEvent(input$validate_btn, {
                      # rv$dataIn <- AddItemToDataset(self$rv$dataIn, self$config$process.name)
                      self$ValidateCurrentPos()
                    })
                  },
                ################################################################################
                
                # SERVER
                server = function(dataIn = NULL, 
                                  dataOut = NULL,
                                  remoteReset = FALSE,
                                  isSkipped = FALSE,
                                  logics = NULL) {
                  ns <- NS(self$id)
                  current.pos = reactiveVal()
                  
                  
                  observeEvent(self$dataOut$trigger, {
                    print("new values on self$dataOut$trigger")
                    dataOut$name <- self$dataOut$name
                    dataOut$obj <- self$dataOut$obj
                    dataOut$trigger <- self$dataOut$trigger
                  })
                  
                  
                  
                  
                  observeEvent(req(dataIn()), ignoreNULL=T, ignoreInit = F, { 
                    
                    print('event detected on dataIn()')
                    # Test if input is NA or not
                    inputExists <- length(self$config$status) > 0
                    
                    #Test if a dataset is already loaded
                    tmpExists <- !is.null(self$rv$dataIn)
                    
                    self$rv$wake <- FALSE
                    
                    if (tmpExists){
                      # this case is either the module is skipped or validated
                      #self$rv$current.pos <- length(self$config$status)
                      self$rv$wake <- self$Wake()
                    } else {
                      if (inputExists){
                        # The current position is pointed on a new module
                        print("launch case 3")
                        
                        InitializeDataIn()
                        InitializeModule()
                        InitializeTimeline()
                        self$ValidateCurrentPos()
                      } else if (!inputExists){
                        # Initialization of Prostar
                      }
                    }
                    
                  })
                  
                  
                  observe({
                    #browser()
                    private$timelineForProcess <- TimelineForProcess$new(
                      id = NS(self$id)('timeline'),
                      mandatory = self$config$mandatory
                    )
                  })
                  
                  
                  InitializeTimeline <- function(){
                    self$rv$timeline.res <- private$timelineForProcess$server(
                      config = self$config,
                      wake = reactive({self$rv$wake}),
                      remoteReset = remoteReset
                    )
                    
                    #Catch a new position from timeline
                    observeEvent(req(self$rv$timeline.res$current.pos()), ignoreInit=T, {
                      self$rv$current.pos <- self$rv$timeline.res$current.pos()
                    })
                    
                    
                    #--- Catch a reset from timeline or caller
                    observeEvent(req(c(self$rv$timeline.res$reset()!=0, remoteReset()!=0)), {
                      ResetScreens()
                      self$rv$dataIn <- NA
                      self$Initialize_Status_Process()
                      self$Send_Result_to_Caller()
                      InitializeDataIn()
                    })
                  }
                  #})
                  
                  
                  CreateScreens <- function(){
                    setNames(
                      lapply(1:length(self$steps), 
                             function(x){
                               do.call(uiOutput, list(outputId=ns(self$steps)[x]))}),
                      self$steps)
                  }
                  
                  InitializeModule <- function(){
                    self$Initialize_Status_Process()
                    
                    self$config$screens <- CreateScreens()
                    
                    
                    # Must be placed after the initialisation of the 'config$stepsNames' variable
                    # config$screens <- CreateScreens(names(config$mandatory))
                    #InitializeDataIn()
                    self$rv$current.pos <- 1
                  }
                  
                  InitializeDataIn <- function(){ self$rv$dataIn <- dataIn()}
                  
                  # This function cannot be implemented in the timeline module because 
                  # the id of the screens to reset are not known elsewhere.
                  # Trying to reset the global 'div_screens' in the timeline module
                  # does not work
                  ResetScreens <- function(screens){
                    lapply(1:length(self$config$status), function(x){
                      shinyjs::reset(names(self$config$mandatory)[x])
                    })
                  }
                  
                  
                  
                  
                  # MODULE SERVER
                  moduleServer(self$id, function(input, output, session) {
                    
                    ###########---------------------------#################
                    output$show_dataIn <- renderUI({
                      req(dataIn())
                      tagList(lapply(names(dataIn()), function(x){tags$p(x)}))
                    })
                    
                    output$show_rv_dataIn <- renderUI({
                      tagList(lapply(names(self$rv$dataIn), function(x){tags$p(x)}))
                    })
                    
                    output$show_rv_dataOut <- renderUI({
                      req(dataOut$trigger)
                      tagList(
                        lapply(names(dataOut$obj), function(x){tags$p(x)})
                      )
                    })
                    
                    
                    output$show_timeline_ui <- renderUI({
                      req(private$timelineForProcess)
                      private$timelineForProcess$ui()
                    })
                    
                    output$show_status <- renderUI({
                      req(self$config$status, self$rv$current.pos)
                      tagList(lapply(1:length(self$config$status), 
                                     function(x){if (x == self$rv$current.pos) tags$p(tags$b(paste0('-> ',names(self$config$status)[x], ' - ', self$GetStringStatus(self$config$status[[x]]))))
                                       else tags$p(paste0(names(self$config$status)[x], ' - ', self$GetStringStatus(self$config$status[[x]])))
                                     }))
                    })
                    
                    ###########---------------------------#################
                    
                    
                    
                    #################################################
                    # Main listener of the module which initialize it
                    
                    observeEvent(isSkipped(), ignoreInit = T, {
                      if (isSkipped())
                        tag <- private$global$SKIPPED
                      else
                        tag <- private$global$UNDONE
                      
                      self$config$status <- setNames(rep(tag,length(self$config$status)),
                                                     names(self$config$status))
                      
                      
                    })
                    
                    
                    GetValidationBtnIds <- reactive({validated.btns <- grep('_validate_btn', names(input))})
                    
                    
                    ###################################################
                    #output$screens <- renderUI({self$ServerScreens()})
                    #logics(self$id, input, output)
                    self$ProcessLogics(self$id, input, output)
                  }
                  ) }
  )
)