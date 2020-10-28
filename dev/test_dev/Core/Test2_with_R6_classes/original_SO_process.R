library(shiny)
library(R6)
library(tibble)

options(shiny.fullstacktrace = F)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('.', 'class_TimelineDraw.R'), local=TRUE)$value

source(file.path('../../../../R', 'mod_insert_md.R'), local=TRUE)$value
source(file.path('../../../../R', 'global.R'), local=TRUE)$value

# ------------- Class TimelineDataManager  --------------------------------------
TimelineManager <- R6Class(
  "TimelineManager",
  private=list(verbose = T,
               style = 2,
               global = list(VALIDATED = 1,
                             SKIPPED = -1,
                             UNDONE = 1
                             ),
               default_pos =list(VALIDATED = 1,
                                 SKIPPED = 1,
                                 UNDONE = 1
                                 ), 
               length = NULL,
               modal_txt = NULL,
               btn_style = "display:inline-block; vertical-align: middle; padding: 7px",
               timelineDraw  = NULL,
               
               
               Analyse_status = function(){}
               ),
  
  public = list(id = NULL,
                steps = NULL,
                
                
                initialize = function(id, steps, style=2 ) {
                  self$id <- id
                  self$steps = steps
                  private$timelineDraw <- TimelineDraw$new(NS(id)('tl_draw'),
                                                   steps = steps,
                                                   style = style)
                }, 
                
                # UI
                ui = function() {
                  ns <- NS(self$id)
                  fluidPage(
                    wellPanel(style="background-color: lightblue;",
                              tagList(h3('TimelineManager'),
                                      shinyjs::useShinyjs(),
                                      div(id = 'GlobalTL',
                                          fluidRow(
                                            align= 'center',
                                            column(width=2,div(style = private$btn_style,
                                                         uiOutput(ns('showPrevBtn')),
                                                         uiOutput(ns('showResetBtn'))
                                                         )
                                                   ),
                                            column(width=8,div( style = private$btn_style,
                                                                private$timelineDraw$ui())),
                                            column(width=2,div(style = private$btn_style,
                                                               uiOutput(ns('showNextBtn')),
                                                               uiOutput(ns('showSaveExitBtn'))
                                                               )
                                                   )
                                            ),
                                          uiOutput(ns('show_screens'))
                                          )
                                      )
                              )
                  )
                },
                
                SetModalTxt = function(txt){private$modal_txt <- txt},
                
                # SERVER
                server = function(config, wake) {
                  ns <- NS(self$id)
                  rv = reactiveValues(
                    current.pos = 1,
                    reset_OK = NULL
                  )
                  private$timelineDraw$server(
                    status = reactive({config$status}),
                    position = reactive({rv$current.pos})
                    )
                  
                  # MODULE SERVER
                  moduleServer(self$id, function(input, output, session) {
                    ns <- NS(self$id)
                    
                   

                    ###############################
                    output$showResetBtn <- renderUI({
                      print(paste0('TL(',self$id, ') : output$showResetBtn <- renderUI'))
                      actionButton(ns("rstBtn"), paste0("Reset ", config$type),
                                   style='padding:4px; font-size:80%')
                    })
                    
                    output$showPrevBtn <- renderUI({
                      shinyjs::disabled(actionButton(ns("prevBtn"), "<<",
                                                     style='padding:4px; font-size:80%'))
                    })
                    
                    output$showNextBtn <- renderUI({
                      shinyjs::disabled(actionButton(ns("nextBtn"), "next",
                                                     style='padding:4px; font-size:80%'))
                    })
                    
                    
                    
                    #-------------------------------------------------------
                    # Return the UI for a modal dialog with data selection input. If 'failed' is
                    # TRUE, then display a message that the previous value was invalid.
                    dataModal <- function() {
                      modalDialog(
                        span(private$modal_txt),
                        footer = tagList(
                          modalButton("Cancel"),
                          actionButton(ns("modal_ok"), "OK")
                        )
                      )
                    }
                    
                    
                    # Show modal when button reset is clicked
                    observeEvent(input$rstBtn, {
                      showModal(dataModal())
                    })
                    
                    # When OK button is pressed, update the reactive value which will be sent
                    # to the caller
                    observeEvent(input$modal_ok, {
                      print('EVENT ON MODAL RESET')
                      rv$reset_OK <- input$rstBtn
                      removeModal()
                    })
                    
                    navPage <- function(direction) {
                      newval <- rv$current.pos + direction 
                      newval <- max(1, newval)
                      newval <- min(newval, length(config$status))
                      if(newval == 0)
                        browser()
                      
                      rv$current.pos <- newval
                    }
                    
                    observeEvent(req(wake()),{Update_Cursor_position()})
                    
                    
                    Init_Default_Positions <- reactive({
                      private$default_pos <- list(VALIDATED = length(config$status),
                                                  SKIPPED = length(config$status),
                                                  UNDONE = 1
                                                  )
                    })
                    
                    
                    
                    observeEvent(input$prevBtn, ignoreInit = TRUE, {navPage(-1)})
                    observeEvent(input$nextBtn, ignoreInit = TRUE, {navPage(1)})
                    
                    
                    output$show_screens <- renderUI({tagList(config$screens)})
                    
                    
                    Analyse_status_Process = function(){
                      req(length(config$status))
                      if ((length(config$status)==1) || (length(config$status)>=2 && sum(unlist(config$status)[2:length(config$status)])== 0 )){
                        # This is the case at the initialization of a process or after a reset
                        # Enable all steps and buttons
                        toggleState_Steps(cond = TRUE, i = length(config$status))
                      } else if (config$status[[length(config$status)]] == private$global$SKIPPED){
                        # Disable all steps
                        toggleState_Steps(cond = FALSE, i = length(self$config$status))
                      } else {
                        # Disable all previous steps from each VALIDATED step
                        ind.max <- max(grep(private$global$VALIDATED, unlist(config$status)))
                        toggleState_Steps(cond = FALSE, i = ind.max)
                      }
                    }
                    
                    # Catch a new position or a change in the status list
                    observeEvent(req(c(rv$current.pos, config$status)), {
                      

                      
                    #  if (config$type == 'process'){
                        # Update_Cursor_position()
                        Analyse_status_Process()
                        
                        # One only displays the steps that are not skipped
                     #   lapply(1:length(self$config$status), function(x){
                     #     shinyjs::toggle(paste0('div_screen', x), condition = x==self$rv$current.pos && config$status[[self$rv$current.pos]] != private$global$SKIPPED)})
                        
                     # } else if (config$type == 'pipeline'){
                        #Analyse_status_Pipeline()
                        
                        # Display current page
                        # One display all processes, even the validated ones
                        lapply(1:length(config$status), function(x){
                          shinyjs::toggle(paste0('div_screen', x), condition = x==self$rv$current.pos)})
                      #}

                      Update_Buttons()
                    })
                    
                    
                    observeEvent(req(config), ignoreInit=F,{
                      req(length(config$screens)>0)
                      
                      if (!CheckConfig(config)$passed)
                        stop(paste0("Errors in 'config'", paste0(CheckConfig(config)$msg, collapse=' ')))
                      
                      EncapsulateScreens()
                      
                    })
                    
                    # Initialization of the screens by integrating them into a div specific
                    # to this module (name prefixed with the ns() function
                    # Those div englobs the div of the caller where screens are defined
                    EncapsulateScreens <- reactive({
                      req(config$screens)
                      #length(self$config$status) <- length(config$status)
                     # browser()
                      Init_Default_Positions() 
                      config$screens <- lapply(1:length(config$status),
                                               function(x){
                                                 config$screens[[x]] <- if (x == 1) 
                                                   div(id = ns(paste0("div_screen", x)),  config$screens[[x]])
                                                 else 
                                                   shinyjs::hidden(div(id = ns(paste0("div_screen", x)),  config$screens[[x]]))
                                               })
                    })
                    
                    Update_Buttons <- reactive({
                      # Compute status for the Next button
                      end_of_tl <- rv$current.pos == length(config$status)
                      mandatory_step <- isTRUE(config$mandatory[[rv$current.pos]])
                      validated <- config$status[[rv$current.pos]] == private$global$VALIDATED
                      skipped <- config$status[[rv$current.pos]] == private$global$SKIPPED
                      entireProcessSkipped <- config$status[[length(config$status)]] == private$global$SKIPPED
                      NextBtn_logics <- !end_of_tl && !entireProcessSkipped && (!mandatory_step || (mandatory_step && (validated || skipped)))
                      
                      # Compute status for the Previous button
                      start_of_tl <- rv$current.pos == 1
                      entireProcessSkipped <- config$status[[length(config$status)]] == private$global$SKIPPED
                      PrevBtn_logics <- !start_of_tl && !entireProcessSkipped
                      
                      shinyjs::toggleState('prevBtn', cond = PrevBtn_logics)
                      shinyjs::toggleState('nextBtn', cond = NextBtn_logics)
                    })
                    
                    
                    CheckConfig = function(conf){
                      passed <- T
                      msg <- ""
                      if (!is.list(conf)){
                        passed <- F
                        msg <- c(msg, "'config' is not a list")
                      }
                      if (length(conf)!=3){
                        passed <- F
                        msg <- c(msg, "The length of 'config' is not equal to 4")
                      }
                      names.conf <- c("process.name", "type", "steps")
                      if (!all(sapply(names.conf, function(x){x %in% names(conf)}))){
                        passed <- F
                        msg <- c(msg, "The names of elements in 'config' must be the following: 'process.name', 'type', 'steps'")
                      }
                      if (!is.list(conf$steps)){
                        passed <- F
                        msg <- c(msg, "The 'steps' slot is not a list")
                      }
                      
                      passed <- T
                      list(passed=passed,
                           msg = msg)
                    }
                    
                    toggleState_Steps = function(cond, i){
                      lapply(1:i, function(x){
                        shinyjs::toggleState(paste0('div_screen', x), condition = cond)})
                    }
                    
                    
                    
                    Update_Cursor_position = function(){
                      req(config$status)

                      if (config$status[length(config$status)] == private$global$VALIDATED)
                        rv$current.pos <- private$default_pos$VALIDATED
                      else if (config$status[length(config$status)] == private$global$SKIPPED)
                        rv$current.pos <- private$default_pos$SKIPPED
                      else if (config$status[length(config$status)] == private$global$UNDONE)
                        rv$current.pos <- private$default_pos$UNDONE
                    }
                    
                    #####################################
                    list(current.pos = reactive({rv$current.pos}),
                         reset = reactive({rv$reset_OK})
                         )
                  })
                }
  )
)


















#----------------------- Class ProcessManager ----------------------------------
ProcessManager <- R6Class(
  "ProcessManager",
  private = list(
    timelineManager = NULL,
    global = list(VALIDATED = 1,
                  SKIPPED = -1,
                  UNDONE = 1
    )
  ),
  public = list(id = NULL,
                config = reactiveValues(
                  process.name = 'processX',
                  mandatory = setNames(c(T, F, F,T), c("Description", "Step1", "Step2", "Step3")),
                  status = setNames(c(0, 0, 0,0), c("Description", "Step1", "Step2", "Step3")),
                  screens = list()
                ),
                
                
                rv = reactiveValues(
                  dataIn = NULL,
                  current.pos = 1,
                  selectData = 0,
                  wake = F,
                  timeline.res = NULL),
                
                
                res = reactiveValues(
                  name = NULL,
                  obj = NULL,
                  trigger = NULL
                ),
                
                initialize = function(id) {
                  self$id <- id
                },
                # UI
                ui = function() {
                  ns <- NS(self$id)
                  fluidPage(
                  wellPanel(style="background-color: yellow;",
                            h3('ProcessManager'),
                            uiOutput(ns('show_timeline_ui')),
                            uiOutput(ns("select")),
                            actionButton(ns("dataOut_btn"), "Simulate validate (change in dataOut)"),
                            actionButton(ns("pos_btn"), "Simulate change in pos"),
                            hr(),
                             uiOutput(ns('show_dataIn'))
                            )
                  )
                },
                
                Wake = function(){ runif(1,0,1)},
                

                # SERVER
                server = function(dataIn = NULL, 
                                  dataOut = NULL,
                                  remoteReset = FALSE,
                                  isSkipped = FALSE) {
                  ns <- NS(self$id)
                  
                 
                  
                  current.pos = reactiveVal()
                  Initialize_Status_Process = function(){
                    self$config$status <- setNames(rep(0,length(self$config$status)),
                                                         names(self$config$status))
                  }
                  
                  #Initialize_Status_Process()
                  self$config$screens <- list(uiOutput(ns('Description')),
                                              uiOutput(ns('Step1')),
                                              uiOutput(ns('Step2')),
                                              uiOutput(ns('Step3'))
                                              
                                              )
             
                  
                  
                  
                  observeEvent(req(dataIn()), ignoreNULL=T, ignoreInit = F, { 
                    
                    #browser()
                    inputExists <- length(self$config$status) > 0
                    tmpExists <- !is.null(self$rv$dataIn)
                    self$rv$wake <- FALSE

                    if (tmpExists){
                      # this case is either the module is skipped or validated
                      self$rv$current.pos <- length(self$config$status)
                      self$rv$wake <- self$Wake()
                    } else {
                      if (inputExists){
                        # The current position is pointed on a new module
                        InitializeDataIn()
                        InitializeModule()
                        InitializeTimeline()
                      } else if (!inputExists){
                        # Initialization of Prostar
                      }
                    }
                    
                  })
                  
                  
                  
                  
                  
                  InitializeTimeline <- function(){
                    private$timelineManager <- TimelineManager$new(
                      id = NS(self$id)('timeline'),
                      steps = reactive({self$config$mandatory})
                    )
                    
                    self$rv$timeline.res <- private$timelineManager$server(
                      config = self$config,
                      wake = reactive({self$rv$wake})
                    )
                    
                    #Catch a new position from timeline
                    observeEvent(req(self$rv$timeline.res$current.pos()), ignoreInit=T, {
                      print("NEW POSITION FROM TIMELINE")
                      self$rv$current.pos <- self$rv$timeline.res$current.pos()
                      
                    })
                    
                    observe({
                      print(self$rv$timeline.res$current.pos())
                   
                    })
                    #--- Catch a reset from timeline or caller
                    observeEvent(req(c(self$rv$timeline.res$reset()!=0, remoteReset()!=0)), {
                      print("RESET FROM TIMELINE")
                      ResetScreens()
                      self$rv$dataIn <- NA
                      self$rv$current.pos <- 1
                      self$rv$wake <- Wake()
                      Initialize_Status_Process()
                      Send_Result_to_Caller()
                      InitializeDataIn()
                    })
                  }
                  
                  
                  
                  
                  InitializeModule <- function(){
                    Initialize_Status_Process()
                    # rv$screens <- InitScreens(length(self$config$status))
                    self$config$screens <- list(uiOutput(ns('Description')),
                                                uiOutput(ns('Step1')),
                                                uiOutput(ns('Step2')),
                                                uiOutput(ns('Step3'))
                                                
                    )
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
                  
                  
                  
                  
                  Send_Result_to_Caller <- reactive({
                    dataOut$obj <- rv$dataIn
                    dataOut$name <- 'toto'
                    dataOut$trigger <- self$rv$wake
                  })
                 
                  # MODULE SERVER
                  moduleServer(self$id, function(input, output, session) {
                    
                    output$show_timeline_ui <- renderUI({
                      req(private$timelineManager)
                      private$timelineManager$ui()
                    })
                    
                    output$select <- renderUI({selectInput(ns('selectData'), 'Mother : Select data', 1:4) })
                    output$show_dataIn <- renderUI({paste0('received dataIn = ', dataIn())})
                    observeEvent(dataOut$trigger,{print(paste0('receive new dataOut : ', paste0(lapply(reactiveValuesToList(dataOut),function(x){ x}), collapse=' ')))})
                    observeEvent(input$selectData,{self$rv$data <- input$selectData})
                    observeEvent(self$rv$data,{print(paste0('new value for self$rv$data : ', self$rv$data)) })
                    
                    
                    
                    observeEvent(input$dataOut_btn,{
                      Send_Result_to_Caller()
                      print(paste0('send dataOut :', paste0(lapply(reactiveValuesToList(dataOut),function(x){ x}), collapse=' ')))
                    })
                    #################################################
                    # Main listener of the module which initialize it
                    
                    
                    
                    
                    
                   
                    
                    
                    observeEvent(req(isSkipped()), {
                      if (isSkipped())
                        Initialize_Status_Process()
                    })
                    
                    
                    
                    
                    GetMaxValidated_AllSteps <- reactive({
                      val <- 0
                      ind <- which(self$config$status == private$global$VALIDATED)
                      if (length(ind) > 0)
                        val <- max(ind)
                      val
                    })
                    
                    GetMaxValidated_BeforeCurrentPos <- reactive({
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
                    })
                    
                    
                    # Test if a process module (identified by its name) has been skipped.
                    # This function is called each time the list config$isDone is updated
                    # because one can know the status 'Skipped' only when a further module
                    # has been validated
                    is.skipped <- function(name){
                      pos <- which(name == names(config$mandatory))
                      return(GetStatusPosition(pos) == private$global$SKIPPED)
                    }
                    
                    
                    
                    GetMaxValidated_AllSteps <- reactive({
                      val <- 0
                      ind <- which(self$config$status == private$global$VALIDATED)
                      if (length(ind) > 0)
                        val <- max(ind)
                      val
                    })
                    
                    
                    
                    GetCurrentStepName <- reactive({ names(config$mandatory)[self$rv$current.pos] })
                    
                    Unskip <- function(pos){config$status[pos] <- private$global$UNDONE}
                    
                    GetStatusPosition <- function(pos){config$status[pos]}
                    
                    
                   
                    Set_Skipped_Status <- function(){
                      for (i in 1:length(self$config$mandatory))
                        if (self$config$status[i] != private$global$VALIDATED && GetMaxValidated_AllSteps() > i)
                          self$config$status[i] <- private$global$SKIPPED
                    }
                    
                   # observeEvent(self$config$status,{
                   #   browser()
                   #   Set_Skipped_Status()})
                    
                    ValidateCurrentPos <- reactive({
                      print("run ValidateCurrentPos()")
                      self$config$status[self$rv$current.pos] <- private$global$VALIDATED
                      Set_Skipped_Status()
                      #browser()
                      if (self$config$status[length(self$config$status)] == private$global$VALIDATED)
                        # Either the process has been validated, one can prepare data to ben sent to caller
                        # Or the module has been reseted
                        Send_Result_to_Caller()
                    })
                    
                    
                    GetValidationBtnIds <- reactive({
                      
                      validated.btns <- grep('_validate_btn', names(input))
                    })
                    
                    
                    ###################################################
                    #####################################################################
                    ## screens of the module
                    ##
                    ############### SCREEN 1 ######################################
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
                    
                    observeEvent(input$start_btn, {
                      print('event on statr_btn')
                       InitializeDataIn()
                      ValidateCurrentPos()
                    })
                    
                    ############### SCREEN 2 ######################################
                    
                    output$Step1 <- renderUI({
                      name <- 'Step1'
                      
                      tagList(
                        div(id=ns(name),
                            div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                                tags$h2('Step 1')),
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
                      print('event on perform_Step1_btn')
                      ValidateCurrentPos()
                    })
                    
                    ############### SCREEN 3 ######################################
                    output$Step2 <- renderUI({
                      name <- 'Step2'
                      tagList(
                        div(id=ns(name),
                            div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                                tags$h3('Step 2')),
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
                      print('event on perform_Step2_btn')
                      ValidateCurrentPos()
                    })
                    
                    
                    
                    
                    ############### SCREEN 4 ######################################
                    output$Step3 <- renderUI({
                      name <- 'Step3'
                      
                      tagList(
                        div(id=ns(name),
                            div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                                tags$h3('Step 3')),
                            div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                                actionButton(ns('validate_btn'), 'Validate'))
                        )
                      )
                      
                    })
                    
                    
                    observeEvent(input$validate_btn, {
                     # rv$dataIn <- AddItemToDataset(self$rv$dataIn, self$config$process.name)
                      print('event on validate_btn')
                      ValidateCurrentPos()
                    })
                    
                    
                    
                    #################################################
                    
                  }
                  ) }
  )
)

#----------------------------------------------------------------------------


rv = reactiveValues(data = 3)
dataOut <- reactiveValues()

processManager <- ProcessManager$new("ProcessManager")

ui = function() {
  fluidPage(
    wellPanel(style="background-color: green;",
              h3('Prostar'),
              processManager$ui()
    )
  )
    }
server = function(input, output, session) {
  processManager$server(
    dataIn = reactive({rv$data}),
    dataOut = dataOut,
    remoteReset = reactive({NULL}),
    isSkipped = reactive({FALSE})
    )
}

shinyApp(ui, server)
