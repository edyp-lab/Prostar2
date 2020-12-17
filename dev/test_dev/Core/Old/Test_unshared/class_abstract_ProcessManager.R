# Use of the tip in this page to unshare reactiveValues between different instances
# of the same class
# https://community.rstudio.com/t/r6-class-reactivevalues-property-and-instantiation/31025/2

ProcessManager <- R6Class(
  "ProcessManager",
  private = list(),
  public = list(
    input = NULL,
    id = NULL,
    timeline = NULL,
    timeline.res = NULL,
    ll.process = NULL,
    
    global = list(VALIDATED = 1,
                  SKIPPED = -1,
                  UNDONE = 0
    ),
    config = "<reactiveValues>" ,
    dataOut = "<reactiveValues>",
    rv = "<reactiveValues>",
    length = NULL,
    
    initialize = function(id, config = NULL) {
      cat(paste0(class(self)[1], '::initialize() from - ', self$id, '\n'))
      self$id <- id
      
      self$config = reactiveValues(
        name = NULL,
        mandatory = NULL,
        steps = NULL,
        screens = NULL,
        status = NULL
      )
      
      self$dataOut = reactiveValues(
        value = NULL,
        trigger = NULL
      )
      
      self$rv = reactiveValues(
        dataIn = NULL,
        current.pos = NULL,
        reset = NULL,
        isSkipped = FALSE)
      
      
      self$InitConfig(private$.config)
      self$Additional_Initialize_Class()
      },


    Additional_Initialize_Class = function(){},
    Wake = function(){ 
      cat(paste0(class(self)[1], '::Wake() from - ', self$id, '\n'))
      runif(1,0,1)
      },
    
    GetStringStatus = function(status){
      cat(paste0(class(self)[1], '::GetStringStatus() from - ', self$id, '\n'))
      if (status==self$global$VALIDATED) "Validated"
      else if (status==self$global$UNDONE) "Undone"
      else if (status==self$global$SKIPPED) 'Skipped'
    },
    
    Send_Result_to_Caller = function(){
      cat(paste0(class(self)[1], '::Send_Result_to_Caller() from - ', self$id, '\n'))
      self$dataOut$value <- self$rv$dataIn
      self$dataOut$trigger <- self$Wake()
    },
    
    Set_Skipped_Status = function(){
      cat(paste0(class(self)[1], '::Set_Skipped_Status() from - ', self$id, '\n'))
      if(verbose=='skip') browser()
      for (i in 1:self$length)
        if (self$config$status[i] != self$global$VALIDATED && self$GetMaxValidated_AllSteps() > i)
          self$config$status[i] <- self$global$SKIPPED
        if(verbose=='skip') browser()
    },
    
    GetMaxValidated_AllSteps = function(){
      cat(paste0(class(self)[1], '::', 'GetMaxValidated_AllSteps() from - ', self$id, '\n'))
      val <- 0
      ind <- which(self$config$status == self$global$VALIDATED)
      if (length(ind) > 0)
        val <- max(ind)
      val
    },
    
    GetMaxValidated_BeforeCurrentPos = function(){
      cat(paste0(class(self)[1], '::', 'GetMaxValidated_BeforeCurrentPos() from - ', self$id, '\n'))
      ind.max <- NULL
      indices.validated <- which(self$config$status == self$global$VALIDATED)
      if (length(indices.validated) > 0){
        ind <- which(indices.validated < self$rv$current.pos)
        if(length(ind) > 0)
          ind.max <- max(ind)
      }
      
      # if (ind.max == 0)
      #   ind.max <- 1
       
      ind.max
    },
    
    
    # Test if a process module (identified by its name) has been skipped.
    # This function is called each time the list config$isDone is updated
    # because one can know the status 'Skipped' only when a further module
    # has been validated
    is.skipped = function(name){
      cat(paste0(class(self)[1], '::', 'is.skipped() from - ', self$id, '\n'))
      if(verbose=='skip') browser()
      pos <- which(name == self$config$steps)
      return(self$GetStatusPosition(pos) == self$global$SKIPPED)
    },
    
    InitializeModule = function(){
      #self$config$screens <- self$CreateScreens()
      #self$config$screens <- self$GetScreensDefinition()
      self$rv$current.pos <- 1
    },
    
    ActionsOnNewPosition = function(){},
    
    GetScreensDefinition = function(){},
    
    ActionsOnIsSkipped = function(){},
    
    GetCurrentStepName = function(){
      cat(paste0(class(self)[1], '::GetCurrentStepName() from - ', self$id, '\n'))
      self$config$steps[self$rv$current.pos]
    },
    
    Unskip = function(pos){
      cat(paste0(class(self)[1], '::Unskip() from - ', self$id, '\n'))
      self$config$status[pos] <- self$global$UNDONE
    },
    
    GetStatusPosition = function(pos){
      cat(paste0(class(self)[1], '::GetStatusPosition() from - ', self$id, '\n'))
      self$config$status[pos]
    },
    
    # This function cannot be implemented in the timeline module because 
    # the id of the screens to reset are not known elsewhere.
    # Trying to reset the global 'div_screens' in the timeline module
    # does not work
    ResetScreens = function(){
      cat(paste0(class(self)[1], '::ResetScreens() from - ', self$id, '\n'))
      
      lapply(1:self$length, function(x){
        shinyjs::reset(NS(self$id)(self$config$steps[x]))
      })
    },
    
    #Function defined in child classes
    Actions_On_Data_Trigger = function(){},
    
    ValidateCurrentPos = function(){
      cat(paste0(class(self)[1], '::', 'ValidateCurrentPos() from - ', self$id, '\n'))
      if(verbose=='skip') browser()
      self$config$status[self$rv$current.pos] <- self$global$VALIDATED
      self$Set_Skipped_Status()
    #  browser()
      if (self$rv$current.pos == self$length)
        # Either the process has been validated, one can prepare data to be sent to caller
        # Or the module has been reseted
        self$Send_Result_to_Caller()
    },
    
   
    
    InitializeDataIn = function(){ 
      cat(paste0(class(self)[1], '::', 'InitializeDataIn() from - ', self$id, '\n'))
      if (verbose==T) browser()
      self$rv$dataIn <- self$rv$temp.dataIn
    },
    

    
    Initialize_Status_Process = function(){
      cat(paste0(class(self)[1], '::', 'Initialize_Status_Process() from - ', self$id, '\n'))
      self$config$status <- setNames(rep(self$global$UNDONE, self$length),
                                     self$config$steps)
    },
    
    Actions_On_Reset = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnReset() from - ', self$id, '\n'))
      #browser()
      self$ResetScreens()
      self$rv$dataIn <- NULL
      self$Initialize_Status_Process()
      self$Send_Result_to_Caller()
      self$InitializeDataIn()
    },
    
    #Actions onf receive new dataIn()
    ActionsOn_Tmp_NoInput = function(){
      #self$Actions_On_Reset()
      },
    
    ActionsOn_Tmp_Input = function(){},
    
    ActionsOn_NoTmp_Input = function(){},
    
    ActionsOn_NoTmp_NoInput = function(){
     # self$Actions_On_Reset()
    },
    
    Actions_On_New_DataIn = function(data){
      cat(paste0(class(self)[1], '::', 'Actions_On_New_DataIn() from - ', self$id, '\n'))
      if (verbose=='skip') browser()
      # This variable serves as a tampon while waiting the user click on the
      # validate button in the Description screen.
      # Once done, this variable is observed and the real rv$dataIn function can be
      # instanciated
      self$rv$temp.dataIn <- data
      
      
      # Test if input is NA or not
      inputExists <- !is.null(data)
      
      #Test if a dataset is already loaded
      tmpExists <- !is.null(self$rv$dataIn)
      
      
      if (tmpExists && inputExists){
        # this case is either the module is skipped or validated
        #self$rv$current.pos <- length(self$config$status)
        self$ActionsOn_Tmp_Input()
      } else if (tmpExists && !inputExists) {
        # The module has been reseted
        browser()
        self$ActionsOn_Tmp_NoInput()
      } else if (!tmpExists && inputExists){
        # The current position is pointed on a new module
        self$ActionsOn_NoTmp_Input()
      } else if (!tmpExists && !inputExists){
        # Initialization of Prostar
        self$ActionsOn_NoTmp_NoInput()
      }
    },
    
    CreateTimeline = function(){ },
    
    Additional_Funcs_In_Server = function(){},
    Additional_Funcs_In_ModuleServer = function(){},
    
    
    
    InitializeTimeline = function(){
      cat(paste0(class(self)[1], '::', 'InitializeTimeline() from - ', self$id, '\n'))
      browser()
      self$timeline.res <- self$timeline$server(
        config = reactive({self$config})
        )

      
      observeEvent(req(self$timeline.res$current.pos()), ignoreInit=T, {
        cat(paste0(class(self)[1], '::', 'observeEvent(req(self$timeline.res$current.pos()) from - ', self$id, '\n'))
        if (verbose==T) browser()
        self$rv$current.pos <- self$timeline.res$current.pos()
      })

      observeEvent(self$timeline.res$tl.reset(), ignoreInit = T,  ignoreNULL=T,{
        cat(paste0(class(self)[1], '::', 'observeEvent(req(self$timeline.res$tl.reset()) from - ', self$id, '\n'))
       # browser()
        self$rv$reset <-  self$timeline.res$tl.reset()
        self$Actions_On_Reset()
      })
      
      
    },
    
    
    # This function adds the renderUI functions of the screens.
    # In the process child class, these functions are written in a separate file for each process
    # One just have to call them by setting the variable logics (which is the name of the function
    # containing the renderUIs
    # In the pipeline child class, these functions are dynamically created 
    # Add_RenderUIs_Definitions = function(input, output){},
    # 
    TimelineUI = function(){
      cat(paste0(class(self)[1], '::', 'TimelineUI() from - ', self$id, '\n'))
      self$timeline$ui()
    },
    # 
    # 
    CheckConfig = function(conf){
      cat(paste0(class(self)[1], '::CheckConfig() from - ', self$id, '\n'))
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
      names.conf <- c("name", "steps", "mandatory")
      if (!all(sapply(names.conf, function(x){x %in% names(conf)}))){
        passed <- F
        msg <- c(msg, "The names of elements in 'config' must be the following: 'name', 'steps', 'mandatory'")
      }
      if (length(conf$steps) != length(conf$mandatory)){
        passed <- F
        msg <- c(msg, "The length of 'steps' and 'mandatory' must be equal.")
      }
      
      passed <- T
      list(passed=passed,
           msg = msg)
    },
    
    
    
    InitConfig = function(config){
      cat(paste0(class(self)[1], '::', 'InitConfig() from - ', self$id, '\n'))
      check <- self$CheckConfig(config)
      if (!check$passed)
        stop(paste0("Errors in 'config'", paste0(check$msg, collapse=' ')))
      
      self$length <- length(config$steps)
      
      observeEvent(config, {
        cat(paste0(class(self)[1], '::', 'observe() in InitConfig(config) from - ', self$id, '\n'))
        lapply(names(config), function(x){self$config[[x]] <- config[[x]]})
        self$config$type = class(self)[2]
        self$config$status <- setNames(rep(0, self$length), config$steps)
        #self$config$screens <- self$GetScreensDefinition()
        self$config$mandatory <- setNames(self$config$mandatory, self$config$steps)

        self$ll.process <- setNames(lapply(self$config$steps, function(x){x <- NULL}),
                                    self$config$steps)
        self$CreateTimeline()
      })

    },
    
    
    
    # UI
    ui = function() {
      ns <- NS(self$id)
      fluidPage(
        #wellPanel(
          #style="background-color: yellow;",
                 # uiOutput(ns('show_currentPos')),
                  uiOutput(ns('show_timeline_ui')),
                  #self$TimelineUI(),
                  #self$timeline$ui(),
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
                  )
       # )
      )
    },
    
    # SERVER
    server = function(dataIn = NULL, 
                      reset = reactive({NULL}),
                      isSkipped = reactive({NULL})) {
      ns <- NS(self$id)
      cat(paste0(class(self)[1], '::', 'server() from - ', self$id, '\n'))
      
      
      self$Additional_Funcs_In_Server()
      
      # Catch the new values of the temporary dataOut (instanciated by the last validation button of screens
      # and set the variable which will be read by the caller
      # observeEvent(self$dataOut$trigger, {
      #   cat(paste0(class(self)[1], '::observeEvent(self$dataOut$trigger)\n'))
      #   
      #   #self$Actions_On_Data_Trigger()
      # })
      
      
      
      
      observeEvent(req(dataIn()), ignoreNULL=T, ignoreInit = F, { 
        cat(paste0(class(self)[1], '::observeEvent(req(dataIn()) from - ', self$id, '\n'))
        self$Actions_On_New_DataIn(dataIn())
      })
      
      observeEvent(req(self$rv$current.pos), ignoreInit=T, {
        cat(paste0(class(self)[1], '::', 'observeEvent(req(self$rv$current.pos) from - ', self$id, '\n'))
        self$ActionsOnNewPosition()
      })
      
      observeEvent(reset(), ignoreInit = F, { 
        cat(paste0(class(self)[1], '::', 'observeEvent(remoteReset()) from - ', self$id, '\n'))
        print("remote reset activated")
        
        # Used to transmit info of local Reset to child processes
        self$rv$reset <- reset()
        self$Actions_On_Reset()
        })
      


      observeEvent(req(isSkipped()), { 
        cat(paste0(class(self)[1], '::observeEvent(isSkipped()) from - ', self$id, '\n'))
        if(verbose=='skip') browser()
        self$rv$isSkipped <- isSkipped()
        self$ActionsOnIsSkipped()
      })
      
      # MODULE SERVER
      moduleServer(self$id, function(input, output, session) {
        cat(paste0(class(self)[1], '::moduleServer() from - ', self$id, '\n'))
        # TODO In a script for dev, write a test function to check the validity of the logics for the new processLogics
        
        observe({ self$input <- input })
        
        self$Additional_Funcs_In_ModuleServer()
        
        output$show_timeline_ui <- renderUI({
          cat(paste0(class(self)[1], '::output$show_timeline_ui() from - ', self$id, '\n'))
          req(self$timeline)
          self$TimelineUI()
          })
        # 
        
        ###########---------------------------#################
        output$show_dataIn <- renderUI({
          cat(paste0(class(self)[1], '::output$show_dataIn from - ', self$id, '\n'))
          req(dataIn())
          tagList(
           # h4('show dataIn()'),
            lapply(names(dataIn()), function(x){tags$p(x)})
            )
        })
        
        output$show_rv_dataIn <- renderUI({
          tagList(
            #h4('show self$rv$dataIn)'),
            lapply(names(self$rv$dataIn), function(x){tags$p(x)})
            )
        })
        
        output$show_rv_dataOut <- renderUI({
          req(self$dataOut$trigger)
          self$dataOut$value
          tagList(
            #h4('show self$dataOut$value'),
            lapply(names(self$dataOut$value), function(x){tags$p(x)})
          )
        })

      }
      )
      reactive({self$dataOut})
      }
  )
)