# Use of the tip in this page to unshare reactiveValues between different instances
# of the same class
# https://community.rstudio.com/t/r6-class-reactivevalues-property-and-instantiation/31025/2

ProcessManager <- R6Class(
  "ProcessManager",
  private = list(),
  public = list(
    input = NULL,
    id = NULL,
    ns = NULL,
    timeline = NULL,
    timeline.res = NULL,
    ll.process = NULL,
    length = NULL,
    config = NULL,
    screens = NULL,
    
    dataOut = "<reactiveValues>",
    rv = "<reactiveValues>",
    
    initialize = function(id) {
      cat(paste0(class(self)[1], '::initialize() from - ', self$id, '\n'))
      self$id <- id
      self$ns <- NS(id)
      
      self$dataOut = reactiveValues(
        value = NULL,
        trigger = NULL
      )
      
      self$rv = reactiveValues(
        dataIn = NULL,
        temp.dataIn = NULL,
        current.pos = 1,
        status = NULL,
        reset = NULL,
        isSkipped = FALSE,
        dataLoaded = FALSE)
      
      check <- self$CheckConfig(private$.config)
      if (!check$passed)
        stop(paste0("Errors in 'config'", paste0(check$msg, collapse=' ')))
      else
        self$config <- private$.config
      
      self$length <- length(self$config$mandatory)
      self$config$type = class(self)[2]
      self$config$mandatory <- setNames(self$config$mandatory, self$config$steps)
      
      self$rv$status <- setNames(rep(global$UNDONE, self$length), self$config$steps)
      
      self$ll.process <- setNames(lapply(self$config$steps, function(x){x <- NULL}),
                                  self$config$steps)
      
      self$Additional_Initialize_Class()
      
     },
    
    Additional_Initialize_Class = function(){},
    
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
    
    #GetScreensDefinition = function(){},
    
    Wake = function(){ 
      cat(paste0(class(self)[1], '::Wake() from - ', self$id, '\n'))
      runif(1,0,1)
    },
    
    Send_Result_to_Caller = function(){
      cat(paste0(class(self)[1], '::Send_Result_to_Caller() from - ', self$id, '\n'))
      self$dataOut$value <- self$rv$dataIn
      self$dataOut$trigger <- self$Wake()
    },
    
    InitializeDataIn = function(){ 
      cat(paste0(class(self)[1], '::', 'InitializeDataIn() from - ', self$id, '\n'))
      if (verbose==T) browser()
      self$rv$dataIn <- self$rv$temp.dataIn
    },
    
    GetMaxValidated_AllSteps = function(){
      cat(paste0(class(self)[1], '::', 'GetMaxValidated_AllSteps() from - ', self$id, '\n'))
      val <- 0
      ind <- which(self$rv$status == global$VALIDATED)
      if (length(ind) > 0)
        val <- max(ind)
      val
    },
    
    Set_Skipped_Status = function(){
      cat(paste0(class(self)[1], '::Set_Skipped_Status() from - ', self$id, '\n'))
      if(verbose=='skip') browser()
      for (i in 1:self$length)
        if (self$rv$status[i] != global$VALIDATED && self$GetMaxValidated_AllSteps() > i)
          self$rv$status[i] <- global$SKIPPED
    },
    
    Initialize_Status_Process = function(){
      cat(paste0(class(self)[1], '::', 'Initialize_Status_Process() from - ', self$id, '\n'))
      self$rv$status <- setNames(rep(global$UNDONE, self$length),self$config$steps)
    },
    
    ActionOn_Reset = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnReset() from - ', self$id, '\n'))
      #browser()
      self$ResetScreens()
      self$rv$dataIn <- NULL
      self$Initialize_Status_Process()
      self$Send_Result_to_Caller()
      self$InitializeDataIn()
    },
    
    # This function cannot be implemented in the timeline module because 
    # the id of the screens to reset are not known elsewhere.
    # Trying to reset the global 'div_screens' in the timeline module
    # does not work
    ResetScreens = function(){
      cat(paste0(class(self)[1], '::ResetScreens() from - ', self$id, '\n'))
      
      lapply(1:self$length, function(x){
        shinyjs::reset(self$ns(self$config$steps[x]))
      })
    },
    
    ValidateCurrentPos = function(){
      cat(paste0(class(self)[1], '::', 'ValidateCurrentPos() from - ', self$id, '\n'))
      if(verbose=='skip') browser()
      self$rv$status[self$rv$current.pos] <- global$VALIDATED
      self$Set_Skipped_Status()
      #browser()
      # Either the process has been validated, one can prepare data to be sent to caller
      # Or the module has been reseted
      if (self$rv$current.pos == self$length)
        self$Send_Result_to_Caller()
    },
    Additional_Server_Funcs = function(){},
    
    # UI
    ui = function() {
      cat(paste0(class(self)[1], '::ui() from - ', self$id, '\n'))
      #browser()
      fluidPage(
        uiOutput(self$ns('show_tl')),
        hr(),
        fluidRow(
          column(width=2,
                 tags$b(h4(style = 'color: blue;', "Input")),
                 uiOutput(self$ns('show_dataIn'))),
          column(width=2,
                 tags$b(h4(style = 'color: blue;', "Output")),
                 uiOutput(self$ns('show_rv_dataOut'))),
          column(width=4,
                 tags$b(h4(style = 'color: blue;', "status")),
                 uiOutput(self$ns('show_status')))
        )
      )

    },
    
    # SERVER
    server = function(dataIn, 
                      remoteReset=reactive({FALSE}), 
                      isSkipped=reactive({FALSE})) {
      cat(paste0(class(self)[1], '::server(dataIn, remoteReset, isSkippe) from - ', self$id, '\n'))
      self$Additional_Server_Funcs()
      
     observe({
       # This function get the UI definition of the screens for the steps
       # It differs between process class (which screens are given in details in their
       # corresponding source code file) and pipeline class in which the UI is
       # the ui() function from the entire process class
       self$screens <- self$GetScreensDefinition()
       #browser()
       self$timeline.res <-self$CreateTimeline()
       self$timeline.res <- self$timeline$server(
         status = reactive({self$rv$status}),
         dataLoaded = reactive({!is.null(dataIn()) }),
         remoteReset = reactive({remoteReset()})
       )
     })
      
      observeEvent(dataIn(),{self$rv$temp.dataIn <- dataIn()})
      
      observeEvent(self$timeline.res$current.pos(), {
       # browser()
        self$rv$current.pos <- self$timeline.res$current.pos()
        })

      observeEvent(self$timeline.res$tl.reset(),{
        print('local reset')
        self$rv$reset <- self$timeline.res$tl.reset()
        self$ActionOn_Reset()
        })

      
      observeEvent(remoteReset(), ignoreInit = F, { 
        cat(paste0(class(self)[1], '::', 'observeEvent(remoteReset()) from - ', self$id, '\n'))
        print("remote resetd")
        
        # Used to transmit info of local Reset to child processes
        self$rv$reset <- remoteReset()
        self$ActionOn_Reset()
      })
      
      
      
      observeEvent(isSkipped(), { 
        cat(paste0(class(self)[1], '::observeEvent(isSkipped()) from - ', self$id, '\n'))
        if(verbose=='skip') browser()
        self$rv$isSkipped <- isSkipped()
        self$ActionOn_isSkipped()
      })
      
      # MODULE SERVER
      moduleServer(self$id, function(input, output, session) {
        cat(paste0(class(self)[1], '::moduleServer(input, output, session) from - ', self$id, '\n'))
        
        observe({
          self$input <- input
        })
        
        output$show_tl <- renderUI({
          req(self$timeline)
          self$timeline$ui()
        })
        
        ###########---------------------------#################
        output$show_dataIn <- renderUI({
          cat(paste0(class(self)[1], '::output$show_dataIn from - ', self$id, '\n'))
          req(dataIn())
          tagList(
            # h4('show dataIn()'),
            lapply(names(dataIn()), function(x){tags$p(x)})
          )
        })
        
        output$show_rv_dataOut <- renderUI({
          cat(paste0(class(self)[1], '::output$show_rv_dataOut from - ', self$id, '\n'))
          req(self$dataOut$trigger)
          self$dataOut$value
          tagList(
            #h4('show self$dataOut$value'),
            lapply(names(self$dataOut$value), function(x){tags$p(x)})
          )
        })
        
        
      reactive({self$dataOut})
      })
    }
)
)