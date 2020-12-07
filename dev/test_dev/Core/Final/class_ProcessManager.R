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
    child.process = NULL,
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
        trigger = 0
      )
      
      self$rv = reactiveValues(
        dataIn = NULL,
        temp.dataIn = NULL,
        current.pos = 1,
        status = NULL,
        isReseted = NULL,
        isSkipped = NULL)
      
      check <- self$CheckConfig(private$.config)
      if (!check$passed)
        stop(paste0("Errors in 'config'", paste0(check$msg, collapse=' ')))
      else
        self$config <- private$.config
      
      self$length <- length(self$config$mandatory)
      self$config$type = class(self)[2]
      self$config$mandatory <- setNames(self$config$mandatory, self$config$steps)
      
      self$rv$status <- setNames(rep(global$UNDONE, self$length), self$config$steps)
      
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
    
    Timestamp = function(){ 
      cat(paste0(class(self)[1], '::Timestamp() from - ', self$id, '\n'))
      as.numeric(Sys.time())
    },
    
    Send_Result_to_Caller = function(){
      cat(paste0(class(self)[1], '::Send_Result_to_Caller() from - ', self$id, '\n'))
      #self$dataOut$value <- self$rv$dataIn
      self$dataOut$trigger <- self$Timestamp()
      #self$dataOut$name <- self$name
    },
    Get_Result = function(){
      self$rv$dataIn
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
    
   
    Initialize_Status_Process = function(){
      cat(paste0(class(self)[1], '::', 'Initialize_Status_Process() from - ', self$id, '\n'))
      self$rv$status <- setNames(rep(global$UNDONE, self$length),self$config$steps)
    },
    
    ActionOn_New_DataIn = function(){},
    ActionOn_NewPosition = function(){},
    
    ActionOn_Reset = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnReset() from - ', self$id, '\n'))
      browser()
      
      self$ResetScreens()
      self$rv$dataIn <- NULL
      self$Initialize_Status_Process()
      self$Send_Result_to_Caller()
      #self$InitializeDataIn()
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
    

    ActionOn_isSkipped = function(){},
    
    SetSkipped = function(skip){self$rv$isSkipped <- skip},
    SetReseted = function(reset){self$rv$isReseted <- reset},
    
    ValidateCurrentPos = function(){
      cat(paste0(class(self)[1], '::', 'ValidateCurrentPos() from - ', self$id, '\n'))
      #if(verbose=='skip')
       # browser()
      self$rv$status[self$rv$current.pos] <- global$VALIDATED
      self$Discover_Skipped_Status()
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
    server = function(dataIn = reactive({NULL})) {
      cat(paste0(class(self)[1], '::server(dataIn) from - ', self$id, '\n'))
      
     observe({
       cat(paste0(class(self)[1], '::observe() from - ', self$id, '\n'))
       #browser()
       isolate({
         self$Additional_Server_Funcs()
       
       # This function get the UI definition of the screens for the steps
       # It differs between process class (which screens are given in details in their
       # corresponding source code file) and pipeline class in which the UI is
       # the ui() function from the entire process class
       self$screens <- self$GetScreensDefinition()
       #browser()
       self$CreateTimeline()
       })
       
       self$timeline.res <- self$timeline$server(
         status = reactive({self$rv$status}))
     })
      
      observeEvent(dataIn(), ignoreNULL = F, {
        cat(paste0(class(self)[1], '::observeEvent(dataIn()) from --- ', self$id, '\n'))
        #self$rv$current.pos <- 1
        self$timeline$Change_Current_Pos(1)
        self$rv$temp.dataIn <- dataIn()
        self$ActionOn_New_DataIn()
        })
      
      observeEvent(self$timeline.res$current.pos(), {
       # browser()
        self$rv$current.pos <- self$timeline.res$current.pos()
        self$ActionOn_NewPosition()
        })


      observeEvent(self$rv$isReseted, ignoreInit = F, { 
        cat(paste0(class(self)[1], '::', 'observeEvent(c(self$rv$isReseted) from -- ', self$id, ' --\n'))
        browser()
        
        if (!is.null(self$child.process))
          lapply(self$config$steps, function(x){
            self$child.process[[x]]$SetReseted(self$rv$isReseted)
          })
        
        self$ActionOn_Reset()
      })
      
      observeEvent(req(!is.null(self$timeline.res$tl.reset())), ignoreInit = F, { 
        cat(paste0(class(self)[1], '::', 'observeEvent(self$timeline.res$tl.reset() from - ', self$id, '\n'))
        browser()
        
        if (!is.null(self$child.process))
          lapply(self$config$steps, function(x){
            self$child.process[[x]]$SetReseted(self$timeline.res$tl.reset())
          })
        
        self$ActionOn_Reset()
      })
      
      
      
      observeEvent(req(!is.null(self$rv$isSkipped)), ignoreNULL=F, { 
        cat(paste0(class(self)[1], '::observeEvent(isSkipped()) from - ', self$id, '\n'))
        #if(verbose=='skip') 
        
        print(self$rv$isSkipped)
         # browser()
        self$ActionOn_isSkipped()
      })
      
      # MODULE SERVER
      moduleServer(self$id, function(input, output, session) {
        cat(paste0(class(self)[1], '::moduleServer(input, output, session) from - ', self$id, '\n'))
        
        observe({self$input <- input})
        
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
          self$dataOut$trigger
          tagList(
            #h4('show self$dataOut$value'),
            lapply(names(self$rv$dataIn), function(x){tags$p(x)})
          )
        })
        
        
      reactive({self$dataOut})
      })
    }
)
)