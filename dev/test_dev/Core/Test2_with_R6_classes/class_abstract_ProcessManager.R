ProcessManager <- R6Class(
  "ProcessManager",
  private = list(
    id = NULL,
    timeline = NULL,
    global = list(VALIDATED = 1,
                  SKIPPED = -1,
                  UNDONE = 1
    ),
    #config = reactiveValues() ,
    steps = NULL,
    dataOut = reactiveValues(),
    length = NULL,
    
    rv = reactiveValues(
      dataIn = NULL,
      current.pos = NULL,
      wake = F,
      reset = NULL,
      isSkipped = FALSE,
      timeline.res = NULL),
    
    Wake = function(){ runif(1,0,1)},
    
    GetStringStatus = function(status){
      if (status==private$global$VALIDATED) "Validated"
      else if (status==private$global$UNDONE) "Undone"
      else if (status==private$global$SKIPPED) 'Skipped'
    },
    
    Send_Result_to_Caller = function(){
      private$rv$wake <- private$Wake()
      
      private$dataOut$obj <- private$rv$dataIn
      private$dataOut$name <- private$config$process.name
      private$dataOut$trigger <- private$rv$wake
    },
    
    Set_Skipped_Status = function(){
      for (i in 1:private$length)
        if (private$config$status[i] != private$global$VALIDATED && private$GetMaxValidated_AllSteps() > i)
          private$config$status[i] <- private$global$SKIPPED
    },
    
    GetMaxValidated_AllSteps = function(){
      val <- 0
      ind <- which(private$config$status == private$global$VALIDATED)
      if (length(ind) > 0)
        val <- max(ind)
      val
    },
    
    GetMaxValidated_BeforeCurrentPos = function(){
      ind.max <- NULL
      indices.validated <- which(private$config$status == private$global$VALIDATED)
      if (length(indices.validated) > 0){
        ind <- which(indices.validated < private$rv$current.pos)
        if(length(ind) > 0)
          ind.max <- max(ind)
      }
      
      # if (ind.max == 0)
      #   ind.max <- 1
      # 
      ind.max
    },
    
    
    # Test if a process module (identified by its name) has been skipped.
    # This function is called each time the list config$isDone is updated
    # because one can know the status 'Skipped' only when a further module
    # has been validated
    is.skipped = function(name){
      pos <- which(name == private$config$steps)
      return(private$GetStatusPosition(pos) == private$global$SKIPPED)
    },
    
    InitializeModule = function(){
      private$config$screens <- private$CreateScreens()
      private$rv$current.pos <- 1
    },
    
    ActionsOnNewPosition = function(){},
    
    
    ActionsOnIsSkipped = function(){},
    
    GetCurrentStepName = function(){
      private$config$steps[private$rv$current.pos]
    },
    
    Unskip = function(pos){
      private$config$status[pos] <- private$global$UNDONE
    },
    
    GetStatusPosition = function(pos){
      private$config$status[pos]
    },
    
    # This function cannot be implemented in the timeline module because 
    # the id of the screens to reset are not known elsewhere.
    # Trying to reset the global 'div_screens' in the timeline module
    # does not work
    ResetScreens = function(){
      lapply(1:private$length, function(x){
        shinyjs::reset(NS(private$id)(private$config$steps[x]))
      })
    },
    
    ActionsOnDataTrigger = function(data){
      data$name <- private$dataOut$name
      data$obj <- private$dataOut$obj
      data$trigger <- private$dataOut$trigger
      data
    },
    
    ValidateCurrentPos = function(){
      private$config$status[private$rv$current.pos] <- private$global$VALIDATED
      private$Set_Skipped_Status()

      if (private$config$status[private$length] == private$global$VALIDATED)
        # Either the process has been validated, one can prepare data to be sent to caller
        # Or the module has been reseted
        private$Send_Result_to_Caller()
    },
    
    InitializeDataIn = function(){ 
      private$rv$dataIn <- private$rv$temp.dataIn
    },
    
    
    CreateScreens = function(){
      
      setNames(
        lapply(1:private$length, 
               function(x){
                 do.call(uiOutput, list(outputId=NS(private$id)(private$config$steps[x])))}),
        private$config$steps)
    },
    
    
    Initialize_Status_Process = function(){
      private$config$status <- setNames(rep(0, private$length),
                                        private$config$steps)
    },
    
    
    #Actions onf receive new dataIn()
    ActionsOn_Tmp_NoInput = function(){
      private$rv$wake <- private$Wake()},
    
    ActionsOn_Tmp_Input = function(){
      private$rv$wake <- private$Wake()
    },
    
    ActionsOn_NoTmp_Input = function(){
      private$InitializeModule()
      private$InitializeTimeline()
    },
    
    ActionsOn_NoTmp_NoInput = function(){},
    
    ActionsOnNewDataIn = function(data){
      # This variable serves as a tampon while waiting the user click on the
      # validate button in the Description screen.
      # Once done, this variable is observed and the real rv$dataIn function can be
      # instanciated
      private$rv$temp.dataIn <- data
      
      private$rv$wake <- FALSE
      
      # Test if input is NA or not
      inputExists <- length(data) > 0
      
      #Test if a dataset is already loaded
      tmpExists <- !is.null(private$rv$dataIn)
      
      
      if (tmpExists && inputExists){
        # this case is either the module is skipped or validated
        #private$rv$current.pos <- length(private$config$status)
        private$ActionsOn_Tmp_Input()
      } else if (tmpExists && !inputExists) {
        private$ActionsOn_Tmp_Input()
      } else if (!tmpExists && inputExists){
        # The current position is pointed on a new module
        #private$ActionsOn_NoTmp_Input()
        private$ActionsOn_NoTmp_Input()
      } else if (!tmpExists && !inputExists){
        # Initialization of Prostar
        private$ActionsOn_NoTmp_NoInput()
      }
    },
    
    CreateTimeline = function(){
      private$timeline <- TimelineForProcess$new(
        id = NS(private$id)('timeline'),
        mandatory = private$config$mandatory
      )
     },
    
    InitializeTimeline = function(){
      print(paste0("in InitializeTimeline for", private$id))
 
       private$CreateTimeline()
      private$rv$timeline.res <- private$timeline$server(
        config = private$config,
        wake = reactive({private$rv$wake}),
        remoteReset = reactive({private$rv$remoteReset})
      )
      
      observeEvent(req(private$rv$timeline.res()), ignoreInit=T, {
        private$rv$current.pos <- private$rv$timeline.res()$current.pos
        private$rv$reset <- private$rv$timeline.res()$reset
      })
    },
    
    # This function adds the renderUI functions of the screens.
    # In the process child class, these functions are written in a separate file for each process
    # One just have to call them by setting the variable logics (which is the name of the function
    # containing the renderUIs
    # In the pipeline child class, these functions are dynamically created 
    Add_RenderUIs_Definitions = function(input, output){},
    
    TimelineUI = function(){
     # req(private$timeline)
      print("timeline$ui()")
      private$timeline$ui()
    }
    
  ),
  public = list(
    
    initialize = function() {
      stop(" ProcessManager is an abstract class that can't be initialized.")
    },

    GetConfig = function(){
      req(private$config)
      browser()
      print(paste0('-----GetConfig(', private$id, ') : ', paste0(private$config$steps, collapse=' ')))
    },
    
    # UI
    ui = function() {
      ns <- NS(private$id)
      fluidPage(
        wellPanel(style="background-color: yellow;",
                  uiOutput(ns('title')),
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
                  )
        )
      )
    },
    
    # SERVER
    server = function(dataIn = NULL, 
                      dataOut = NULL,
                      remoteReset = FALSE,
                      isSkipped = FALSE) {
      ns <- NS(private$id)
      current.pos = reactiveVal()
     
      # Catch the new values of the temporary dataOut (instanciated by the last validation button of screens
      # and set the variable which will be read by the caller
      observeEvent(private$dataOut$trigger, {
        dataOut <- private$ActionsOnDataTrigger(dataOut)
        
      })
      
      
      observeEvent(req(dataIn()), ignoreNULL=T, ignoreInit = F, { 
        
        print(paste0("############# recu un dataIn sur le process", private$id))
        private$ActionsOnNewDataIn(dataIn())
      })
      
      
      observe({private$CreateTimeline()})
      
      observeEvent(req(private$rv$current.pos), ignoreInit=T, {
        private$ActionsOnNewPosition()
      })
      
      observeEvent(req(remoteReset()!=0), { private$rv$remoteReset <- remoteReset()})
      
      
      ActionsOnReset = function(){
        private$ResetScreens()
        private$rv$dataIn <- NA
        private$Initialize_Status_Process()
        private$Send_Result_to_Caller()
        private$InitializeDataIn()
      }
      
      #--- Catch a reset from timeline or caller
      observeEvent(req(c(private$rv$reset, private$rv$remoteReset)), {
        ActionsOnReset()
      })
      
      observeEvent(isSkipped(), ignoreInit = T, { 
        private$rv$isSkipped <- isSkipped()
        private$ActionsOnIsSkipped()
      })
      
      # MODULE SERVER
      moduleServer(private$id, function(input, output, session) {
        print(paste0("## in moduleServer for id = ", private$id))
        # TODO In a script for dev, write a test function to check the validity of the logics for the new processLogics
        
        private$Add_RenderUIs_Definitions( input, output)
        
        output$show_timeline_ui <- renderUI({private$TimelineUI() })
        
        
        ###########---------------------------#################
        output$show_dataIn <- renderUI({
          req(dataIn())
          tagList(lapply(names(dataIn()), function(x){tags$p(x)}))
        })
        
        output$show_rv_dataIn <- renderUI({
          tagList(lapply(names(private$rv$dataIn), function(x){tags$p(x)}))
        })
        
        output$show_rv_dataOut <- renderUI({
          req(private$dataOut$trigger)
          tagList(
            lapply(names(private$dataOut$obj), function(x){tags$p(x)})
          )
        })
        
        output$show_status <- renderUI({
          req(private$config$status, private$rv$current.pos)
          tagList(lapply(1:private$length, 
                         function(x){if (x == private$rv$current.pos) 
                           tags$p(tags$b(paste0('-> ', private$config$steps[x], ' - ', private$GetStringStatus(private$config$status[[x]]))))
                           else 
                             tags$p(paste0(private$config$steps[x], ' - ', private$GetStringStatus(private$config$status[[x]])))
                         }))
        })
        
        output$title <- renderUI({ h3(paste0('private$id = ',private$id)) })
        
        #################################################
        # Main listener of the module which initialize it
        
        
        
        
        GetValidationBtnIds <- reactive({validated.btns <- grep('_validate_btn', names(input))})
        
      }
      ) }
  )
)