ProcessManager <- R6Class(
  "ProcessManager",
  private = list(
    id = NULL,
    dataOut = reactiveValues(),
    config = reactiveValues(),
    timeline = NULL,
    global = list(VALIDATED = 1,
                  SKIPPED = -1,
                  UNDONE = 1
    ),
    
    rv = reactiveValues(
      dataIn = NULL,
      current.pos = NULL,
      wake = F,
      reset = NULL,
      isSkipped = FALSE,
      timeline.res = NULL),
    
    GetRemoteReset = function(){private$rv[[private$id]]$remoteReset},
    SetRemoteReset = function(value){private$rv[[private$id]]$remoteReset <- value},
    GetDataIn = function(){private$rv[[private$id]]$dataIn},
    SetDataIn = function(value){private$rv[[private$id]]$dataIn <- value},
    GetTempDataIn = function(){private$rv[[private$id]]$temp.dataIn},
    SetTempDataIn = function(value){private$rv[[private$id]]$temp.dataIn <- value},
    GetCurrentPos = function(){private$rv[[private$id]]$current.pos},
    SetCurrentPos = function(value){private$rv[[private$id]]$current.pos <- value},
    GetWake = function(){private$rv[[private$id]]$wake},
    SetWake= function(value){private$rv[[private$id]]$wake <- value},
    GetReset = function(){private$rv[[private$id]]$reset},
    SetReset= function(value){private$rv[[private$id]]$reset <- value},
    GetIsSkipped = function(){private$rv[[private$id]]$isSkipped},
    SetIsSkipped = function(value){private$rv[[private$id]]$isSkipped <- value},
    GetTimelineRes = function(){private$rv[[private$id]]$timeline.res},
    SetTimelineRes = function(value){private$rv[[private$id]]$timeline.res <- value},
    
    
    
    GetConfig = function(){private$config[[private$id]]},
    SetConfig = function(value){private$config[[private$id]] <- value},
    GetSteps = function(){private$config[[private$id]]$steps},
    SetSteps = function(pos, value){private$config[[private$id]]$steps[pos] <- value},
    GetStatus = function(pos){private$config[[private$id]]$status[pos]},
    SetStatus = function(pos, value){private$config[[private$id]]$status[pos] <- value},
    GetCompleteStatus = function(){private$config[[private$id]]$status},
    SetCompleteStatus = function( value){private$config[[private$id]]$status <- value},
    GetMandatory = function(){private$config[[private$id]]$mandatory},
    SetMandatory = function(pos, value){private$config[[private$id]]$mandatory[pos] <- value},
    GetName = function(){private$config[[private$id]]$name},
    SetName = function(value){private$config[[private$id]]$name <- value},
    GetScreens = function(){private$config[[private$id]]$screens},
    SetScreens = function(value){private$config[[private$id]]$screens <- value},
    GetDataOut = function() {private$dataOut[[private$id]]},
    SetDataOut = function(data){private$dataOut[[private$id]] <- data},
    Length = function(){length(private$config[[private$id]]$steps)},
    InitStatus = function(){isolate({private$config[[private$id]]$status <- setNames(rep(0, private$Length()), private$GetSteps())})},
      
      
    Wake = function(){ runif(1,0,1)},
    
    GetStringStatus = function(status){
      if (status==private$global$VALIDATED) "Validated"
      else if (status==private$global$UNDONE) "Undone"
      else if (status==private$global$SKIPPED) 'Skipped'
    },
    
    Send_Result_to_Caller = function(){
      private$SetWake(private$Wake())
      private$SetDataOut(list(obj=private$GetDataIn(),
                           trigger = private$GetWake())
      )
    },
    
    Set_Skipped_Status = function(){
      for (i in 1:private$Length())
        if (private$GetStatus(i) != private$global$VALIDATED && private$GetMaxValidated_AllSteps() > i)
          private$SetStatus(i, private$global$SKIPPED)
    },
    
    GetMaxValidated_AllSteps = function(){
      val <- 0
      ind <- which(private$GetCompleteStatus() == private$global$VALIDATED)
      if (length(ind) > 0)
        val <- max(ind)
      val
    },
    
    GetMaxValidated_BeforeCurrentPos = function(){
      ind.max <- NULL
      indices.validated <- which(private$GetStatus() == private$global$VALIDATED)
      if (length(indices.validated) > 0){
        ind <- which(indices.validated < private$GetCurrentPos())
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
      pos <- which(name == private$GetSteps())
      return(private$GetStatus(pos) == private$global$SKIPPED)
    },
    
    InitializeModule = function(){
      browser()
      private$SetScreens(private$CreateScreens())
      private$SetCurrentPos(1)
    },
    
    ActionsOnNewPosition = function(){},
    
    
    ActionsOnIsSkipped = function(){
      private$SetSkipped(isSkipped())
    },
    
    GetCurrentStepName = function(){
      private$GetSteps()[private$GetCurrentPos()]
    },
    
    Unskip = function(pos){
      private$SetStatus(pos, private$global$UNDONE)
    },

    
    # This function cannot be implemented in the timeline module because 
    # the id of the screens to reset are not known elsewhere.
    # Trying to reset the global 'div_screens' in the timeline module
    # does not work
    ResetScreens = function(){
      lapply(1:private$Length(), function(x){
        shinyjs::reset(NS(private$id)(private$GetSteps()[x]))
      })
    },
    
    ActionsOnDataTrigger = function(){
      list(obj = private$GetDataOut()$obj,
              trigger = private$GetDataOut()$trigger)
    },
    
    ValidateCurrentPos = function(){
      private$SetStatus(private$GetCurrentPos(), private$global$VALIDATED)
      private$Set_Skipped_Status()

      if (private$GetStatus()[private$Length()] == private$global$VALIDATED)
        # Either the process has been validated, one can prepare data to be sent to caller
        # Or the module has been reseted
        private$Send_Result_to_Caller()
    },
    
    InitializeDataIn = function(){ 
      private$SetDataIn(private$GetTempDataIn())
    },
    
    
    CreateScreens = function(){
      setNames(
        lapply(1:private$Length(), 
               function(x){
                 do.call(uiOutput, list(outputId=NS(private$id)(private$GetSteps()[x])))}),
        private$GetSteps())
    },
 
    #Actions on receive new dataIn()
    ActionsOn_Tmp_NoInput = function(){
      private$SetWake(private$Wake())},
    
    ActionsOn_Tmp_Input = function(){
      private$SetWake(private$Wake())
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
      private$SetTempDataIn(data)
      
      private$SetWake(FALSE)
      
      # Test if input is NA or not
      inputExists <- length(data) > 0
      
      #Test if a dataset is already loaded
      tmpExists <- !is.null(private$GetDataIn())
      
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
        mandatory = private$GetMandatory()
      )
     },
    
    InitializeTimeline = function(){
      print(paste0("in InitializeTimeline for", private$id))

       
      private$SetTimelineRes(private$timeline$server(
        config = private$GetConfig(),
        wake = reactive({private$GetWake()}),
        remoteReset = reactive({private$GetRemoteReset()})
      )
      )
      
      observeEvent(req(private$GetRemoteReset()), ignoreInit=T, {
        private$SetCurrentPos(private$GetRemoteReset()$current.pos)
        private$SetReset(private$GetRemoteReset()$reset)
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
      print("launch timeline$ui()")
      private$timeline$ui()
    }
    
  ),
  public = list(
    
    initialize = function() {
      stop(" ProcessManager is an abstract class that can't be initialized.")
    },

    
    # UI
    ui = function() {
      ns <- NS(private$id)
      fluidPage(
        wellPanel(style="background-color: yellow;",
                  p('abstract ProcessManager'),
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
                      remoteReset = FALSE,
                      isSkipped = FALSE) {
      ns <- NS(private$id)
     
      # Catch the new values of the temporary dataOut (instanciated by the last validation button of screens
      # and set the variable which will be read by the caller
      observeEvent(private$GetDataOut()$trigger, {
        private$SetDataOut(private$ActionsOnDataTrigger())
      })
      
      
      observeEvent(req(dataIn()), ignoreNULL=T, ignoreInit = F, { 
        
        print(paste0("############# recu un dataIn sur le process", private$id))
        private$ActionsOnNewDataIn(dataIn())
      })
      
      
      observe({private$CreateTimeline()})
      
      observeEvent(req(private$GetCurrentPos()), ignoreInit=T, {
        private$ActionsOnNewPosition()
      })
      
      observeEvent(req(remoteReset()!=0), { 
        private$SetRemoteReset(remoteReset())
        })
      
      
      ActionsOnReset = function(){
        private$ResetScreens()
        private$SetDataIn(NA)
        private$InitStatus()
        private$Send_Result_to_Caller()
        private$InitializeDataIn()
      }
      
      #--- Catch a reset from timeline or caller
      observeEvent(req(c(private$GetReset(), private$GetRemoteReset())), {
        ActionsOnReset()
      })
      
      observeEvent(isSkipped(), ignoreInit = T, { 
        private$ActionsOnIsSkipped()
      })
      
      # MODULE SERVER
      moduleServer(private$id, function(input, output, session) {
        print(paste0("## in moduleServer for id = ", private$id))
        # TODO In a script for dev, write a test function to check the validity of the logics for the new processLogics
        
        private$Add_RenderUIs_Definitions( input, output)
        
        output$show_timeline_ui <- renderUI({private$TimelineUI() })
        
        output$show_dataIn <- renderUI({
          req(dataIn())
          tagList(lapply(names(dataIn()), function(x){tags$p(x)}))
        })
        
        output$show_rv_dataIn <- renderUI({
          tagList(lapply(names(private$GetDataIn()), function(x){tags$p(x)}))
        })
        
        output$show_rv_dataOut <- renderUI({
          req(private$GetDataOut()$trigger)
          tagList(
            lapply(names(private$GetDataOut()$obj), function(x){tags$p(x)})
          )
        })
        
        output$show_status <- renderUI({
          req(private$GetStatus(), private$GetCurrentPos())
          tagList(lapply(1:private$Length(), 
                         function(x){if (x == private$GetCurrentPos()) 
                           tags$p(tags$b(paste0('-> ', private$GetSteps()[x], ' - ', private$GetStringStatus(private$GetStatus()[x]))))
                           else 
                             tags$p(paste0(private$GetSteps()[x], ' - ', private$GetStringStatus(private$GetStatus()[x])))
                         }))
        })
        
        output$title <- renderUI({ h3(paste0('private$id = ', private$id)) })
        
        GetValidationBtnIds <- reactive({validated.btns <- grep('_validate_btn', names(input))})
        
      }
      )
      reactive({dataOut})}
  )
)