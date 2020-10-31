ProcessManager <- R6Class(
  "ProcessManager",
  private = list(
    id = NULL,
    timelineForProcess = NULL,
    global = list(VALIDATED = 1,
                  SKIPPED = -1,
                  UNDONE = 1
    ),
    config = reactiveValues() ,
    steps = NULL,
    dataOut = reactiveValues(),
    length = NULL,
    rv = reactiveValues(
      dataIn = NULL,
      current.pos = NULL,
      wake = F,
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
      ind.max <- 0
      indices.validated <- which(private$config$status == private$global$VALIDATED)
      if (length(indices.validated) > 0){
        ind <- which(indices.validated < private$rv$current.pos)
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
      return(private$GetStatusPosition(pos) == private$global$SKIPPED)
    },
    
    InitializeModule = function(){
      private$Initialize_Status_Process()
      private$config$screens <- private$CreateScreens()
      private$rv$current.pos <- 1
    },
    
    
    
    GetCurrentStepName = reactive({ names(config$mandatory)[private$rv$current.pos] }),
    
    Unskip = function(pos){config$status[pos] <- private$global$UNDONE},
    
    GetStatusPosition = function(pos){config$status[pos]},
    # This function cannot be implemented in the timeline module because 
    # the id of the screens to reset are not known elsewhere.
    # Trying to reset the global 'div_screens' in the timeline module
    # does not work
    ResetScreens = function(){
      lapply(1:private$length, function(x){
        shinyjs::reset(NS(private$id)(names(private$config$mandatory)[x]))
      })
    },
    
    ValidateCurrentPos = function(){
      private$config$status[private$rv$current.pos] <- private$global$VALIDATED
      private$Set_Skipped_Status()
      #browser()
      if (private$config$status[private$length] == private$global$VALIDATED)
        # Either the process has been validated, one can prepare data to ben sent to caller
        # Or the module has been reseted
        private$Send_Result_to_Caller()
    },
    InitializeDataIn = function(){ 
      private$rv$dataIn <- private$rv$temp.dataIn
      #private$rv$dataIn <- dataIn()
    },
    
    
    CreateScreens = function(){
      setNames(
        lapply(1:private$length, 
               function(x){
                 do.call(uiOutput, list(outputId=NS(private$id)(private$steps[x])))}),
        private$steps)
    },
    
    
    Initialize_Status_Process = function(){
      private$config$status <- setNames(rep(0, private$length),
                                     names(private$config$status))
    }
  ),
  public = list(
                
                initialize = function(id, config = NULL) {
                  private$id <- id
                  private$steps <- names(config$status)
                  private$length <- length(config$status)
                  lapply(names(config), function(x){private$config[[x]] <- config[[x]]})
                },
                
                
                
                # UI
                ui = function() {
                  ns <- NS(private$id)
                  fluidPage(
                    wellPanel(style="background-color: yellow;",
                              h3('ProcessManager'),
                              uiOutput(ns('show_timeline_ui')),
                              #private$timelineForProcess$ui(),
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
                
                # SERVER
                server = function(dataIn = NULL, 
                                  dataOut = NULL,
                                  remoteReset = FALSE,
                                  isSkipped = FALSE,
                                  logics = NULL) {
                  ns <- NS(private$id)
                  current.pos = reactiveVal()
                  
                  
                  # Catch the new values of the temporary dataOut (instanciated by the last validation button of scrrens
                  # and set the variable which will be read by the caller
                  observeEvent(private$dataOut$trigger, {
                    dataOut$name <- private$dataOut$name
                    dataOut$obj <- private$dataOut$obj
                    dataOut$trigger <- private$dataOut$trigger
                  })
                  

                  observeEvent(req(dataIn()), ignoreNULL=T, ignoreInit = F, { 
                    # This variable serves as a tampon while waiting the user click on the
                    # validate button in the Description screen.
                    # Once done, this variable is observed and the real rv$dataIn function can be
                    # instanciated
                    private$rv$temp.dataIn <- dataIn()
                    
                    # Test if input is NA or not
                    inputExists <- length(dataIn()) > 0
                    
                    #Test if a dataset is already loaded
                    tmpExists <- !is.null(private$rv$dataIn)
                    
                    private$rv$wake <- FALSE
                    
                    if (tmpExists){
                      # this case is either the module is skipped or validated
                      #private$rv$current.pos <- length(private$config$status)
                      private$rv$wake <- private$Wake()
                    } else {
                      if (inputExists){
                        # The current position is pointed on a new module
                        print("launch case 3")
                        private$InitializeModule()
                        InitializeTimeline()
                      } else if (!inputExists){
                        # Initialization of Prostar
                      }
                    }
                    
                  })
                  
                  
                  observe({
                    private$timelineForProcess <- TimelineForProcess$new(
                      id = NS(private$id)('timeline'),
                      mandatory = private$config$mandatory
                    )
                  })
                  
                  
                  InitializeTimeline <- function(){
                    private$rv$timeline.res <- private$timelineForProcess$server(
                      config = private$config,
                      wake = reactive({private$rv$wake}),
                      remoteReset = remoteReset
                    )
                    

                    #Catch a new position from timeline
                    observeEvent(req(private$rv$timeline.res$current.pos()), ignoreInit=T, {
                      private$rv$current.pos <- private$rv$timeline.res$current.pos()
                    })
                    
                    
                    #--- Catch a reset from timeline or caller
                    observeEvent(req(c(private$rv$timeline.res$reset()!=0, remoteReset()!=0)), {
                      private$ResetScreens()
                      private$rv$dataIn <- NA
                      private$Initialize_Status_Process()
                      private$Send_Result_to_Caller()
                      private$InitializeDataIn()
                    })
                  }
                  
                  
                  observeEvent(isSkipped(), ignoreInit = T, {
                    if (isSkipped())
                      tag <- private$global$SKIPPED
                    else
                      tag <- private$global$UNDONE
                    
                    private$config$status <- setNames(rep(tag, private$length),
                                                   names(private$config$status))
                  })
                  
                  # MODULE SERVER
                  moduleServer(private$id, function(input, output, session) {
                    
                    # TODO In a script for dev, write a test function to check the validity of the logics for the new processLogics
                    # CheckLogics = function(FUN){
                    #   FUN(self, input, output)
                    #   out <- names(outputOptions(output))
                    #   otherUI <- c("ProcessManager-show_dataIn",
                    #                "ProcessManager-show_rv_dataIn" , 
                    #                "ProcessManager-show_rv_dataOut",
                    #                "ProcessManager-show_status",
                    #                "show_ui")
                    #   out <- out[-which(!is.na(match(out, otherUI)))]
                    # 
                    #   browser()
                    #   outInStatusNotInOut <- names(config$status)[which( NS(private$id)(names(config$status)) %in% out == F)]
                    #   outInOutNotInStatus <- out[which( out %in% NS(private$id)(names(config$status)) == FALSE)]
                    #   
                    #   list(outInStatusNotInOut = outInStatusNotInOut,
                    #        outInOutNotInStatus = outInOutNotInStatus
                    #        )
                    # }
                    # 
                    # observeEvent(logics, {
                    #   missingUI <- CheckLogics(ProcessLogics)
                    #   if (length(missingUI)>0){
                    #     warning(paste0("Your logics function is malformed. The following renderUI functions are missing:", 
                    #                    paste0(missingUI, collapse=' ')))
                    #     return(NULL)
                    #   }
                      logics(private, input, output)
                    
                    output$show_timeline_ui <- renderUI({
                      private$timelineForProcess$ui()
                    })

                    
                    ###########---------------------------#################
                    output$show_dataIn <- renderUI({
                      req(dataIn())
                      tagList(lapply(names(dataIn()), function(x){tags$p(x)}))
                    })
                    
                    output$show_rv_dataIn <- renderUI({
                      tagList(lapply(names(private$rv$dataIn), function(x){tags$p(x)}))
                    })
                    
                    output$show_rv_dataOut <- renderUI({
                      req(dataOut$trigger)
                      tagList(
                        lapply(names(dataOut$obj), function(x){tags$p(x)})
                      )
                    })

                    output$show_status <- renderUI({
                      req(private$config$status, private$rv$current.pos)
                      tagList(lapply(1:private$length, 
                                     function(x){if (x == private$rv$current.pos) tags$p(tags$b(paste0('-> ',names(private$config$status)[x], ' - ', private$GetStringStatus(private$config$status[[x]]))))
                                       else tags$p(paste0(names(private$config$status)[x], ' - ', private$GetStringStatus(private$config$status[[x]])))
                                     }))
                    })
                   
                    #################################################
                    # Main listener of the module which initialize it
                    
                    
                    
                    
                    GetValidationBtnIds <- reactive({validated.btns <- grep('_validate_btn', names(input))})

                  }
                  ) }
  )
)