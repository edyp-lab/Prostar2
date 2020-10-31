ProcessManager <- R6Class(
  "ProcessManager",
  private = list(
    timelineForProcess = NULL,
    global = list(VALIDATED = 1,
                  SKIPPED = -1,
                  UNDONE = 1
    ),
    Wake = function(){ runif(1,0,1)},
    
    GetStringStatus = function(status){
      if (status==private$global$VALIDATED) "Validated"
      else if (status==private$global$UNDONE) "Undone"
      else if (status==private$global$SKIPPED) 'Skipped'
    },
    
    Send_Result_to_Caller = function(){
      self$rv$wake <- private$Wake()
      
      self$dataOut$obj <- self$rv$dataIn
      self$dataOut$name <- self$config$process.name
      self$dataOut$trigger <- self$rv$wake
    },
    
    Set_Skipped_Status = function(){
      for (i in 1:length(self$config$mandatory))
        if (self$config$status[i] != private$global$VALIDATED && private$GetMaxValidated_AllSteps() > i)
          self$config$status[i] <- private$global$SKIPPED
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
      return(private$GetStatusPosition(pos) == private$global$SKIPPED)
    },
    
    InitializeModule = function(){
      private$Initialize_Status_Process()
      self$config$screens <- private$CreateScreens()
      self$rv$current.pos <- 1
    },
    
    InitializeDataIn = function(){ 
      self$rv$dataIn <- self$rv$temp.dataIn
      #self$rv$dataIn <- dataIn()
    },
    
    GetCurrentStepName = reactive({ names(config$mandatory)[self$rv$current.pos] }),
    
    Unskip = function(pos){config$status[pos] <- private$global$UNDONE},
    
    GetStatusPosition = function(pos){config$status[pos]},
    # This function cannot be implemented in the timeline module because 
    # the id of the screens to reset are not known elsewhere.
    # Trying to reset the global 'div_screens' in the timeline module
    # does not work
    ResetScreens = function(){
      lapply(1:length(self$config$status), function(x){
        shinyjs::reset(NS(self$id)(names(self$config$mandatory)[x]))
      })
    },
    
    CreateScreens = function(){
      setNames(
        lapply(1:length(self$steps), 
               function(x){
                 do.call(uiOutput, list(outputId=NS(self$id)(self$steps[x])))}),
        self$steps)
    },
    
    
    Initialize_Status_Process = function(){
      self$config$status <- setNames(rep(0,length(self$config$status)),
                                     names(self$config$status))
    }
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
                
               
                
                
                # Cannot be a private function because it is used by functions
                # in the process logics code
                ValidateCurrentPos = function(){
                  self$config$status[self$rv$current.pos] <- private$global$VALIDATED
                  private$Set_Skipped_Status()
                  #browser()
                  if (self$config$status[length(self$config$status)] == private$global$VALIDATED)
                    # Either the process has been validated, one can prepare data to ben sent to caller
                    # Or the module has been reseted
                    private$Send_Result_to_Caller()
                },
                
                
                # SERVER
                server = function(dataIn = NULL, 
                                  dataOut = NULL,
                                  remoteReset = FALSE,
                                  isSkipped = FALSE,
                                  logics = NULL) {
                  ns <- NS(self$id)
                  current.pos = reactiveVal()
                  
                  
                  # Catch the new values of the temporary dataOut (instanciated by the last validation button of scrrens
                  # and set the variable which will be read by the caller
                  observeEvent(self$dataOut$trigger, {
                    dataOut$name <- self$dataOut$name
                    dataOut$obj <- self$dataOut$obj
                    dataOut$trigger <- self$dataOut$trigger
                  })
                  
                  
                  
                  
                  observeEvent(req(dataIn()), ignoreNULL=T, ignoreInit = F, { 
                    
                    # This variable serves as a tampon while waiting the user click on the
                    # validate button in the Description screen.
                    # Once done, this variable is observed and the real rv$dataIn function can be
                    # instanciated
                    self$rv$temp.dataIn <- dataIn()
                    
                    # Test if input is NA or not
                    inputExists <- length(self$config$status) > 0
                    
                    #Test if a dataset is already loaded
                    tmpExists <- !is.null(self$rv$dataIn)
                    
                    self$rv$wake <- FALSE
                    
                    if (tmpExists){
                      # this case is either the module is skipped or validated
                      #self$rv$current.pos <- length(self$config$status)
                      self$rv$wake <- private$Wake()
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
                      private$ResetScreens()
                      self$rv$dataIn <- NA
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
                    
                    self$config$status <- setNames(rep(tag,length(self$config$status)),
                                                   names(self$config$status))
                  })
                  
                  # MODULE SERVER
                  moduleServer(self$id, function(input, output, session) {
                    
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
                    #   outInStatusNotInOut <- names(config$status)[which( NS(self$id)(names(config$status)) %in% out == F)]
                    #   outInOutNotInStatus <- out[which( out %in% NS(self$id)(names(config$status)) == FALSE)]
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
                      logics(self, input, output)
                    
                      output$show_timeline_ui <- renderUI({
                        req(private$timelineForProcess)
                        private$timelineForProcess$ui()
                     # })
                      
                    })
                    
                    
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

                    output$show_status <- renderUI({
                      req(self$config$status, self$rv$current.pos)
                      tagList(lapply(1:length(self$config$status), 
                                     function(x){if (x == self$rv$current.pos) tags$p(tags$b(paste0('-> ',names(self$config$status)[x], ' - ', private$GetStringStatus(self$config$status[[x]]))))
                                       else tags$p(paste0(names(self$config$status)[x], ' - ', private$GetStringStatus(self$config$status[[x]])))
                                     }))
                    })
                   
                    #################################################
                    # Main listener of the module which initialize it
                    
                    
                    
                    
                    GetValidationBtnIds <- reactive({validated.btns <- grep('_validate_btn', names(input))})

                  }
                  ) }
  )
)