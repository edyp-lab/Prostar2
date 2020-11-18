#Timeline_R6.R
Process = R6Class(
  "Process",
  inherit = ProcessManager,
  private = list(),
  
  public = list(
    
    CreateTimeline = function(){
      cat(paste0(class(self)[1], '::', 'CreateTimeline()\n'))
      self$timeline <- TimelineForProcess$new(
        id = NS(self$id)('timeline'),
        mandatory = self$config$mandatory
      )
    },
    
    ActionsOnNewPosition = function(){},
    
    ActionsOnReset = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnReset()\n'))
      browser()
      self$ResetScreens()
      self$rv$dataIn <- NA
      self$Initialize_Status_Process()
      self$Send_Result_to_Caller()
      self$InitializeDataIn()
    },
    
    ActionsOn_NoTmp_Input = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOn_NoTmp_Input()\n'))
      self$InitializeModule()
      self$InitializeTimeline()
    },
    
    ActionsOnIsSkipped = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnIsSkipped()\n'))
      if (self$rv$isSkipped)
        tag <- self$global$SKIPPED
      else
        tag <- self$global$UNDONE
      
      self$config$status <- setNames(rep(tag, self$length),
                                     self$config$steps)
    }

  )
)