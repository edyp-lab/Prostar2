#Timeline_R6.R
Process = R6Class(
  "Process",
  inherit = ProcessManager,
  private = list(),
  
  public = list(
    
    ActionsOnNewPosition = function(){},
    
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