#Timeline_R6.R
Process = R6Class(
  "Process",
  inherit = ProcessManager,
  private = list(
    ActionsOnNewPosition = function(){},
    
    ActionsOnIsSkipped = function(){
      if (private$rv$isSkipped)
        tag <- private$global$SKIPPED
      else
        tag <- private$global$UNDONE
      
      private$config$status <- setNames(rep(tag, private$length),
                                        config$steps)
    }

    
  ),
  
  public = list(
    initialize = function(){
      stop(" Process is an abstract class that can't be initialized.")
    }
   
    
  )
)