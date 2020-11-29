#Timeline_R6.R
Process = R6Class(
  "Process",
  inherit = ProcessManager,
  private = list(
    ActionsOnNewPosition = function(){},
    
    ActionsOnIsSkipped = function(){
      if (private$GetIsSkipped())
        tag <- private$global$SKIPPED
      else
        tag <- private$global$UNDONE
      
      private$InitStatus()
    }

    
  ),
  
  public = list(
    initialize = function(){
      stop(" Process is an abstract class that can't be initialized.")
    }
   
    
  )
)