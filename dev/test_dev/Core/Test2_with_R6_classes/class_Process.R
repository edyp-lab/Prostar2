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
    initialize = function(id, config=NULL) {
      private$id <- id
      private$steps <- names(config$status)
      private$length <- length(config$status)
      lapply(names(config), function(x){private$config[[x]] <- config[[x]]})
      private$Initialize_Status_Process()
    }
    
  )
)