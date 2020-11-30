#Timeline_R6.R
Process = R6Class(
  "Process",
  inherit = ProcessManager,
  private = list(),
  
  public = list(
    
    GetScreensDefinition = function(){
      #browser()
      setNames(lapply(self$config$steps, function(x){
        eval(parse(text = paste0("self$", x, '()')))
      }),
      self$config$steps)
    }
  )
)