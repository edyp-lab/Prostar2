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
    },
    
    ActionOn_isSkipped = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnIsSkipped() from - ', self$id, '\n'))
      #if(verbose=='skip') 
      value <- if (self$rv$isSkipped) global$SKIPPED else global$UNDONE
      self$rv$status <- setNames(rep(value, self$length), self$config$steps)
    }
  )
)