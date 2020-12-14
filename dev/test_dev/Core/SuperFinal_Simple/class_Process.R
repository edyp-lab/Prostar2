Process = R6Class(
  "Process",
  inherit = ProcessManager,
  private = list(),
  
  public = list(
    GetScreens = function(){
      cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n'))
      #browser()
      setNames(lapply(self$config$steps, function(x){
        eval(parse(text = paste0("self$", x, '()')))
      }),
      self$config$steps)
    }
  )
)