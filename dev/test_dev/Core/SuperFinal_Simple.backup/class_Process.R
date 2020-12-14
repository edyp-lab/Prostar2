Process = R6Class(
  "Process",
  inherit = ProcessManager,
  private = list(),
  
  public = list(
    GetScreens_ui = function(){
      cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n'))
      setNames(lapply(self$config$steps, function(x){
        eval(parse(text = paste0("self$", x, '_ui()')))
      }),
      self$config$steps)
    },
    
    GetScreens_listeners = function(){
      cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n'))
      setNames(lapply(self$config$steps, function(x){
        eval(parse(text = paste0("self$", x, '_listeners()')))
      }),
      self$config$steps)
    }
  )
)
