#Timeline_R6.R
Process = R6Class(
  "Process",
  inherit = ProcessManager,
  private = list(),
  
  public = list(


    
    ActionsOnIsSkipped = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnIsSkipped() from - ', self$id, '\n'))
      if(verbose=='skip') browser()
      
      self$config$status <- setNames(rep(self$global$SKIPPED, self$length),
                                     self$config$steps)
      if(verbose=='skip') browser()
    },
    
    

  )
)