#Timeline_R6.R
Process = R6Class(
  "Process",
  inherit = ProcessManager,
  private = list(
    ActionsOnNewPosition = function(){},
    
    ActionsOnIsSkipped = function(){
      cat(paste0(class(self)[1], '::', 'ActionsOnIsSkipped()\n'))
      if (private$rv[[private$id]]$isSkipped)
        tag <- private$global$SKIPPED
      else
        tag <- private$global$UNDONE
      
      private$config$status <- setNames(rep(tag, private$length),
                                        private$config$steps)
    },
    
    InitializeModule = function(){
      cat(paste0(class(self)[1], '::', 'InitializeModule()\n'))
      private$config$screens <- private$CreateScreens()
      private$rv[[private$id]]$current.pos <- 1
    }
    
    # Add_RenderUIs_Definitions = function( input, output){},
    
    
    
  ),
  
  public = list(

  )
)