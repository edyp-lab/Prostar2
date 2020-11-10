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
    },
    
    InitializeModule = function(){
      
      private$config$screens <- private$CreateScreens()
      private$rv$current.pos <- 1
    }
    
    # Add_RenderUIs_Definitions = function( input, output){},
    
    
    
  ),
  
  public = list(
 
    GetConfig = function(){
      observe({print(paste0('-----GetConfig(', private$id, ') : ', paste0(private$config$steps, collapse=' ')))})
    }
    
  )
)