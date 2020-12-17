
PipelineSimple = R6Class(
  "PipelineSimple",
  inherit = Pipeline,
  private = list(
    .config = list(name = 'PipelineSimple',
                   steps = c('ProcessOriginal', 'ProcessA'),
                   mandatory = c(T, F)
    )
  ),
  
  public = list(
    ProcessOriginal = function(){
      ns <- self$ns(self$id)
      p('toto')
    },
    
    ProcessA = function(){
      ns <- self$ns(self$id)
      p('toto')
    }
    
  )
)