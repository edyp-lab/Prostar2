
Pipeline1 = R6Class(
  "Pipeline1",
  inherit = Pipeline,
  private = list(
    .config = list(name = 'Pipeline',
                   steps = c('ProcessDescription', 'ProcessA'),
                   mandatory = c(T, F)
    )
  ),
  
  public = list(
    ProcessDescription = function(){
      ns <- NS(self$id)
      p('toto')
    },
    
    ProcessA = function(){
      ns <- NS(self$id)
      p('toto')
    }
    
  )
)