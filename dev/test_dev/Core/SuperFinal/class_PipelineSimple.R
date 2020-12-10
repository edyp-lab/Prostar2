
PipelineSimple = R6Class(
  "PipelineSimple",
  inherit = Pipeline,
  private = list(
    .config = list(name = 'PipelineSimple',
                   steps = c('ProcessDescription', 'ProcessA', 'ProcessB'),
                   mandatory = c(T, F, F)
    )
  ),
  
  public = list(
    # ProcessDescription = function(){
    #   ns <- self$ns(self$id)
    #   p('toto')
    # },
    # 
    # ProcessA = function(){
    #   ns <- self$ns(self$id)
    #   p('toto')
    # },
    # 
    # ProcessB = function(){
    #   ns <- self$ns(self$id)
    #   p('toto')
    # }
    
  )
)