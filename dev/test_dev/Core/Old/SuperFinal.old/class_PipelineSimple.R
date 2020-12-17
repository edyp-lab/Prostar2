
PipelineSimple = R6Class(
  "PipelineSimple",
  inherit = Pipeline,
  private = list(
    .config = list(name = 'PipelineSimple',
                   steps = c('ProcessA', 'ProcessB'),
                   mandatory = c( F, F)
    )
  ),
  
  public = list()
)