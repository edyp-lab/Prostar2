
PipelineSimple = R6Class(
  "PipelineSimple",
  inherit = Pipeline,
  private = list(
    .config = list(name = 'PipelineSimple',
                   steps = c('ProcessDescription', 'ProcessA', 'ProcessB', 'ProcessC'),
                   mandatory = c(T, F, T, F)
    )
  ),
  
  public = list()
)