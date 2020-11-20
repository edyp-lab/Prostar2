
Pipeline1 = R6Class(
  "Pipeline1",
  inherit = Pipeline,
  private = list(
    .config = list(name = 'Pipeline',
                   steps = c('ProcessDescription', 'ProcessA', 'ProcessB'),
                   mandatory = c(T, F, F)
    )
  ),
  
  public = list()
)