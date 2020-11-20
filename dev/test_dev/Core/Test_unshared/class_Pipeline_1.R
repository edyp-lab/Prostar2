#Timeline_R6.R
Pipeline1 = R6Class(
  "Pipeline1",
  inherit = Pipeline,
  private = list(
    .config = list(name = 'Pipeline',
                   type = 'pipeline',
                   steps = c('ProcessDescription', 'ProcessA', 'ProcessB'),
                   mandatory = c(T,F, F)
    )
  ),
  
  public = list()
)