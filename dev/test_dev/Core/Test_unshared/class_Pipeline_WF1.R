
Pipeline_WF1 = R6Class(
  "Pipeline_WF1",
  inherit = Pipeline,
  private = list(
    .config = list(name = 'Pipeline_WF1',
                   steps = c('WF1_Original', 'WF1_Filtering', 'WF1_Normalization', 'WF1_Imputation', 'WF1_HypothesisTest'),
                   mandatory = c(T, F, F, F, F)
    )
  ),
  
  public = list()
)