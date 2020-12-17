
Pipeline_WF1 = R6Class(
  "Pipeline_WF1",
  inherit = Pipeline,
  private = list(
    .config = list(name = 'Pipeline_WF1',
                   steps = c('WF1_Original', 'WF1_Filtering', 'WF1_Normalization', 'WF1_Imputation', 'WF1_HypothesisTest'),
                   mandatory = c(T, F, F, F, F)
    )
  ),
  
  public = list(
    
    WF1_Original = function(){
      ns <- NS(self$id)
      p('toto')
    },
    
    WF1_Filtering = function(){
      ns <- NS(self$id)
      p('toto')
    },
    WF1_Normalization = function(){
      ns <- NS(self$id)
      p('toto')
    },
    WF1_Imputation = function(){
      ns <- NS(self$id)
      p('toto')
    },
    WF1_HypothesisTest = function(){
      ns <- NS(self$id)
      p('toto')
    }
  )
)