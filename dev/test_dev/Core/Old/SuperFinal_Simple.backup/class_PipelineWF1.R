
PipelineWF1 = R6Class(
  "PipelineWF1",
  inherit = Pipeline,
  private = list(
    .config = list(name = 'PipelineWF1',
                   steps = c('WF1_Original',
                             'WF1_Filtering',
                             'WF1_Normalization',
                             'WF1_Imputation',
                             'WF1_HypothesisTest'),
                   mandatory = c(T, F, T, F, T)
                   )
    ),
  
  public = list(
    WF1_Original = function(){
      ns <- self$ns(self$id)
      p('toto')
    },
    
    WF1_Filtering = function(){
      ns <- self$ns(self$id)
      p('toto')
    },
    
    WF1_Normalization = function(){
      ns <- self$ns(self$id)
      p('toto')
    },
    
    WF1_Imputation = function(){
      ns <- self$ns(self$id)
      p('toto')
    },
    
    WF1_HypothesisTest = function(){
      ns <- self$ns(self$id)
      p('toto')
    }
  )
)