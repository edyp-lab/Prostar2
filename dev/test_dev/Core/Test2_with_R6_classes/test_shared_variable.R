library(R6)


AbstractPM <- R6Class(
  "AbstractPM",
  private = list(
    id = NULL,
    config = list()
  ),
  public = list(
    initialize = function(id){},
    GetId = function(){cat(private$id)},
    SetConfig = function(conf){private$config <- conf},
    GetConfig = function(){paste0(private$config, collapse=' ')}
  )
)



##################################################################################################
Process <- R6Class(
  "Process",
  inherit = AbstractPM,
  public = list(
    initialize = function(id){
      private$id <- id
      private$config <- list(A=1, B=2, C=3)
    }
    
  )
)

##################################################################################################

Pipeline <- R6Class(
  "Pipeline",
  inherit = AbstractPM,
  private=(
    listUI = NULL
    ),
  public = list(
    initialize = function(id){
      private$id <- id
      listUI <- Process$new('test')
      private$config <- list(toto=1, tutu=23)

    }

  )
)



pipeline <- Pipeline$new('test-pipeline')