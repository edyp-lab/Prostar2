Test <- R6Class(
  classname = "Test",
  public = list(
    test = NULL,
    launch = NULL,
    initialize = function() {
      self$test <- reactive({
        2
      })
      self$launch <- reactive({
        self$test
      })
    },
    
    launch2 = function(){
      cat(self$launch())
    }
  )
)
