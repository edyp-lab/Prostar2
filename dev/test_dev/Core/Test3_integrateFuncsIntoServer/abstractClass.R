ChildClass <- R6Class(
  "AbstractClass",
  inherit=AbstractClass,
  private=list(),
  
  public = list(
    initialize = function(id){
                  self$id <- id
                }
  )
)


AbstractClass <- R6Class(
  "AbstractClass",
  private=list(),
  
  public = list(id = NULL,
                initialize = function(){
                   stop("AbstractClass is an abstract class that can't be initialized.")
                 },
                print = function(){
                  cat(self$id)
                }
  )
)


test <- AbstractClass$new()
test$print()

child <- ChildClass$new('toto')
child$print()