Queue <- R6Class("Queue",
                 public = list(
                   initialize = function(...) {
                     for (item in list(...)) {
                       self$add(item)
                     }
                   },
                   add = function(x) {
                     private$queue <- c(private$queue, list(x))
                     invisible(self)
                   },
                   remove = function() {
                     if (private$length() == 0) return(NULL)
                     # Can use private$queue for explicit access
                     head <- private$queue[[1]]
                     private$queue <- private$queue[-1]
                     head
                   },
                   cat = function(){
                     cat(paste0(private$queue, collapse = ' '))
                   }
                 ),
                 private = list(
                   queue = list(),
                   length = function() base::length(private$queue)
                 )
)





HistoryQueue <- R6Class("HistoryQueue",
                        inherit = Queue,
                        public = list(
                          show = function() {
                            cat("Next item is at index", private$head_idx + 1, "\n")
                            for (i in seq_along(private$queue)) {
                              cat(i, ": ", private$queue[[i]], "\n", sep = "")
                            }
                          },
                          remove = function() {
                            if (private$length() - private$head_idx == 0) return(NULL)
                            private$head_idx <- private$head_idx + 1
                            private$queue[[private$head_idx]]
                          }
                        ),
                        private = list(
                          head_idx = 0
                        )
)




q <- Queue$new(5, 6, "foo")