#' @title Catch to a list
#'
#' @param expr xxx
#'
#' @return A `list` of three items: 'value', 'warnings' and 'error'
#'
#' @export
#' @examples
#' NULL
#'
catchToList <- function(expr) {
  val <- NULL
  myWarnings <- NULL
  myErrors <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, w$message)
    invokeRestart("muffleWarning")
  }
  myError <- NULL
  eHandler <- function(e) {
    myError <<- c(myErrors, e$message)
    NULL
  }
  val <- tryCatch(withCallingHandlers(expr, warning = wHandler),
    error = eHandler
  )
  list(value = val, warnings = myWarnings, error = myError)
}