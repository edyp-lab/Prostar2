#' @title Evaluate an Expression While Capturing Warnings and Errors
#'
#' @description
#' Evaluates an R expression and captures its return value, warnings, and errors
#' in a structured list. Useful for logging, testing, or interactive environments
#' where you need to inspect the output and messages without stopping execution.
#'
#' @param expr An expression to evaluate.
#'
#' @return A list with three components:
#' \item{value}{The result of the expression. If an error occurs, this will be `NULL`.}
#' \item{warnings}{A character vector of all warning messages generated during evaluation. `NULL` if no warnings issued.}
#' \item{error}{A character vector of all error messages generated during evaluation. `NULL` if no error occurred.}
#'
#' @examples
#' #Capture a successful evaluation
#' catchToList(mean(1:10))
#' 
#' # Capture warnings
#' catchToList({
#'   warning("First warning")
#'   warning("Second warning")
#'   42
#' })
#' catchToList(log(c(1, 0, -1)))
#'
#' # Capture errors
#' catchToList(stop("Something went wrong"))
#'
#' @export
#' 
catchToList <- function(expr) {
  state <- new.env(parent = emptyenv())
  state$warnings <- NULL
  state$error <- NULL

  wHandler <- function(w) {
    state$warnings <- c(state$warnings, w$message)
    invokeRestart("muffleWarning")
  }

  eHandler <- function(e) {
    state$error <- e$message
    NULL
  }

  value <- tryCatch(
    withCallingHandlers(expr, warning = wHandler),
    error = eHandler
  )

  list(
    value = value,
    warnings = state$warnings,
    error = state$error
  )
}
