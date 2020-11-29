# ------------ START OF COMMON FUNCTIONS --------------------

#' Declaration of global variables
VALIDATED <- 1
UNDONE <- 0
SKIPPED <- -1

InitActions <- function(n){
  setNames(lapply(1:n,
                  function(x){T}),
           paste0('screen', 1:n)
  )
}

CreateScreens <- function(names){
  setNames(
    lapply(1:length(names), 
           function(x){
             do.call(uiOutput, list(outputId=ns(names)[x]))}),
    paste0('screen_', names(config$steps)))
}

nbSteps <- reactive({
  req(config$steps)
  length(config$steps)
})



InsertDescriptionUI <- reactive({
  output[['Description']] <- renderUI({
    mod_insert_md_ui(ns(paste0(config$process.name, "_md")))
  })
  mod_insert_md_server(paste0(config$process.name, "_md"), 
                       paste0('./md/',config$process.name, '.md'))
})



#' Get the index of the last Validated step in 'tab' in an interval from 1 
#' to 'bound'
GetMaxValidated <- function(tab = NULL, bound = NULL){
  stopifnot(!is.null(tab))
  stopifnot(!is.null(bound))
  browser()
  ind <- max(which(unlist(tab)[1:bound]==VALIDATED))
  if (is.infinite(ind))
    # There is no Validated step in the tab
    ind <- NULL
  ind
}

#' isDone is a static list of n elements (the number of steps)
#' It indicates whether a step has been validated (TRUE) or not (FALSE)
#' It is updated by the return of module server
Init_isDone <- function(){
 setNames(lapply(1:nbSteps(), 
                  function(x){UNDONE}), 
           names(config$steps))
}

CommonInitializeFunctions <- function(){
  rv$screens <- InitActions(nbSteps())
  
  config$isDone <- Init_isDone()
  
  # Must be placed after the initialisation of the 'config$stepsNames' variable
  config$screens <- CreateScreens(names(config$steps))
}