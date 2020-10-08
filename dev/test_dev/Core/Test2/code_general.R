# ------------ START OF COMMON FUNCTIONS --------------------
# SendCmdToTimeline <- function(names){
#   append(as.list(names), list(runif(1, min=0, max=1e5)))
# }

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




GetMaxTrue <- function(tab = NULL, bound = NULL){
  stopifnot(!is.null(tab))
  stopifnot(!is.null(bound))
  ind <- max(which(unlist(tab)[1:bound]==VALIDATED))
  if (is.infinite(ind))
    ind <- NULL
  ind
}

# isDone is a static list of n elements (the number of steps)
# It indicates whether a step has been validated (TRUE) or not (FALSE)
# It is updated by the return of module server
Init_isDone <- function(){
  if(isSkipped())
    setNames(lapply(1:nbSteps(), 
                    function(x){SKIPPED}), 
             names(config$steps))
  else
    setNames(lapply(1:nbSteps(), 
                  function(x){ if (x == 1) VALIDATED else UNDONE}), 
           names(config$steps))
}

CommonInitializeFunctions <- function(){
  rv$screens <- InitActions(nbSteps())
  
  #config$stepsNames <- setNames(config$stepsNames, config$stepsNames)
  #config$stepsNames[1] <- 'Description'
  
  config$isDone <- Init_isDone()
  
  # Must be placed after the initialisation of the 'config$stepsNames' variable
  config$screens <- CreateScreens(names(config$steps))
  
}