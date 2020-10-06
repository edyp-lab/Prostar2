# ------------ START OF COMMON FUNCTIONS --------------------
SendCmdToTimeline <- function(names){
  append(as.list(names), list(runif(1, min=0, max=1e5)))
}


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



InsertDescriptionUI <- function(){
  output[['Description']] <- renderUI({
    mod_insert_md_ui(ns(paste0(config$process.name, "_md")))
  })
  mod_insert_md_server(paste0(config$process.name, "_md"), 
                       paste0('./md/',config$process.name, '.md'))
}


Reset_Pipeline_Data_logics <- function(){
  rv$dataOut <- dataIn()
}

GetMaxTrue <- function(bound = nbSteps()){
  max(which(unlist(config$isDone)[1:bound]==T))
}

# isDone is a static list of n elements (the number of steps)
# It indicates whether a step has been validated (TRUE) or not (FALSE)
# It is updated by the return of module server
Init_isDone <- function(){
  setNames(lapply(1:nbSteps(), 
                  function(x){ x == 1}), 
           names(config$steps))
}

CommonInitializeFunctions <- function(){
  rv$event_counter <- 0
  rv$screens <- InitActions(nbSteps())
  
  #config$stepsNames <- setNames(config$stepsNames, config$stepsNames)
  #config$stepsNames[1] <- 'Description'
  
  config$isDone <- Init_isDone()
  
  # Must be placed after the initialisation of the 'config$stepsNames' variable
  config$screens <- CreateScreens(names(config$steps))
  
}