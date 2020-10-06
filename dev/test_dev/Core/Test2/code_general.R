# ------------ START OF COMMON FUNCTIONS --------------------
InitActions <- function(n){
  setNames(lapply(1:n,
                  function(x){T}),
           paste0('screen', 1:n)
  )
}

CreateScreens <- function(stepsNames){
  setNames(
    lapply(1:length(stepsNames), 
           function(x){
             do.call(uiOutput, list(outputId=ns(stepsNames)[x]))}),
    paste0('screen_', config$stepsNames))
}

nbSteps <- reactive({
  req(config$stepsNames)
  length(config$stepsNames)
})

Init_isDone <- function(){
  setNames(lapply(1:nbSteps(), 
                  function(x){ x == 1}), 
           config$stepsNames)
}




Reset_Pipeline_Data_logics <- function(){
  rv$dataOut <- dataIn()
}

GetMaxTrue <- function(bound = nbSteps()){
  max(which(unlist(config$isDone)[1:bound]==T))
}


CommonInitializeFunctions <- function(){
  rv$event_counter <- 0
  rv$screens <- InitActions(nbSteps())
  config$stepsNames <- setNames(config$stepsNames, config$stepsNames)
  config$stepsNames[1] <- 'Description'
  config$isDone <- Init_isDone()
  
  # Must be placed after the initialisation of the 'config$stepsNames' variable
  config$screens <- CreateScreens(names(config$stepsNames))
  
}