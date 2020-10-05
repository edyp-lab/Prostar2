# ------------ START OF COMMON FUNCTIONS --------------------
InitActions <- function(n){
  setNames(lapply(1:n,
                  function(x){T}),
           paste0('screen', 1:n)
  )
}

CreateScreens <- function(n){
  setNames(
    lapply(1:n, 
           function(x){
             do.call(uiOutput, list(outputId=ns(paste0("screen", x))))}),
    paste0('screenStep', 1:n))
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
  config$screens <- CreateScreens(nbSteps())
  config$stepsNames <- setNames(config$stepsNames,config$stepsNames)
  config$stepsNames[1] <- 'Description'
  config$isDone <- Init_isDone()
}