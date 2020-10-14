#' Declaration of global variables
VALIDATED <- 1
UNDONE <- 0
SKIPPED <- -1
RESETED <- 2




InitScreens <- function(n){
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





Wake <- function(){ runif(1,0,1)}


Initialize_Status <- reactive({
  config$status <- setNames(rep(UNDONE,length(config$steps)),names(config$steps))
})

GetMaxValidated_AllSteps <- reactive({
  #browser()
  val <- 0
  ind <- which(config$status == VALIDATED)
  if (length(ind) > 0)
    val <- max(ind)
  val
})

GetMaxValidated_BeforeCurrentPos <- reactive({
  ind.max <- 0
  indices.validated <- which(config$status == VALIDATED)
  if (length(indices.validated) > 0){
    ind <- which(indices.validated < rv$current.pos)
    if(length(ind) > 0)
      ind.max <- max(ind)
  }
  
  if (ind.max == 0)
    ind.max <- 1
  
  ind.max
})

Set_Skipped_Status <- function(){
  if(verbose)
    print(paste0(config$process.name, ' : Set_Skipped_Status() = '))
  #browser()
  for (i in 1:nbSteps())
    if (config$status[i] != VALIDATED && GetMaxValidated_AllSteps() > i)
      config$status[i] <- SKIPPED
}

# Test if a process module (identified by its name) has been skipped.
# This function is called each time the list config$isDone is updated
# because one can know the status 'Skipped' only when a further module
# has been validated
is.skipped <- function(name){
  pos <- which(name == names(config$steps))
  return(GetStatusPosition(pos) == SKIPPED)
}



Initialize_Status_Process <- function(){
  if(verbose)
    print(paste0(config$process.name, ' : Initialize_Status_Process() : '))
  
  config$status <- setNames(rep(UNDONE,length(config$steps)),names(config$steps))
}

GetMaxValidated_AllSteps <- reactive({
  #browser()
  val <- 0
  ind <- which(config$status == VALIDATED)
  if (length(ind) > 0)
    val <- max(ind)
  val
})



GetCurrentStepName <- reactive({ names(config$steps)[rv$current.pos] })

Unskip <- function(pos){config$status[pos] <- UNDONE}

GetStatusPosition <- function(pos){config$status[pos]}

