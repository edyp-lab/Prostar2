Wake <- function(){ runif(1,0,1)}


Initialize_Status <- reactive({
  config$status <- setNames(rep(UNDONE,length(config$steps)),names(config$steps))
})

GetMaxValidated_AllSteps <- reactive({
  browser()
  val <- 0
  ind <- which(config$status == VALIDATED)
  if (length(ind) > 0)
    val <- max(ind)
  val
})

GetMaxValidated_BeforeCurrentPos <- reactive({
  ind.max <- NULL
  indices.validated <- which(config$status == VALIDATED)
  if (length(indices.validated) > 0){
    ind <- which(indices.validated < rv$current.pos)
    if(length(ind) > 0)
      ind.max <- max(ind)
  }
  ind.max
})

Set_Skipped_Status <- function(){
  for (i in nbSteps())
    if (config$status[i] != 1 && GetMaxValidated_AllSteps() > i)
      config$status <- SKIPPED
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
  config$status <- setNames(rep(UNDONE,length(config$steps)),names(config$steps))
}

GetMaxValidated_AllSteps <- reactive({
  browser()
  val <- 0
  ind <- which(config$status == VALIDATED)
  if (length(ind) > 0)
    val <- max(ind)
  val
})



GetCurrentStepName <- reactive({ names(config$steps)[rv$current.pos] })

Unskip <- function(pos){config$status[pos] <- UNDONE}

GetStatusPosition <- function(pos){config$status[pos]}

