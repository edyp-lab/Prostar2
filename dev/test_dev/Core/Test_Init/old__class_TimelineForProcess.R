#Timeline_R6.R
TimelineForProcess = R6Class(
  "TimelineForProcess",
  inherit = TimelineManager,
  private = list(),
  
  public = list(
    
 
    modal_txt = "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed",
    
    
    Force_ToggleState_Steps = function(){
      cat(paste0(class(self)[1], '::Force_ToggleState_Steps() from - ', self$id, '\n'))
      #if (verbose==T) 
       # browser()
      req(self$nbSteps)
      if (self$rv$isAllUndone){
        # Enable all steps and buttons at the initialization of a process or after a reset
        self$ToggleState_Steps(cond = TRUE, range = 1:self$nbSteps)
      } else if (self$rv$isAllSkipped){
        # Disable all steps if all steps are skipped
        self$ToggleState_Steps(cond = FALSE, range = 1:self$nbSteps)
      } else if (self$config$status[self$rv$current.pos]==self$global$UNDONE &&
                 self$config$mandatory[self$rv$current.pos]==TRUE) {
        # Disable all further screens
        self$ToggleState_Steps(cond = FALSE, range = (1+self$rv$current.pos):self$nbSteps)
      } else {
        # Disable all previous steps from each VALIDATED step
        ind.max <- self$GetMaxValidated_AllSteps()
        if (ind.max > 0)
          self$ToggleState_Steps(cond = FALSE, range = 1:ind.max)
      }
      
    }
  )
)