#Timeline_R6.R
TimelineForProcess = R6Class(
  "TimelineForProcess",
  inherit = TimelineManager,
  private = list(
    modal_txt = "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed",
    
    
    Analyse_Status = function(){
      req(private$nbSteps)
      if ((private$nbSteps==1) || (private$nbSteps>=2 && sum(private$config[[private$id]]$status[2:private$nbSteps])== 0 )){
        # This is the case at the initialization of a process or after a reset
        # Enable all steps and buttons
        self$toggleState_Steps(cond = TRUE, i = private$nbSteps)
      } else if (private$config[[private$id]]$status[private$nbSteps] == private$global$SKIPPED){
        # Disable all steps
        self$toggleState_Steps(cond = FALSE, i = private$nbSteps)
      } else {
        # Disable all previous steps from each VALIDATED step
        ind.max <- max(grep(private$global$VALIDATED, private$config[[private$id]]$status))
        self$toggleState_Steps(cond = FALSE, i = ind.max)
      }
      
    },
    
    Display_Current_Step = function(){
      req(private$nbSteps)
      # One only displays the steps that are not skipped
      lapply(1:private$nbSteps, function(x){
        shinyjs::toggle(paste0('div_screen', x), condition = x==private$rv[[private$id]]$current.pos && private$config[[private$id]]$status[private$rv[[private$id]]$current.pos] != private$global$SKIPPED)})
    }
  ),
  
  public = list(
    initialize = function(id,  style=2 ) {
      print(paste0('in class_TimelineForProcess::initialize() with id = ', id))
      private$id <- id

    }
  )
)