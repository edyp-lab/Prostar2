#Timeline_R6.R
TimelineForProcess = R6Class(
  "TimelineForProcess",
  inherit = TimelineManager,
  private = list(
    modal_txt = "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed",


    Analyse_Status = function(){
      req(private$nbSteps)
      if ((private$nbSteps==1) || (private$nbSteps>=2 && sum(private$config$status[2:private$nbSteps])== 0 )){
        # This is the case at the initialization of a process or after a reset
        # Enable all steps and buttons
        self$toggleState_Steps(cond = TRUE, i = private$nbSteps)
      } else if (private$config$status[private$nbSteps] == private$global$SKIPPED){
        # Disable all steps
        self$toggleState_Steps(cond = FALSE, i = private$nbSteps)
      } else {
        # Disable all previous steps from each VALIDATED step
        ind.max <- max(grep(private$global$VALIDATED, private$config$status))
        self$toggleState_Steps(cond = FALSE, i = ind.max)
      }

    },
    
    Display_Current_Step = function(){
      req(private$nbSteps)
      # One only displays the steps that are not skipped
         lapply(1:private$nbSteps, function(x){
           shinyjs::toggle(paste0('div_screen', x), condition = x==private$rv$current.pos && private$config$status[private$rv$current.pos] != private$global$SKIPPED)})
    }
  ),
  
  public = list(
    initialize = function(id, mandatory, style=2 ) {
      self$id <- id
      private$nbSteps <- length(mandatory)
      private$timelineDraw <- TimelineDraw$new(NS(id)('tl_draw'), 
                                               mandatory = mandatory,
                                               style = style)
    }
  )
)