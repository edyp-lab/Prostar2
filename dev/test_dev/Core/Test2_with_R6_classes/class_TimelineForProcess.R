#Timeline_R6.R
TimelineForProcess = R6Class(
  "TimelineForProcess",
  inherit = TimelineManager,
  private = list(
    modal_txt = "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed",


    Analyse_Status = function(){
      req(private$Length())
      if ((private$Length()==1) || (private$Length()>=2 && sum(private$GetStatus()[2:private$Length()])== 0 )){
        # This is the case at the initialization of a process or after a reset
        # Enable all steps and buttons
        self$toggleState_Steps(cond = TRUE, i = private$Length())
      } else if (private$GetStatus()[private$Length()] == private$global$SKIPPED){
        # Disable all steps
        self$toggleState_Steps(cond = FALSE, i = private$Length())
      } else {
        # Disable all previous steps from each VALIDATED step
        ind.max <- max(grep(private$global$VALIDATED, private$GetStatus()))
        self$toggleState_Steps(cond = FALSE, i = ind.max)
      }

    },
    
    Display_Current_Step = function(){
      req(private$Length())
      # One only displays the steps that are not skipped
         lapply(1:private$Length(), function(x){
           shinyjs::toggle(paste0('div_screen', x), condition = x==private$GetCurrentPos() && private$GetStatus()[private$GetCurrentPos()] != private$global$SKIPPED)})
    }
  ),
  
  public = list(
    initialize = function(id, mandatory, style=2 ) {
      private$id <- id
      private$timelineDraw <- TimelineDraw$new(NS(id)('tl_draw'), 
                                               mandatory = mandatory,
                                               style = style)
    }
  )
)