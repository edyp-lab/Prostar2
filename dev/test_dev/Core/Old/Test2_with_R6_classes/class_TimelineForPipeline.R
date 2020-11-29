#Timeline_Pipeline_R6.R
TimelineForPipeline = R6Class(
  "TimelineForPipeline",
  inherit = TimelineManager,
  private = list(
  modal_txt = "This action will reset this process, all further results will be deleted The new input dataset becomes the output of the last previous
                      validated process",
  
  Display_Current_Step = function(){
    req(private$nbSteps)
    # Display current page: One display all processes, even the validated ones
    lapply(1:private$nbSteps,
           function(x){
             shinyjs::toggle(paste0('div_screen', x),
                             condition = x==private$rv$current.pos)
             }
    )
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