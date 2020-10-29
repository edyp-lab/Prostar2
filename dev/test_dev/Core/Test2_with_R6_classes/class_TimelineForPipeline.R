#Timeline_Pipeline_R6.R
TimelinePipeline = R6Class(
  "TimelinePipeline",
  inherit = TimelineManager,
  private = list(
  modal_txt = "This action will reset this process, all further results will be deleted The new input dataset becomes the output of the last previous
                      validated process",
  
  Display_Current_Step = function(){
    # Display current page: One display all processes, even the validated ones
    lapply(1:private$length,
           function(x){
             shinyjs::toggle(paste0('div_screen', x),
                             condition = x==private$rv$current.pos)
             }
    )
    }
  ),
  public = list(
    initialize = function(id, steps, style=2 ) {
      self$id <- id
      private$nbSteps <- length(steps())
      private$timelineDraw <- TimelineDraw$new(NS(id)('tl_draw'), 
                                               steps = steps,
                                               style = style)
    }
  )
)