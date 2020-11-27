#Timeline_R6.R
TimelineForPipeline = R6Class(
  "TimelineForPipeline",
  inherit = TimelineManager,
  private = list(),
  
  public = list(
    initialize = function(id, mandatory, style=2 ) {
      cat(paste0(class(self)[1], '::initialize() from - ', self$id, '\n'))
      #browser()
      self$id <- id
      self$nbSteps <- length(mandatory)
      
      self$config <- reactiveValues()
      self$rv <- reactiveValues(
        current.pos = 1,
        reset_OK = NULL,
        isAllSkipped = FALSE
      )
      
      
      self$timelineDraw <- TimelineDraw$new(NS(id)('tl_draw'), 
                                            mandatory = mandatory,
                                            style = style)
    },
    
    modal_txt = "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed",
    
    ui = function() {
      ns <- NS(self$id)
      fluidPage(
        #self$Main_UI()
        uiOutput(ns('toto'))
      )
    },
    
    Force_ToggleState_Steps = function(){
      cat(paste0(class(self)[1], '::Force_ToggleState_Steps() from - ', self$id, '\n'))
       req(self$nbSteps)
      # if ((self$nbSteps==1) || (self$nbSteps>=2 && sum(self$config$status[2:self$nbSteps])== 0 )){
      #   # This is the case at the initialization of a process or after a reset
      #   # Enable all steps and buttons
      #   self$toggleState_Steps(cond = TRUE, i = self$nbSteps)
      # } else 
       # if (self$config$status[self$nbSteps] == self$global$SKIPPED){
       #   # Disable all steps
       #   self$ToggleState_Steps(cond = FALSE, i = self$nbSteps)
       # } 
       # else {
       #   # Disable all previous steps from each VALIDATED step
       #   ind.max <- max(grep(self$global$VALIDATED, self$config$status))
       #   browser()
       #   if (ind.max > 0)
       #   self$ToggleState_Steps(cond = FALSE, i = ind.max)
       # }
      
    }
  )
)