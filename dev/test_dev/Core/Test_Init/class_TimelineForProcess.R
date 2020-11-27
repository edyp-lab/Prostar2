#Timeline_R6.R
TimelineForProcess = R6Class(
  "TimelineForProcess",
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
    
    ui = function() {
      ns <- NS(self$id)
      fluidPage(
        shinyjs::useShinyjs(),
        wellPanel(
          style="background: white; border-width: 2px; border-color: blue;",
          uiOutput(ns('toto'))
        )
      )
    },
    
    
    modal_txt = "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed",
    
    
    Force_ToggleState_Steps = function(){
      cat(paste0(class(self)[1], '::Force_ToggleState_Steps() from - ', self$id, '\n'))
      #if (verbose==T) 
       # browser()
      req(self$nbSteps)
      if (self$rv$isAllUndone){
        # Enable all steps and buttons at the initialization of a process or after a reset
        self$ToggleState_Steps(cond = TRUE, i = self$nbSteps)
      } else if (self$rv$isAllSkipped){
        # Disable all steps if all steps are skipped
        self$ToggleState_Steps(cond = FALSE, i = self$nbSteps)
      } else {
        # Disable all previous steps from each VALIDATED step
        ind.max <- self$GetMaxValidated_AllSteps()
        if (ind.max > 0)
          self$ToggleState_Steps(cond = FALSE, i = ind.max)
      }
      
    }
  )
)