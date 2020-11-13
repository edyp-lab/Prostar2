#Timeline_R6.R
TimelineForProcess = R6Class(
  "TimelineForProcess",
  inherit = TimelineManager,
  private = list(),
  
  public = list(
    initialize = function(id, mandatory, style=2 ) {
      cat(paste0(class(self)[1], '::initialize()\n'))
      browser()
      self$id <- id
      self$nbSteps <- length(mandatory)
      
      self$config <- reactiveValues()
      self$rv <- reactiveValues(
        current.pos = 1,
        reset_OK = NULL
      )
      
      
      self$timelineDraw <- TimelineDraw$new(NS(id)('tl_draw'), 
                                               mandatory = mandatory,
                                               style = style)
    },
    
    modal_txt = "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed",
    
    
    Analyse_Status = function(){
      cat(paste0(class(self)[1], '::Analyse_Status()\n'))
      req(self$nbSteps)
      if ((self$nbSteps==1) || (self$nbSteps>=2 && sum(self$config$status[2:self$nbSteps])== 0 )){
        # This is the case at the initialization of a process or after a reset
        # Enable all steps and buttons
        self$toggleState_Steps(cond = TRUE, i = self$nbSteps)
      } else if (self$config$status[self$nbSteps] == self$global$SKIPPED){
        # Disable all steps
        self$toggleState_Steps(cond = FALSE, i = self$nbSteps)
      } else {
        # Disable all previous steps from each VALIDATED step
        ind.max <- max(grep(self$global$VALIDATED, self$config$status))
        self$toggleState_Steps(cond = FALSE, i = ind.max)
      }
      
    },
    
    Display_Current_Step = function(){
      cat(paste0(class(self)[1], '::Display_Current_Step()\n'))
      req(self$nbSteps)
      req(self$rv$current.pos)
      browser()
      # One only displays the steps that are not skipped
      lapply(1:self$nbSteps, function(x){
        shinyjs::toggle(paste0('div_screen', x), condition = x==self$rv$current.pos && self$config$status[self$rv$current.pos] != self$global$SKIPPED)})
    }
  )
)