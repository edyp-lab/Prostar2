
TimelineForPipeline = R6Class(
  "TimelineForPipeline",
  inherit = TimelineManager,
  private = list(),
  
  public = list(
    modal_txt = "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed.",
    
    ui = function(){
      fluidPage(
        shinyjs::useShinyjs(),
        self$Main_UI()
        )
    },
    
    Force_ToggleState_Screens = function(){
      cat(paste0(class(self)[1], '::Force_ToggleState_Steps() from - ', self$id, '\n'))
      #if (verbose==T) 
      # browser()
      req(self$length)
      
      if (!self$rv$dataLoaded){
        self$ToggleState_Screens(cond = FALSE, range = 1:self$length)
      } else {
        if (self$rv$isAllUndone){
          # Enable all steps and buttons at the initialization of a process or after a reset
          self$ToggleState_Screens(cond = TRUE, range = 1:self$length)
        } else if (self$rv$isAllSkipped){
          # Disable all steps if all steps are skipped
          self$ToggleState_Screens(cond = FALSE, range = 1:self$length)
        }
        
        firstM <- self$GetFirstMandatoryNotValidated()
        if (!is.null(firstM)) {
          offset <- if (firstM==self$length) 0 else 1
          # Disable all further screens
          self$ToggleState_Screens(cond = FALSE, range = (firstM + offset):self$length)
        }
      }
    }
    
  )
)