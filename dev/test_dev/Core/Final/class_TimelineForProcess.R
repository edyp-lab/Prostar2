
TimelineForProcess = R6Class(
  "TimelineForProcess",
  inherit = TimelineManager,
  private = list(),
  
  public = list(
    modal_txt = "This action will reset this process. The input dataset will be the output of the last previous
                      validated process and all further datasets will be removed",
    
    ui = function(){
      fluidPage(
      shinyjs::useShinyjs(),
      wellPanel(
        style="background: white; border-width: 2px; border-color: blue;",
        self$Main_UI()
      )
    )
    },
    
    Force_ToggleState_Screens = function(){
      cat(paste0(class(self)[1], '::Force_ToggleState_Steps() from - ', self$id, '\n'))
      #if (verbose==T) 
       browser()
      
      #if (!self$rv$dataLoaded){
        self$ToggleState_Screens(cond = self$rv$dataLoaded, range = 1:self$length)
     #   }

      # if (self$rv$isAllUndone){
      #     # Enable all steps and buttons at the initialization of a process or after a reset
      #     self$ToggleState_Screens(cond = TRUE, range = 1:self$length)
      #     }
      #  else 
         if (self$rv$isAllSkipped){
            # Disable all steps if all steps are skipped
            self$ToggleState_Screens(cond = FALSE, range = 1:self$length)
       }
       
         firstM <- self$GetFirstMandatoryNotValidated()
         if (!is.null(firstM)) {
          offset <- as.numeric(firstM != self$length)
          # Disable all further screens
          self$ToggleState_Screens(cond = FALSE, range = (firstM + offset):self$length)
          }
        
        if (self$rv$status[self$rv$current.pos] == global$VALIDATED) {
          # Disable all previous steps from each VALIDATED step
          # and enable all further steps (in case of current.pos is mandatory)
          ind.max <- self$GetMaxValidated_AllSteps()
          if (!is.null(ind.max)){
            self$ToggleState_Screens(cond = FALSE, range = 1:ind.max)
            if (ind.max < self$length){
              offset <- 1
              self$ToggleState_Screens(cond = TRUE, range = (offset + ind.max):self$length)
            }
          }
        }
      

    }
  )
)