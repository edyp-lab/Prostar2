
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
    
    EncapsulateScreens = function(){
      req(self$screens)
      cat(paste0(class(self)[1], '::GetScreens() from - ', self$id, '\n'))
      lapply(1:self$length, function(i) {
        if (i==1)
          div(
            class = paste0("page_", self$id),
            id = self$ns(self$config$steps[i]),
            self$screens[[i]]
          )
        else
          shinyjs::hidden(
            div(
              class = paste0("page_", self$id),
              id = self$ns(self$config$steps[i]),
              self$screens[[i]])
          )
      }
      )
    },
    
    Force_ToggleState_Screens = function(){
      cat(paste0(class(self)[1], '::Force_ToggleState_Steps() from - ', self$id, '\n'))
      #if (verbose==T) 
       #browser()
      
      if (self$rv$isAllSkipped){
        # Disable all steps if all steps are skipped
        self$ToggleState_Screens(cond = FALSE, range = 1:self$length)
      }
      
      firstM <- self$GetFirstMandatoryNotValidated()
      if (!is.null(firstM) && self$length > 1) {
        offset <- as.numeric(firstM != self$length)
        # Disable all further screens
        self$ToggleState_Screens(cond = FALSE, range = (firstM + offset):self$length)
      }
      
      # Disable all previous steps from each VALIDATED step
      # and enable all further steps (in case of current.pos is mandatory)
      ind.max <- self$GetMaxValidated_AllSteps()
      if (!is.null(ind.max)){
        #self$ToggleState_Screens(cond = FALSE, range = 1:ind.max)
        if (ind.max < self$length){
          offset <- 1
          self$ToggleState_Screens(cond = TRUE, range = (offset + ind.max):self$length)
        }
      }
    }
    
  )
)