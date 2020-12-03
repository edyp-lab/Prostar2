
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
      
    }
    
  )
)