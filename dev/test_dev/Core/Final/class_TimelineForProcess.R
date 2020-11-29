
TimelineForProcess = R6Class(
  "TimelineForProcess",
  inherit = TimelineManager,
  private = list(),
  
  public = list(
    
    ui = function(){
      fluidPage(
      shinyjs::useShinyjs(),
      wellPanel(
        style="background: white; border-width: 2px; border-color: blue;",
        uiOutput(self$ns('showUI'))
      )
    )
    }
    
  )
)