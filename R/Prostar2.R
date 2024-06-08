#' @title xxx
#' 
#' @import DaparToolshed
#' @import MagellanNTK
#' 
#' 
#' @examplesIf interactive()
#' Prostar2()
#' 
#' 
#' @export
#' 
Prostar2 <- function(){
  wf.name <- 'PipelineProtein'
  wf.path <- system.file('workflow/PipelineProtein', package = 'Prostar2')
  
  MagellanNTK(
    workflow.path = wf.path,
    workflow.name = wf.name
  )
  
}