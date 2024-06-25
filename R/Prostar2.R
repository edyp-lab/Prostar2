#' @title xxx
#' @description xxxx
#' @import DaparToolshed
#' @import MagellanNTK
#' 
#' 
#' @examplesIf interactive()
#' Prostar2('PipelineProtein')
#' 
#' 
#' @export
#' 
Prostar2 <- function(wf.name){
  wf.path <- system.file(paste0('workflow/', wf.name), package = 'Prostar2')
  
  MagellanNTK::MagellanNTK(
    workflow.path = wf.path,
    workflow.name = wf.name,
    usermod = 'user'
  )
  
}