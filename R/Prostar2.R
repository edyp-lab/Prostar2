#' @title xxx
#' @description xxxx
#' @import DaparToolshed
#' @import MagellanNTK
#' 
#' 
#' @examplesIf interactive()
#' library(Prostar2)
#' Prostar2('PipelineProtein')
#' 
#' Prostar2('PipelineConvert')
#' 
#' 
#' @export
#' 
Prostar2 <- function(
    wf.name = NULL, 
  usermod = 'user',
  verbose = FALSE){
  wf.path <- system.file(paste0('workflow/', wf.name), package = 'Prostar2')
  
  MagellanNTK::MagellanNTK(
    workflow.path = wf.path,
    workflow.name = wf.name,
    usermod = usermod,
    verbose = verbose
  )
  
}