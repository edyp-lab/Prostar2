#' @title xxx
#' @description xxxx
#' @import DaparToolshed
#' @import MagellanNTK
#' 
#' 
#' @examples
#' \dontrun{
#' library(Prostar2)
#' library(DaparToolshed)
#' Prostar2('PipelineProtein')
#' Prostar2('PipelineProtein', user = 'dev')
#' Prostar2('PipelinePeptide')
#' 
#' Prostar2('PipelineConvert')
#' }
#' 
#' @export
#' 
Prostar2 <- function(
    wf.name = NULL, 
  usermod = 'user',
  verbose = FALSE){
  
  
  options(
    shiny.maxRequestSize = 1024^3,
    shiny.fullstacktrace = TRUE,
    port = 3838,
    host = "127.0.0.1",
    launch.browser = TRUE
  )

  wf.path <- system.file(paste0('workflow/', wf.name), package = 'Prostar2')
  
  MagellanNTK::MagellanNTK(
    obj = NULL,
    workflow.path = wf.path,
    workflow.name = wf.name,
    usermod = usermod,
    verbose = verbose
  )
  
}