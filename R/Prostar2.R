#' @title xxx
#' @description xxxx
#' @import DaparToolshed
#' @import MagellanNTK
#' 
#' 
#' @examples
#' \dontrun{
#' library(Prostar2)
#' Prostar2('PipelineProtein')
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
    port = 3838,
    host = "127.0.0.1",
    launch.browser = FALSE
  )
  
  library(MagellanNTK)
  library(highcharter)
  library(DaparToolshed)
  library(shinyBS)
  library(omXplore)
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(shinydashboardPlus)
  
  
  wf.path <- system.file(paste0('workflow/', wf.name), package = 'Prostar2')
  
  MagellanNTK::MagellanNTK(
    workflow.path = wf.path,
    workflow.name = wf.name,
    usermod = usermod,
    verbose = verbose
  )
  
}