#' @title xxx
#' @description xxxx
#' @importFrom  MagellanNTK MagellanNTK
#'
#' @param wf.name xxx
#' @param usermod xxx
#' @param verbose xxx
#'
#' @examples
#' if (interactive()){
#' library(Prostar2)
#' Prostar2("PipelineProtein")
#' Prostar2("PipelineProtein", user = "dev")
#' Prostar2("PipelinePeptide")
#' }
#'
#' @export
#'
Prostar2 <- function(
    wf.name = NULL,
    usermod = "user",
    verbose = FALSE) {
  options(
    shiny.maxRequestSize = 1024^3,
    shiny.fullstacktrace = TRUE,
    port = 3838,
    host = "127.0.0.1",
    launch.browser = FALSE
  )

  
  require(QFeatures)
  require(MagellanNTK)
  
  
  data(Exp1_R25_prot, package = 'DaparToolshedData')
  obj <- Exp1_R25_prot
  
  # Launch in the Magellan workspace
  wf.name <- 'PipelineProtein'
  #wf.path <- system.file('workflow/PipelineProtein', package = 'Prostar2')
  wf.path <- system.file(paste0("workflow/", wf.name), package = "Prostar2")

  MagellanNTK::MagellanNTK(
    obj = obj,
    workflow.path = wf.path,
    workflow.name = wf.name
   # usermod = usermod,
   # verbose = verbose
  )
  
  
  
  
  
  
  
}
