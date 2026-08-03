#' @title xxx
#' 
#' @description xxxx
#' 
#' @param wf.name xxx
#' @param convert.name xxx
#' @param usermod xxx
#' @param verbose xxx
#'
#' @return NA
#'
#' @examples
#' if (interactive()){
#' library(Prostar2)
#' library(MagellanNTK)
#' Prostar2("PipelineProtein")
#' Prostar2("PipelineProtein_Filtering")
#' Prostar2("PipelineProtein_Normalization")
#' Prostar2("PipelineProtein_Imputation")
#' Prostar2("PipelineProtein_HypothesisTest")
#' }
#'
#' @importFrom MagellanNTK MagellanNTK
#'
#' @export
#'
Prostar2 <- function(
    wf.name = NULL,
    convert.name = 'PipelineProtein_Convert',
    usermod = "user",
    verbose = FALSE) {

  pkgs_require(c('MagellanNTK', 'omXplore'))
  
  # Launch in the Magellan workspace
  wf.path <- unlist(strsplit(wf.name, '_'))[1]
  wf.path <- system.file(paste0("workflow/", wf.path), package = "Prostar2")

  convert.path <- unlist(strsplit(convert.name, '_'))[1]
  convert.path <- system.file(paste0("workflow/", convert.path), package = "Prostar2")
  
  
  MagellanNTK::MagellanNTK(
    obj = NULL,
    workflow.path = wf.path,
    workflow.name = wf.name,
    convert.path = convert.path
  )
}
