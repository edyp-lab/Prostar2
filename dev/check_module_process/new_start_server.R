
#' pipe_protein_Filtering Server Function
#'
#' @noRd 
#' 
#' @import DAPAR2
#' @import QFeatures
#' @importFrom shinyalert shinyalert
#' 
mod_pipe_protein_Filtering_server <- function(input, output, session, obj, indice){
  ns <- session$ns
  
  callModule(mod_navigation_server, 'nav_pipe_process', style=2, pages=r.nav)
