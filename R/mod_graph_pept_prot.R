#' graph_pept_prot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_graph_pept_prot_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' graph_pept_prot Server Function
#'
#' @noRd 
mod_graph_pept_prot_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_graph_pept_prot_ui("graph_pept_prot_ui_1")
    
## To be copied in the server
# callModule(mod_graph_pept_prot_server, "graph_pept_prot_ui_1")
 
