#' pipe_pep_filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pipe_pep_filter_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' pipe_pep_filter Server Function
#'
#' @noRd 
mod_pipe_pep_filter_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_pipe_pep_filter_ui("pipe_pep_filter_ui_1")
    
## To be copied in the server
# callModule(mod_pipe_pep_filter_server, "pipe_pep_filter_ui_1")
 
