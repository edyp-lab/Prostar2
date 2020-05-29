#' mod_select_keyID UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mod_select_keyID_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' mod_select_keyID Server Function
#'
#' @noRd 
mod_mod_select_keyID_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_mod_select_keyID_ui("mod_select_keyID_ui_1")
    
## To be copied in the server
# callModule(mod_mod_select_keyID_server, "mod_select_keyID_ui_1")
 
