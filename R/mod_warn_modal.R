#' warn_update_previous_SE UI Function
#'
#' @description This module shows a modal windows if condition is true. Then, 
#' it shows the text given in parameter.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @param condition xxx
#' 
#' @param msg xxx
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_warn_modal_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('warn'))
  )
}
    
#' warn_modal Server Function
#'
#' @noRd 
#' 
mod_warn_modal_server <- function(input, output, session, condition, msg){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_warn_update_previous_SE_ui("warn_update_previous_SE_ui_1")
    
## To be copied in the server
# callModule(mod_warn_update_previous_SE_server, "warn_update_previous_SE_ui_1")
 
