
#' pipe_protein_Filtering UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import shinyjs
#' @importFrom shinyalert useShinyalert
#' 
mod_pipe_protein_Filtering_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    div(id=ns('div_nav_pipe_process'), mod_navigation_ui(ns('nav_pipe_process')))
  )
}
