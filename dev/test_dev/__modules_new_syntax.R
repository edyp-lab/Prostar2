# https://shiny.rstudio.com/articles/modules.html


mod_B_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h4(paste0('Module ', id)),
    uiOutput(ns('show_n')),
    actionButton(ns('btn_valid'), 'Validate')
  )
}


mod_B_server <- function(id, dataIn=NULL){
  moduleServer(
    id,
    function(input, output, session){
     # xxxxxxxxxxx
  )
}