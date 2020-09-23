
#' pipe_protein_Filtering_shinyTest UI Function
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
mod_pipe_protein_Filtering_shinyTest_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    div(id=ns('div_nav_pipe_process'), mod_navigation_ui(ns('nav_pipe_process')))
  )
}

#' pipe_protein_Filtering_shinyTest Server Function
#'
#' @noRd 
#' 
#' @import DAPAR2
#' @import QFeatures
#' @importFrom shinyalert shinyalert
#' 
mod_pipe_protein_Filtering_shinyTest_server <- function(input, output, session, obj, indice){
  ns <- session$ns
  
  callModule(mod_navigation_server, 'nav_pipe_process', style=2, pages=r.nav)

## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = 'Filtering',
    stepsNames = screen1,table2,plop,
    ll.UI = list(screenStep1 = uiOutput(ns('Screen_Filtering_1')),screenStep2 = uiOutput(ns('Screen_Filtering_2')),screenStep3 = uiOutput(ns('Screen_Filtering_3'))),
    isDone =  rep(FALSE,3),
    mandatory =  rep(FALSE,3),
    reset = FALSE
  )

## reactive values for variables in the module
  rv <- reactiveValues(
    name = 'process_Filtering',
    dataIn = NULL,
    dataOut = NULL,
    i = NULL,
    settings = NULL,
    
    widgets = list(ChooseFilters=None)
  )

observeEvent(req(r.nav$reset),{
    
    rv$widgets <- list(ChooseFilters=None)
    
    ## do not modify this part
    rv$dataIn <- obj()
    rv$i <- indice()
    
    r.nav$isDone <- rep(FALSE, 3)
    r.nav$reset <- FALSE
    ## end of no modifiable part
    
    
  })


## Definitions of the screens

#Screen1
output$Screen_Filtering_1 <- renderUI({


})
#Screen2
output$Screen_Filtering_2 <- renderUI({


})
#Screen3
output$Screen_Filtering_3 <- renderUI({


})observeEvent(input$ChooseFilters, ignoreInit=TRUE,{
rv.filter$widgets$ChooseFilters <- input$ChooseFilters
})


 return({reactive(rv$dataOut)})

}

## To be copied in the UI
# mod_pipe_protein_Filtering_shinyTest_ui('pipe_protein_Filtering_shinyTest_ui_1')

## To be copied in the server
# callModule(mod_pipe_protein_Filtering_shinyTest_server, 'pipe_protein_Filtering_shinyTest_ui_1')
