
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
  
  ## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = 'Filtering',
    stepsNames = c('MV filtering', 'Field filtering', 'Validate'),
    ll.UI = list(screenStep1 = uiOutput(ns('Screen_Filtering_1')),
                 screenStep2 = uiOutput(ns('Screen_Filtering_2')),
                 screenStep3 = uiOutput(ns('Screen_Filtering_3'))),
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
    
    widgets = list(ChooseFilters = "None",seuilNA = 0)
  )
  
  observeEvent(req(r.nav$reset),{
    
    rv$widgets <- list(ChooseFilters = "None",seuilNA = 0)
    
    ## do not modify this part
    rv$dataIn <- obj()
    rv$i <- indice()
    
    r.nav$isDone <- rep(FALSE, 3)
    r.nav$reset <- FALSE
    ## end of no modifiable part
    
    
  })
  
  #shinyalert modal asking if user wants to process a dataset with an index <i
  observeEvent(req(rv$dataIn, rv$i ), {
    
    a <- (length(rv$dataIn) != rv$i) && !r.nav$isDone[length(r.nav$isDone)]
    if (!a) return(NULL)
    
    shinyalert::shinyalert(
      title = 'title',
      text = 'This is a modal',
      size = 'xs', 
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = 'info',
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = 'OK',
      confirmButtonCol = '#15A4E6',
      cancelButtonText = 'Cancel',
      timer = 0,
      imageUrl = '',
      animation = FALSE
    )
  })
  
  observe({
    req(input$shinyalert)
    rv$i
    
    c1 <- input$shinyalert
    c2 <- rv$i == length(rv$dataIn)
    c3 <- r.nav$isDone[length(r.nav$isDone)]
    if (c1 && !c2 && !c3){
      #Delete all assays after that one indicated by the indice given in parameter
      rv$dataIn <- rv$dataIn[ , , -((rv$i+1):length(rv$dataIn))]
      c1 <- input$shinyalert
      c2 <- rv$i == length(rv$dataIn)
      c3 <- r.nav$isDone[length(r.nav$isDone)]
    } else {
      # Do nothing, the module interface is still disabled
    }
    shinyjs::toggleState('div_nav_pipe_process', condition = !c3 && (c1||c2))
  })
  
  return({reactive(rv$dataOut)})
  
}

## To be copied in the UI
# mod_pipe_protein_Filtering_ui('pipe_protein_Filtering_ui_1')

## To be copied in the server
# callModule(mod_pipe_protein_Filtering_server, 'pipe_protein_Filtering_ui_1')
