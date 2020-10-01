#' pipe_prot_filter UI Function
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


#' pipe_prot_filter Server Function
#'
#' @noRd 
#' 
#' @import DAPAR2
#' @import QFeatures
#' @importFrom shinyalert shinyalert
#' 
mod_pipe_protein_Filtering_server <- function(input, output, session, obj, indice){
  ns <- session$ns
  
  ## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = "Filtering",
    stepsNames = c("MV filtering", "Field filtering", "Validate"),
    ll.UI = list( screenStep1 = uiOutput(ns("Screen_Filtering_1")),
                  screenStep2 = uiOutput(ns("Screen_Filtering_2")),
                  screenStep3 = uiOutput(ns("Screen_Filtering_3"))
    ),
    isDone =  rep(FALSE,3),
    mandatory =  rep(FALSE,3),
    reset = FALSE
  )
  
  ## reactive values for variables in the module
  rv <- reactiveValues(
    name = "processProtFilter",
    dataIn = NULL,
    dataOut = NULL,
    i = NULL,
    settings = NULL,
    
    widgets = list(
      ## need to put each items to its default value
    )
  )
  
  
  
  #global variables for the module
  
  
  #shinyalert modal asking if user wants to process a dataset with an index <i
  observeEvent(req(rv$dataIn, rv$i ), {
    
    a <- (length(rv$dataIn) != rv$i) && !r.nav$isDone[length(r.nav$isDone)]
    if (!a) return(NULL)
    
    shinyalert::shinyalert(
      title = 'title',
      text = "This is a modal",
      size = "xs", 
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#15A4E6",
      cancelButtonText = "Cancel",
      timer = 0,
      imageUrl = "",
      animation = FALSE
    )
  })
  
  
  
  observeEvent(req(r.nav$reset),{
    
    rv$widgets <- list(
      ## need to put each items to its default value
    )
    
    ## do not modify this part
    rv$dataIn <- obj()
    rv$i <- indice()
    
    r.nav$isDone <- rep(FALSE, 3)
    r.nav$reset <- FALSE
    ## end of no modifiable part
    
    
  })
  
  mod_navigation_server('nav_pipe_process', style=2, pages=r.nav)
  
  #### END of template part of the module
  
  
  ##
  ##  
  ## Calls to other modules
  ##
  ##
  
  
  rv$settings <- mod_settings_server("settings", obj=reactive({rv$dataIn}))
  
  
  # Initialisation of the module
  observe({
    req(obj(), indice())
    rv$dataIn <- obj()
    rv$i <- indice()
  })
  
  
  # If the user accepts the conditions on the shinyalert, then the process module is activated
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
  
  
  
  disableActionButton <- function(id,session) {
    session$sendCustomMessage(type="jsCode",
                              list(code= paste("$('#",id,"').prop('disabled',true)"
                                               ,sep="")))
  }
  
  ##
  ## Definitions of the screens
  ##
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 1                                        ###
  ###---------------------------------------------------------------------------------###
  output$Screen_Filtering_1 <- renderUI({
    
    
  })
  
  
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 2                                        ###
  ###---------------------------------------------------------------------------------###
  output$Screen_Filtering_2 <- renderUI({
    
    
    
  })
  
  
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 3                                        ###
  ###---------------------------------------------------------------------------------###
  output$Screen_Filtering_3 <- renderUI({     
    
    
  })
  
  
  
  
  return({reactive(rv$dataOut)})
  
}

## To be copied in the UI
# mod_pipe_protein_Filtering_ui("pipe_prot_filter_ui_1")

## To be copied in the server
# callModule(mod_pipe_protein_Filtering_server, "pipe_prot_filter_ui_1")
