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
  rv.filter <- reactiveValues(
    name = "processProtFilter",
    dataIn = NULL,
    dataOut = NULL,
    i = NULL,
    settings = NULL,
    
    widgets = list(
      
    )
  )
  
  
  
  #global variables for the module
  
  
  #shinyalert modal asking if user wants to process a dataset with an index <i
  observeEvent(req(rv.filter$dataIn, rv.filter$i ), {
    
    a <- (length(rv.filter$dataIn) != rv.filter$i) && !r.nav$isDone[length(r.nav$isDone)]
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
    
    rv.filter$widgets <- list()
    
    ## do not modify this part
    rv.filter$dataIn <- obj()
    rv.filter$i <- indice()
    
    r.nav$isDone <- rep(FALSE, 3)
    r.nav$reset <- FALSE
    ## end of no modifiable part
    
    
  })
  
  callModule(mod_navigation_server, 'nav_pipe_process', style=2, pages=r.nav)
  
  #### END of template part of the module
  
  
  ##
  ##  
  ## Calls to other modules
  ##
  ##
  
  
  rv.filter$settings <- callModule(mod_settings_server, "settings", obj=reactive({rv.filter$dataIn}))
  
  
  
  observe({
    req(obj(), indice())
    
    rv.filter$dataIn <- obj()
    rv.filter$i <- indice()
    
    
    if (MultiAssayExperiment::metadata(obj()[[indice()]])$typeOfData != 'protein'){
      stop("The type of data contained in the dataset is not 'protein'")
      return(NULL)
    }
    
  })
  
  
  # If the user accepts the conditions on the shinyalert, then the process module is activated
  observe({
    input$shinyalert
    rv.filter$i
    if(is.null(input$shinyalert))return(NULL)
    
    c1 <- input$shinyalert
    c2 <- rv.filter$i == length(rv.filter$dataIn)
    c3 <- r.nav$isDone[length(r.nav$isDone)]
    if (c1 && !c2 && !c3){
      #Delete all assays after that one indicated by the indice given in parameter
      rv.filter$dataIn <- rv.filter$dataIn[ , , -((rv.filter$i+1):length(rv.filter$dataIn))]
      c1 <- input$shinyalert
      c2 <- rv.filter$i == length(rv.filter$dataIn)
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
  
  
  
  ##
  ## Perform process
  observeEvent(input$perform.filtering.MV,ignoreInit=TRUE,{

    
    isolate({
      
        r.nav$isDone[1] <- TRUE
        
    })
  })
  
  #for each widget
  observeEvent(input$seuilNA, ignoreInit=TRUE,{
    rv.filter$widgets$seuilNA <- input$seuilNA
  })
  
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 2                                        ###
  ###---------------------------------------------------------------------------------###
  output$Screen_Filtering_2 <- renderUI({

    
    
  })
  
  
  
  
  observeEvent(input$btn_perform_fieldFilter,ignoreInit=TRUE,{
 
      r.nav$isDone[2] <- TRUE
 
  })
  
  
  
  
  
  ###---------------------------------------------------------------------------------###
  ###                                 Screen 3                                        ###
  ###---------------------------------------------------------------------------------###
  output$Screen_Filtering_3 <- renderUI({     
    
    
  })
  
  
  observeEvent(input$ValidateFilters,ignoreInit = TRUE,{

    r.nav$isDone[3] <- TRUE
  })
  
  
  return({reactive(rv.filter$dataOut)})
  
}

## To be copied in the UI
# mod_pipe_protein_Filtering_ui("pipe_prot_filter_ui_1")

## To be copied in the server
# callModule(mod_pipe_protein_Filtering_server, "pipe_prot_filter_ui_1")

