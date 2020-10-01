
#' pipe_protein_newModule_shinyTest UI Function
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
mod_pipe_protein_newModule_shinyTest_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    mod_navigation_ui(ns('nav_pipe_process'))
  )
}

#' pipe_protein_newModule_shinyTest Server Function
#'
#' @noRd 
#' 
#' @import DAPAR2
#' @import QFeatures
#' @importFrom shinyalert shinyalert
#' 
mod_pipe_protein_newModule_shinyTest_server <- function(input, output, session, obj, indice){
  ns <- session$ns
  
  
  
  ## Section navigation module
  # Variable to manage the different screens of the module
  r.nav <- reactiveValues(
    name = 'newModule',
    stepsNames = c('s1','s2'),
    ll.UI = list(screenStep1 = uiOutput(ns('Screen_newModule_1')),screenStep2 = uiOutput(ns('Screen_newModule_2'))),
    isDone =  rep(FALSE,2),
    mandatory =  rep(FALSE,2),
    reset = FALSE
  )
  
  
  
  ## reactive values for variables in the module
  rv <- reactiveValues(
    name = 'process_newModule',
    dataIn = NULL,
    dataOut = NULL,
    i = NULL,
    settings = NULL,
    
    widgets = list(w1=0,w2=0.05,w3="None")
  )
  
  observeEvent(req(r.nav$reset),{
    
    rv$widgets <- list(w1=0,w2=0.05,w3="None")
    
    ## do not modify this part
    rv$dataIn <- obj()
    rv$i <- indice()
    
    r.nav$isDone <- rep(FALSE, 2)
    r.nav$reset <- FALSE
    ## end of no modifiable part
    
    
  })
  
  mod_navigation_server('nav_pipe_process', style=2, pages=r.nav)
  
  ## Definitions of the screens
  
  
  #Screen1
  output$Screen_newModule_1 <- renderUI({
    
    # mod_infos_dataset_ui('infos')
    
  })
  
  # callModule(mod_infos_dataset_server,'infos',
  #            obj = reactive({rv$dataIn}))
  
  #Screen2
  output$Screen_newModule_2 <- renderUI({
    
    # mod_plots_corr_matrix_ui('plots_corr_matrix')
    
  })
  
  # rv$settings <- callModule(mod_settings_server, 'settings', obj=reactive({rv$dataIn}))
  # 
  # callModule(mod_plots_corr_matrix_server,'plots_corr_matrix', 
  #            obj = reactive({rv$dataIn}),
  #            names = reactive({NULL}),
  #            gradientRate = reactive({r$settings()$defaultGradientRate})
  # )
  
  
  observeEvent(input$w1, ignoreInit=TRUE,{
    rv.filter$widgets$w1 <- input$w1
  })
  
  
  observeEvent(input$w2, ignoreInit=TRUE,{
    rv.filter$widgets$w2 <- input$w2
  })
  
  
  observeEvent(input$w3, ignoreInit=TRUE,{
    rv.filter$widgets$w3 <- input$w3
  })
  
  
  return({reactive(rv$dataOut)})
  
}

## To be copied in the UI
# mod_pipe_protein_newModule_shinyTest_ui('pipe_protein_newModule_shinyTest_ui_1')

## To be copied in the server
# callModule(mod_pipe_protein_newModule_shinyTest_server, 'pipe_protein_newModule_shinyTest_ui_1')
