mod_C_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h4(paste0('Module ', id)),
    uiOutput(ns('show_n')),
    actionButton(ns('btn_valid'), 'Validate')
  )
}


mod_C_server <- function(id, dataIn=NULL){
  moduleServer(
    id,
    function(input, output, session){
      
      rv <-reactiveValues(
        dataIn = NULL,
        dataOut = NULL
      )
      
      
      session$userData$mod_C_observer_1 <-  observeEvent(dataIn(),{ 
        rv$dataIn <- dataIn()
        print(paste0('Module C, observer 1 -> ', rv$dataIn))
        })
      
      output$show_n <- renderUI({rv$dataIn})
      
      
      observeEvent(input$btn_valid,{
        rv$dataOut <-  rv$dataIn
      })
      
      return(reactive({rv$dataOut}))
    }
  )
}