


mod_process_A_ui <- function(id){
  ns <- NS(id)
  tagList(
   tags$h4(paste0('Module ', id)),
   uiOutput(ns('show_n')),
   actionButton(ns('btn_valid'), 'Validate')
  )
}


mod_process_A_server <- function(id, dataIn=NULL){
  moduleServer(
    id,
    function(input, output, session){
      
        rv <-reactiveValues(
          dataIn = NULL,
          dataOut = NULL
           )
  
      session$userData$mod_A_obs_1 <-  observeEvent(dataIn(),{ 
          rv$dataIn <- dataIn()
          print(paste0('Module A, observer 1 - dataIn()  -> ', rv$dataIn))
        })
        
        
        output$show_n <- renderUI({
          rv$dataIn
          tags$p(paste0('rv$dataIn = ', rv$dataIn))
        })
        
        session$userData$mod_A_obs_2 <-  observeEvent(input$btn_valid,{
          rv$dataOut <- paste0('A_',rv$dataIn, ' ')
          print(paste0('Module A, observer btn_valid -> ', rv$dataOut))
        })
        
  return(reactive({rv$dataOut}))
    }
  )
}

