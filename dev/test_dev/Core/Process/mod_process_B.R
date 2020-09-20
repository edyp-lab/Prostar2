mod_process_B_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h4(paste0('Module ', id)),
    uiOutput(ns('show_n')),
    actionButton(ns('btn_valid'), 'Validate')
  )
}


mod_process_B_server <- function(id, dataIn=NULL){
  moduleServer(
    id,
    function(input, output, session){
      
      rv <-reactiveValues(
        dataIn = NULL,
        dataOut = NULL
      )
      
      
      session$userData$mod_B_obs_1 <-  observeEvent(dataIn(),{ 
        rv$dataIn <- dataIn()
        print(paste0('Module B, observer 1 - dataIn() -> ', rv$dataIn))
      })
      
      
      output$show_n <- renderUI({
        dataIn()
      })
      
      session$userData$mod_B_obs_2 <-  observeEvent(input$btn_valid,{
        rv$dataOut <-  paste0('B_',rv$dataIn, ' ')
        print(paste0('Module B, observer btn_valid -> ', rv$dataOut))
      })
      
      return(reactive({rv$dataOut}))
    }
  )
}