mod_process_C_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h4(paste0('Module ', id)),
    uiOutput(ns('show_n')),
    actionButton(ns('btn_valid'), 'Validate')
  )
}


mod_process_C_server <- function(id, dataIn=NULL, indice){
  moduleServer(
    id,
    function(input, output, session){
      
      rv <-reactiveValues(
        dataIn = NULL,
        indice = 0,
        dataOut = NULL
      )
      
      
      session$userData$mod_C_obs_1 <-  observeEvent(c(dataIn(), indice()), { 
        rv$dataIn <- dataIn()
        rv$indice <- indice()
        print(paste0('Module C, observer 1 - dataIn()= ', rv$dataIn, ', indice = ', rv$indice))
      })
      
      output$show_n <- renderUI({rv$dataIn})
      
      
      session$userData$mod_C_obs_2 <-  observeEvent(input$btn_valid,{
        rv$dataOut <-  paste0('C_',rv$dataIn, ' ')
        print(paste0('Module C, observer btn_valid -> ', rv$dataOut))
      })
      
      return(reactive({rv$dataOut}))
    }
  )
}