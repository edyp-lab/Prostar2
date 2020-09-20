mod_process_B_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h4(paste0('Module ', id)),
    uiOutput(ns('show_n')),
    actionButton(ns('btn_valid'), 'Validate')
  )
}


mod_process_B_server <- function(id, dataIn=NULL, indice){
  moduleServer(
    id,
    function(input, output, session){
      
      rv <-reactiveValues(
        dataIn = NULL,
        indice = 0,
        dataOut = NULL
      )
      
      
      session$userData$mod_B_obs_1 <-  observeEvent(c(dataIn(), indice()), { 
        rv$dataIn <- dataIn()
        rv$indice <- indice()
        print(paste0('Module B, observer 1 - dataIn()= ', rv$dataIn, ', indice = ', rv$indice))
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