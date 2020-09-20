


mod_source_1_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h4(paste0('Module ', id)),
    selectInput(ns('n'), 'N', choices=1:5, width='70px'),
    actionButton(ns('btn_valid'), 'Validate')
  )
}


mod_source_1_server <- function(id, params=NULL){
  moduleServer(
    id,
    function(input, output, session){
      
      rv <-reactiveValues(
        params = NULL,
        dataOut = NULL
      )
      
      session$userData$mod_source_1_obs_1 <-  observeEvent(params(),{ 
        rv$params <- params()
        print(paste0('Module source 1, observer 1 -> ', rv$params))
      })
      
#session$userData$mod_source_1_obs_2 <- 
      observeEvent(input$btn_valid,{
        print(paste0('Module source 1, observer 2 -> ', input$n))
        rv$dataOut <- input$n
      })
      
      return(reactive(rv$dataOut))
    }
  )
}

