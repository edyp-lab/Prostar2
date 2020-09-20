


mod_source_3_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h4(paste0('Module ', id)),
    selectInput(ns('n'), 'N', choices=1:5, width='70px'),
    actionButton(ns('btn_valid'), 'Validate')
  )
}


mod_source_3_server <- function(id, params=NULL){
  moduleServer(
    id,
    function(input, output, session){
      
      rv <-reactiveValues(
        params = NULL,
        dataOut = NULL
      )
      
      session$userData$mod_source_3_obs_1 <-  observeEvent(params(),{ 
        rv$params <- params()
        print(paste0('Module source 3, observer 1 -> ', rv$params))
      })

      session$userData$mod_source_3_obs_2 <- observeEvent(input$btn_valid,{
        print(paste0('Module source 3, observer 2 -> ', input$n))
        rv$dataOut <- paste0('mod_source_3_',input$n)
      })
      
      return(reactive({rv$dataOut}))
    }
  )
}

