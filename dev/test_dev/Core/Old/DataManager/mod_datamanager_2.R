


mod_datamanager_2_ui <- function(id, ll.wf=""){
  ns <- NS(id)
  tagList(
    tags$h4(paste0('Module ', id)),
    selectInput(ns('workflow'), 'Choose a workflow', choices=ll.wf, width='150px'),
    actionButton(ns('btn_valid'), 'Validate')
  )
}


mod_datamanager_2_server <- function(id, params=NULL){
  moduleServer(
    id,
    function(input, output, session){
      
      rv <-reactiveValues(
        dataIn = NULL,
        dataOut = NULL
      )
      
      session$userData$mod_source_2_obs_1 <-  observeEvent(params(),{ 
        rv$params <- params()
        print(paste0('Module source 2, observer 1 -> ', rv$params))
      })

      session$userData$mod_source_2_obs_2 <- observeEvent(input$btn_valid, ignoreInit = TRUE,{
        print(paste0('Module source 2, observer 2 -> ', 1))
        rv$dataOut <- list(workflow = input$workflow,
                           obj = setNames(lapply(1, function(x) {x}), paste0('original',2))
        )
      })
      
      return(reactive({rv$dataOut}))
    }
  )
}

