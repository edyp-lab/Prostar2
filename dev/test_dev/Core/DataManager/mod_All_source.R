


mod_All_source_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h4(paste0('Module ', id)),
    uiOutput(ns('show_modules_ui'))
  )
}


mod_All_source_server <- function(id, navPage=NULL){
  moduleServer(
    id,
    function(input, output, session){
      
      rv <-reactiveValues(
        dataIn = NULL,
        dataOut = NULL
      )
      
      
      output$show_modules_ui <- renderUI({
        
      })
      
      
      
      
      
      
      
      if('mod_source_1' == input$navPage){
        rv.core$tmp <- mod_source_1_server('mod_source_1', dataIn = reactive('dataIn_1'))
        observeEvent(rv.core$tmp, {rv.core$current.obj <- rv.core$tmp()})
      }
      
      if('mod_source_2' == input$navPage){
        rv.core$tmp <- mod_source_2_server('mod_source_2', dataIn = reactive('dataIn_2'))
        observeEvent(rv.core$tmp, {rv.core$current.obj <- rv.core$tmp()})
      }
      
      if('mod_source_3' == input$navPage){
        rv.core$tmp <- mod_source_3_server('mod_source_3', dataIn = reactive('dataIn_3'))
        observeEvent(rv.core$tmp, {rv.core$current.obj <- rv.core$tmp()})
      }
      
      
      
      
      
      session$userData$mod_source_1_obs_1 <-  observeEvent(dataIn(),{ 
        rv$dataIn <- dataIn()
        print(paste0('Module source 1, observer 1 -> ', rv$dataIn))
      })
      
      #session$userData$mod_source_1_obs_2 <- 
      observeEvent(input$btn_valid,{
        print(paste0('Module source 2, observer 2 -> ', input$n))
        rv$dataOut <- paste0('mod_source_1_',input$n)
      })
      
      return(reactive({rv$dataOut}))
    }
  )
}

