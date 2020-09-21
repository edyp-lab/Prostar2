


mod_wf_wf1_A_ui <- function(id){
  ns <- NS(id)
  tagList(shinyjs::useShinyjs(),
          shinyalert::useShinyalert(),
          div(id=ns('div_nav_pipe_process'), 
              tags$h4(paste0('Module ', id)),
              tags$p('y = 2 * x'),
              actionButton(ns('btn_valid'), 'Validate')
          )
  )
}


mod_wf_wf1_A_server <- function(id, dataIn=NULL, indice){
  moduleServer(
    id,
    function(input, output, session){
      
        rv <-reactiveValues(
          dataIn = NULL,
          indice = NULL,
          dataOut = NULL
           )
  
      session$userData$mod_A_obs_1 <-  observeEvent(c(dataIn(),indice()), { 
          rv$dataIn <- dataIn()
          rv$indice <- if (is.null(indice())) 1 else  indice()
          glue::glue('Module A, observer 1')
          
          # if (rv$indice == length(rv$dataIn))
          #   return(NULL)
          # 
          # shinyalert::shinyalert(
          #   title = 'title',
          #   text = "This is a modal",
          #   size = "xs", 
          #   closeOnEsc = TRUE,
          #   closeOnClickOutside = FALSE,
          #   html = FALSE,
          #   type = "info",
          #   showConfirmButton = TRUE,
          #   showCancelButton = TRUE,
          #   confirmButtonText = "OK",
          #   confirmButtonCol = "#15A4E6",
          #   cancelButtonText = "Cancel",
          #   timer = 0,
          #   imageUrl = "",
          #   animation = FALSE
          # )
      })

        
        
      # 
      # observeEvent(input$shinyalert, ignoreNULL=T, {
      #   shinyjs::toggleState('div_nav_pipe_process', condition = input$shinyalert)
      # })
      
      
        session$userData$mod_A_obs_2 <-  observeEvent(input$btn_valid,{
isolate({
          # We delete all items that are further the given indice
          # to ensure that the new item is always the last one
          print('btn_valid')
          if (rv$indice < length(rv$dataIn)){
            rv$dataIn <- rv$dataIn[-c((rv$indice+1):length(rv$dataIn))]
          }
          name <- paste0('proc A from i =', rv$indice)
          # This ensures that each new item has d different name
          if (length(grep(name, names(rv$dataIn))) > 0)
            paste0(name, '_', (1+length(grep(name, names(rv$dataIn)))))
          
          rv$dataIn <- append(rv$dataIn, setNames(2 * rv$dataIn[[rv$indice]], name))
          rv$indice <- rv$indice+1
          rv$dataOut <- rv$dataIn
          glue::glue('Module A, observer btn_valid')
        })
          
        })
        
  return(reactive({rv$dataOut}))
    }
  )
}

