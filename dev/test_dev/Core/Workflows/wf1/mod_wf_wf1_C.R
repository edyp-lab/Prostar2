mod_wf_wf1_C_ui <- function(id){
  ns <- NS(id)
  tagList(shinyjs::useShinyjs(),
          shinyalert::useShinyalert(),
          div(id=ns('process_ui'), 
              tags$h4(paste0('Module ', id)),
              tags$p('y = x - 10'),
              selectInput(ns('si'), '', choices=LETTERS[1:4], selected='B', width='100px'),
              numericInput(ns('num'), '', value=10, width='100px'),
              actionButton(ns('reset'), 'Reset'),
              actionButton(ns('btn_valid'), 'Validate'),
              uiOutput(ns('currentObj'))
          )
  )
}


mod_wf_wf1_C_server <- function(id, dataIn=NULL, indice){
  moduleServer(
    id,
    function(input, output, session){
      
      rv <-reactiveValues(
        dataIn = NULL,
        indice = if (is.null(indice())) 1 else  indice(),
        dataOut = NULL
      )
      
      session$userData$mod_A_obs_reset <- observeEvent(input$reset, {
        shinyjs::reset('process_ui')
        
        rv$dataIn <- dataIn()
        rv$indice <- if (is.null(indice())) 1 else  indice()
        rv$dataOut <- NULL
      })
      
      
      
      
      # Just for the show absolutePanel
      output$currentObj <- renderUI({
        tagList(
          tags$p(tags$strong(paste0('rv$indice = ',rv$indice))),
          fluidRow(
            column(3,
                   tags$p(tags$strong('rv$dataIn : ')),
                   tags$ul(
                     lapply(paste0(names(rv$dataIn ), "=", unlist(rv$dataIn )), 
                            function(x) tags$li(x))
                   )
            ),
            column(3,
                   tags$p(tags$strong('rv$dataOut : ')),
                   tags$ul(
                     lapply(paste0(names(rv$dataOut ), "=", unlist(rv$dataOut )), 
                            function(x) tags$li(x))
                   )
            )
          )
        )
      })
      
      
      session$userData$mod_C_obs_1 <-  observeEvent(c(dataIn(), indice()), { 
        rv$dataIn <- dataIn()
        rv$indice <- if (is.null(indice())) 1 else  indice()
        print('Module C, observer 1)')
        
        
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

      
      # observeEvent(input$shinyalert, ignoreNULL=T, {
      #   shinyjs::toggleState('div_nav_pipe_process', condition = input$shinyalert)
      # })
      
      session$userData$mod_C_obs_2 <-  observeEvent(input$btn_valid, ignoreInit=T, ignoreNULL = T,{

        # We delete all items that are further the given indice
        # to ensure that the new item is always the last one
        print('btn_valid')
        if (rv$indice < length(rv$dataIn)){
          rv$dataIn <- rv$dataIn[-c((rv$indice+1):length(rv$dataIn))]
        }
        name <- paste0('C-processed', rv$indice)
        # This ensures that each new item has d different name
        if (length(grep(name, names(rv$dataIn))) > 0)
          paste0(name, '_', (1+length(grep(name, names(rv$dataIn)))))
        
        rv$dataIn <- append(rv$dataIn, setNames(rv$dataIn[[rv$indice]] - 10, name))
        rv$indice <- rv$indice+1
        rv$dataOut <- rv$dataIn
        glue::glue('Module C, observer btn_valid')
      })

      
      return(reactive({rv$dataOut}))
    }
  )
}