
# source(file.path('../../../../R', 'mod_navigation.R'), local=TRUE)$value
# source(file.path('../../../../R', 'global.R'), local=TRUE)$value


mod_wf_wf1_A_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('show')),
    uiOutput(ns('currentObj'))
  )
}

#' @param dataIn xxx
#'
#' 
#' 
mod_wf_wf1_A_server <- function(id, dataIn=NULL){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      rv <- reactiveValues()
      
      #rv <- reactiveValues(
      # dataIn = NULL,
      #  dataOut = NULL,
      #  currentData = 0
      #)
      
      
      
      # variables to communicate with the navigation module
      r.nav <- reactiveValues(
        name = "Process A",
        stepsNames = c("A - Description", "A - Step 1", "A - Step 2", "A - Step 3"),
        ll.UI = list( screenStep1 = uiOutput(ns("screen1")),
                      screenStep2 = uiOutput(ns("screen2")),
                      screenStep3 = uiOutput(ns("screen3")),
                      screenStep4 = uiOutput(ns("screen4"))
                      ),
        isDone =  c(TRUE, FALSE, FALSE, FALSE),
        mandatory =  c(FALSE, FALSE, TRUE, TRUE),
        reset = FALSE,
        rerun = FALSE
      )
      
      screens <- mod_navigation_server("test_nav", 
                                      style = 2, 
                                      pages = r.nav)
      
      
      output$show <- renderUI({
        tagList(
          mod_navigation_ui(ns("test_nav")),
          screens()
        )
      })
      
        
        ## The goal is t restart the timeline as if it is the first time
      # The main action is to reload the dataset
      # if the final validation button has not be clicked, then restore the last not null dataset
      # among the set of datasets before current position i
      # else reload the dataset among the set o 1 : (i-1)
      observeEvent(req(r.nav$reset),{
          print('reset action')
        for (i in 1:length(r.nav$stepsNames))
            shinyjs::reset(paste0('screen', i))


        if (r.nav$isDone[length(r.nav$stepsNames)])
            rv$dataOut <- dataIn()[1:(length(dataIn()) - 1)]
          else
            rv$dataOut <- dataIn()[1:(length(rv$dataIn) - length(dataIn()))]
          
          #rv$dataOut <- NULL
          r.nav$current.indice <- 1
          r.nav$reset  <- FALSE
          
          # Set all steps to undone except the first one which is the description screen
          r.nav$isDone <- c(TRUE, rep(FALSE, length(r.nav$stepsNames)-1))
          
        })

      # observeEvent(req(r.nav$act.next),{
      #   r.nav$act.next <- FALSE
      #   print('btn next was acticated')
      # 
      #   if (!(r.nav$isDone[r.nav$current.indice - 1]) || r.nav$current.indice == 2)
      #   rv$dataIn <- append(rv$dataIn, setNames(0, paste0('screen', (r.nav$current.indice-1))))
      # })
      # 
      # 
      
      
      # Initialization fo the process
      session$userData$mod_A_obs_1 <-  observeEvent(dataIn(), { 
        print('Initialisation du module')
        rv$dataIn <- dataIn()
      })
        
        # Just for the show absolutePanel
        output$currentObj <- renderUI({

          wellPanel(
              tagList(
                p('Live view of data from inside the module'),
                p(paste0('r.nav$current.indice = ', r.nav$current.indice)),
            fluidRow(
            column(3,
                    tags$p(tags$strong('rv$dataIn : ')),
                    tags$ul(
                      lapply(paste0(names(rv$dataIn ), "=", rv$dataIn ), 
                            function(x) tags$li(x))
                    )
                   ),
            column(3,
                   tags$p(tags$strong('rv$dataOut : ')),
                    tags$ul(
                      lapply(paste0(names(rv$dataOut ), "=", rv$dataOut ), 
                            function(x) tags$li(x))
                    )
                  )
          )
          )
          )
        })
        
        
      

      
       
       #####################################################################
       ## screens of the module
       
       ############### SCREEN 1 ######################################
       output$screen1 <- renderUI({
         tagList(
           tags$h3(r.nav$name)
         )
       })
       
       
       ############### SCREEN 2 ######################################
       
       output$screen2 <- renderUI({
         
         tagList(
           div(id=ns('screen2'),
               tags$h2('Step 1'),
               actionButton(ns('perform_screen2_btn'), 'Perform'),
               selectInput(ns('select1'), 'Select step 1', 
                           choices = 1:5, 
                           selected = 1,
                           width = '150px')
           )
         )
       })
       
       observeEvent(input$perform_screen2_btn, {
         newVal <- rv$dataIn[[length(rv$dataIn)]] + 1
         rv$dataIn <- append(rv$dataIn, setNames(newVal, paste0('screen', r.nav$current.indice)))
         r.nav$isDone[r.nav$current.indice] <- TRUE
       })
       
       
       ############### SCREEN 3 ######################################
       output$screen3 <- renderUI({
         
         tagList(
           div(id=ns('screen3'),
               tags$h3('Step 2'),
               actionButton(ns('perform_screen3_btn'), 'Perform'),
               selectInput(ns('select2'), 'Select step 2',
                           choices = 1:5,
                           selected = 1,
                           width = '150px')
           )
         )
       })
       
       ## Logics to implement: here, we must take the last data not null
       # in previous datas. The objective is to take account
       # of skipped steps
       observeEvent(input$perform_screen3_btn, {
         newVal <- rv$dataIn[[length(rv$dataIn)]] + 1
         rv$dataIn <- append(rv$dataIn, setNames(newVal, paste0('screen', r.nav$current.indice)))
         r.nav$isDone[r.nav$current.indice] <- TRUE
       })
       
       
       ############### SCREEN 4 ######################################
       output$screen4 <- renderUI({
         
         tagList(
           div(id=ns('screen4'),
               tags$h3('Step 4'),
               actionButton(ns('validate_btn'), 'Validate')
           )
         )
       })
         
          observeEvent(input$validate_btn, {
            isolate({
              rv$dataOut <- dataIn()
            rv$dataOut <- append(rv$dataOut, setNames(rv$dataIn[[length(rv$dataIn)]], r.nav$name))
            rv$dataIn <- NULL
            r.nav$isDone[r.nav$current.indice] <- TRUE
            })
       })
       
       
       ##########################################################
        
  reactive({rv$dataOut})
    }
  )
}

