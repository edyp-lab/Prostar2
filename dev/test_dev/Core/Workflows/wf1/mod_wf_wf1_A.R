
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
      
      init.rv <- reactive({
        #rv$dataIn <- NULL
        #rv$dataOut <- NULL
        #rv$currentData <- 0
        rv <- reactiveValues(
          dataIn = NULL,
          dataOut = NULL,
          currentData = 0
        )
      })
      
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
                                      pages = r.nav,
                                      start = NULL)
      
      
      output$show <- renderUI({
        tagList(
          mod_navigation_ui(ns("test_nav")),
          screens()
        )
      })
      
        
        observeEvent(req(r.nav$reset),{
          r.nav$reset  <- FALSE
          print('reset activated from navigation module')
          r.nav$isDone <- c(TRUE, rep(FALSE, length(r.nav$stepsNames)-1))
          for (i in 1:length(r.nav$stepsNames))
            shinyjs::reset(paste0('screen', i))
          browser()
          init.rv()
          rv$dataIn <- dataIn()
          rv$dataOut <- NULL
          rv$currentData <- 0
        })

        
        # Just for the show absolutePanel
        output$currentObj <- renderUI({

          wellPanel(
              tagList(
                p('Live view of data from inside the module'),
                p(paste0('rv$currentData = ', rv$currentData)),
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
          )
        })
        
        
        
      # Initialization fo the process
        session$userData$mod_A_obs_1 <-  observeEvent(dataIn(), { 
          init.rv()
          rv$dataIn <- dataIn()
          rv$currentData <- rv$dataIn[[length(rv$dataIn)]]
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
         rv$currentData <- rv$currentData + 1
         r.nav$isDone[2] <- TRUE
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
       
       observeEvent(input$perform_screen3_btn, {
         rv$currentData <- rv$currentData +1
         r.nav$isDone[3] <- TRUE
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
         
          observeEvent(input$validate_btn, ignoreInit = T,{
            rv$dataIn <- append(rv$dataIn, setNames(rv$currentData+rv$dataIn[[length(rv$dataIn)]], c(r.nav$name)))
            rv$dataOut <- rv$dataIn
            r.nav$isDone[4] <- TRUE
       })
       
       
       ##########################################################
        
  reactive({rv$dataOut})
    }
  )
}

