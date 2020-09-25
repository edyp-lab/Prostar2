
# source(file.path('../../../../R', 'mod_navigation.R'), local=TRUE)$value
# source(file.path('../../../../R', 'global.R'), local=TRUE)$value


mod_wf_wf1_C_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('show_UI')),
    wellPanel(
      p('rv$dataIn :'),
      verbatimTextOutput(ns('show_dataIn')),
      p('rv$dataOut'),
      verbatimTextOutput(ns('show_dataOut'))
    )
  )
}

#' @param dataIn xxx
#'
#' 
#' 
mod_wf_wf1_C_server <- function(id, dataIn=NULL, remoteReset=FALSE){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      rv <-reactiveValues()
      
      # variables to communicate with the navigation module
      r.nav <- reactiveValues(
        name = "Process C",
        stepsNames = c("C - Description", "C - Step 1", "C - Step 2", "C - Step 3"),
        ll.UI = list( screenStep1 = uiOutput(ns("screen1")),
                      screenStep2 = uiOutput(ns("screen2")),
                      screenStep3 = uiOutput(ns("screen3")),
                      screenStep4 = uiOutput(ns("screen4"))
        ),
        isDone =  c(TRUE, FALSE, FALSE, FALSE),
        mandatory =  c(FALSE, FALSE, TRUE, TRUE),
        start = 1
      )
      
      timeline <- mod_navigation_server("test_nav", style = 2, pages = r.nav)
      
      
      output$show_UI <- renderUI({
        tagList(
          mod_navigation_ui(ns("test_nav")),
          timeline()
        )
      })
      
      output$show_dataIn <- renderPrint({rv$dataIn})
      output$show_dataOut <- renderPrint({rv$dataOut})
      
      observeEvent(r.nav$isDone, {
        print("new event on isDone")
        inames <- names(input)
        print(inames)
        # Disable all input from test_nav but the action buttons
        
        #browser()
        # Disable all previous screens but the action buttons of the timeline
        # if (pages$isDone[current$val])
        #   lapply(1:current$val, function(x){ shinyjs::disable(paste0('screen', x))})
      })

      observeEvent(req(c(r.nav$reset, remoteReset())),{
        #print('Module A : Activation of the reset variable')
        #print(paste0('r.nav$reset = ', r.nav$reset))
        #print(paste0('remoteReset() = ', remoteReset()))
        
        # Re-enable all screens
        lapply(1:length(r.nav$stepsNames), function(x){shinyjs::enable(paste0('screen', x))})
        
        r.nav$start <- 1
        
        # Reload previous dataset
        for (i in 1:length(r.nav$stepsNames))
          shinyjs::reset(paste0('screen', i))
        
        if (r.nav$isDone[length(r.nav$stepsNames)])
          rv$dataOut <- dataIn()[-length(dataIn())]
        else
          rv$dataIn <- dataIn()
        
        # Set all steps to undone except the first one which is the description screen
        r.nav$isDone <- c(TRUE, rep(FALSE, length(r.nav$stepsNames)-1))
        
        rv$dataOut <- NULL
      })
      
     
      
      
      
      # Initialization fo the process
      session$userData$mod_C_obs_1 <-  observeEvent(dataIn(), { 
        print('Initialisation du module C')
        rv$dataIn <- dataIn()
      })
      
      
      
      
      
      
      
      #####################################################################
      ## screens of the module
      
      ############### SCREEN 1 ######################################
      output$screen1 <- renderUI({
        tagList(
          tags$h1('Description of the module')
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
        #rv$dataIn <- rv$dataIn[[length(rv$dataIn)]] + as.numeric(input$select1)
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
        #rv$dataIn <- rv$dataIn[[length(rv$dataIn)]] + as.numeric(input$select2)
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
        
      observeEvent(input$validate_btn, {
        isolate({
          rv$dataIn <- addAssay(rv$dataIn, rv$dataIn[[length(rv$dataIn)]], name='Process_C')
          rv$dataOut <- rv$dataIn
          rv$dataIn <- NULL
          r.nav$isDone[4] <- TRUE
        })

      })
      
      ##########################################################
      
      reactive({rv$dataOut})
    }
  )
}

