
# source(file.path('../../../../R', 'mod_navigation.R'), local=TRUE)$value
# source(file.path('../../../../R', 'global.R'), local=TRUE)$value


mod_wf_wf1_C_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('show')),
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
mod_wf_wf1_C_server <- function(id, dataIn=NULL){
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
        reset = FALSE
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
      
      output$show_dataIn <- renderPrint({rv$dataIn})
      output$show_dataOut <- renderPrint({rv$dataOut})
      

      observeEvent(req(r.nav$reset),{
        
        for (i in 1:length(r.nav$stepsNames))
          shinyjs::reset(paste0('screen', i))
        
        if (r.nav$isDone[length(r.nav$stepsNames)])
          rv$dataOut <- dataIn()[-length(dataIn())]
        else
          rv$dataIn <- dataIn()
        
        
        r.nav$reset  <- FALSE
        
        # Set all steps to undone except the first one which is the description screen
        r.nav$isDone <- c(TRUE, rep(FALSE, length(r.nav$stepsNames)-1))
        
      })
      
     
      
      
      
      # Initialization fo the process
      session$userData$mod_C_obs_1 <-  observeEvent(dataIn(), { 
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
          rv$dataIn <- addAssay(rv$dataIn, rv$dataIn[[length(rv$dataIn)]], name='Process_A')
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

