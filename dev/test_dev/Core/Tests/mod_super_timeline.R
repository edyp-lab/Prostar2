
mod_super_timeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('show')),
    wellPanel(
      # Just for the show absolutePanel
      
          tagList(
            p('Live view of data from inside the super timeline module'),
            fluidRow(
              column(3,
                     tags$p(tags$strong('rv$dataIn : ')),
                     verbatimTextOutput('show_dataIn')
              ),
              column(3,
                     tags$p(tags$strong('rv$dataOut : ')),
                     verbatimTextOutput('show_dataOut')
              )
            )
          )
    )
  )
}

#' @param dataIn xxx
#'
#' 
#' 
mod_super_timeline_server <- function(id, dataIn=NULL){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      rv <-reactiveValues()
      
      observeEvent(dataIn(), {
        print('Initialisation du pipeline X')
        rv$dataIn <- dataIn()
        })
      
      # variables to communicate with the navigation module
      r.nav <- reactiveValues(
        name = "Pipeline X",
        stepsNames = c("Description", "Proc 1", "Proc 2", "Proc 3", "Summary"),
        ll.UI = list( screenStep1 = uiOutput(ns("screen1")),
                      screenStep2 = uiOutput(ns("screen2")),
                      screenStep3 = uiOutput(ns("screen3")),
                      screenStep4 = uiOutput(ns("screen4")),
                      screenStep5 = uiOutput(ns("screen5"))),
        isDone =  c(TRUE, FALSE, FALSE, FALSE, FALSE),
        mandatory =  c(FALSE, FALSE, TRUE, TRUE, TRUE),
        reset = FALSE
      )
      
      screens <- mod_navigation_server("super_nav", 
                                       style = 2, 
                                       pages = r.nav)
      
      
      output$show <- renderUI({
        tagList(
          mod_navigation_ui(ns("super_nav")),
          hr(),
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
      
      
      

      
      #####################################################################
      ## screens of the module
      
      ############### SCREEN 1 ######################################
      output$screen1 <- renderUI({
        tagList(
          tags$h1('Description of the pipeline')
        )
      })
      
      
      ############### SCREEN 2 ######################################
      
      output$screen2 <- renderUI({
        
        tagList(
          div(id=ns('screen2'),
              tags$h3('Processus 1'),
              mod_wf_wf1_A_ui(ns('mod_A_nav'))
          )
        )
      })
      
      rv$tmpA <- mod_wf_wf1_A_server("mod_A_nav", dataIn = reactive({rv$dataIn}) )
      observeEvent(rv$tmpA(),  { 
        rv$dataIn <- rv$tmpA()
        r.nav$isDone[2] <- TRUE
      })
      
      
      
      ############### SCREEN 3 ######################################
      output$screen3 <- renderUI({
        
        tagList(
          div(id=ns('screen3'),
              tags$h3('Processus 2'),
              mod_wf_wf1_B_ui(ns('mod_B_nav'))
          )
        )
      })
      
      rv$tmpB <- mod_wf_wf1_B_server("mod_B_nav", dataIn = reactive({rv$dataIn}) )
      observeEvent(rv$tmpB(),  {
        rv$dataIn <- rv$tmpB()
        r.nav$isDone[3] <- TRUE
        })
      
      
      
      ############### SCREEN 4 ######################################
      output$screen4 <- renderUI({
        
        tagList(
          div(id=ns('screen4'),
              tags$h3('Processus 3'),
              mod_wf_wf1_C_ui(ns('mod_C_nav'))
          )
        )
      })
      
      rv$tmpC <- mod_wf_wf1_C_server("mod_C_nav", dataIn = reactive({rv$dataIn}) )
      observeEvent(rv$tmpC(),  { 
        rv$dataIn <- rv$tmpC()
        r.nav$isDone[4] <- TRUE
        })
      
      
      ############### SCREEN 5 ######################################
      output$screen5 <- renderUI({
        
        tagList(
          div(id='screen5',
              tags$h3('Summary')
          )
        )
      })
      
      ##########################################################
      
      reactive({rv$dataOut})
    }
  )
}

