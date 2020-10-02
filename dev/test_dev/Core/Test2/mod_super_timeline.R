
mod_super_timeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    uiOutput(ns('show_screens')),
    hr(),
    wellPanel(
      h3('Pipeline'),
      fluidRow(
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Data input")),
               uiOutput(ns('show_dataIn')) ),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Data output")),
               uiOutput(ns('show_rv_dataOut'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Current pos")),
               uiOutput(ns('show_currentPos'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "List 'isDone'")),
               uiOutput(ns('show_isDone')))
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
      
      output$show_currentPos <- renderUI({
        req(rv$current.pos)
        p(as.character(rv$current.pos))
      })
      output$show_dataIn <- renderUI({
        tagList(lapply(names(dataIn()), function(x){tags$p(x)}))
      })
      output$show_rv_dataIn <- renderPrint({names(rv$dataIn)})
      output$show_rv_dataOut <- renderUI({
        tagList(
          lapply(names(rv$dataOut), function(x){tags$p(x)})
        )
      })
      output$show_isDone <- renderUI({
        req(config$isDone)
        config$isDone <- setNames(config$isDone, config$stepsNames)
        tagList(lapply(names(config$isDone), 
                       function(x){tags$p(paste0(x, ' - ', config$isDone[[x]]))}))
      })
      
      rv <- reactiveValues(
        current.pos = 1,
        timeline = NULL,
        remoteReset = NULL,
        dataIn = NULL,
        dataOut = NULL,
        maxTRUE = 1,
        sendPosition = 1
      )
      
      # Commands to send to timeline_server
      actions <- reactiveValues(
        btns = list(
          rst = TRUE,
          nxt = TRUE,
          prv = TRUE),
        screens = NULL)
      
      # variables to communicate with the navigation module
      config <- reactiveValues(
        type = 'pipeline',
        process.name = 'Pipeline',
        stepsNames = c("Description", "Filtering", "Normalization", "Imputation", "Summary"),
         mandatory =  c(FALSE, FALSE, TRUE, FALSE, TRUE),
        isDone =  NULL
      )
      #################################################################################
      
      
      #--------------------------------------------------------------
      
   
      # Initialization of the process
      observeEvent(req(dataIn()), {
        print('------ MODULE SUPER_TIMELINE : Initialisation du module ------')
        rv$dataIn <- dataIn()
        rv$dataOut <- NULL
         
        actions$screens <- InitActions(nbSteps())
        config$screens <- CreateScreens(nbSteps())
        config$isDone <- setNames(lapply(1:nbSteps(), function(x){F}), config$stepsNames)
        
        rv$timeline <- mod_timeline_server("timeline", 
                                   style = 2, 
                                   config = config, 
                                   actions = reactive({actions}),
                                   position = reactive({rv$current.pos})
                                  )
        })
      

      
      # ------------ START OF COMMON FUNCTIONS --------------------
      InitActions <- function(n){
        setNames(lapply(1:n,
                        function(x){T}),
                 paste0('screen', 1:n)
        )
      }
      
      CreateScreens <- function(n){
        setNames(
          lapply(1:n, 
                 function(x){
                   do.call(uiOutput, list(outputId=ns(paste0("screen", x))))}),
          paste0('screenStep', 1:n))
      }
      
      nbSteps <- reactive({
        req(config$stepsNames)
        length(config$stepsNames)
      })
      
      
      DisableAllPrevSteps <- function(screens){
       # browser()
        
          lapply(1:rv$maxTRUE, function(x) screens[[x]] <- FALSE)
      }
      
      DisableAllSteps <- function(screens){
        lapply(screens, function(x) x <- FALSE)
      }
      
      EnableAllSteps <- function(screens){
        lapply(screens, function(x) x <- TRUE)
      }
      
      
      ResetScreens <- function(screens){
        lapply(1:nbSteps(), function(x){
          shinyjs::reset(paste0('screen', x))
        })
      }
      
      
      ResetActionBtns <- function(btns){lapply(btns, function(x){x <- T})}
      
      # ------------ END OF COMMON FUNCTIONS --------------------
      
      #Catch a new position from timeline
      observeEvent(req(rv$timeline$pos()),{ 
        print('---- MODULE TL = new position detected')
        rv$current.pos <- rv$timeline$pos()
        
        })
      
      
      
      #Catch a reset command from timeline
      observeEvent(req(rv$timeline$rstBtn()!=0), {
        print("---- MODULE SUPER_TIMELINE : reset activated")
        
        actions$screens <- EnableAllSteps(actions$screens)
        ResetScreens()
        
        config$isDone <- c(TRUE, rep(FALSE, nbSteps()-1))
        actions$btns <- ResetActionBtns(actions$btns)
        rv$current.pos <- 1
        
        # Update datasets logics
        rv$dataIn <- dataIn()
        rv$dataOut <- NULL
      })
      
      
      
      # Catch a change in isDone (validation of a step)
      #observeEvent(config$isDone,  ignoreInit = T, {
      #  print(' ------- MODULE SUPER_TIMELINE : A new step is validated ------- ')
      #  #actions$screens <- DisableAllPrevSteps(actions$screens)
        
      #  if (length(which(unlist(config$isDone)==T))>0){
      #    rv$maxTRUE <- max(which(unlist(config$isDone)==T))
      #  } else {
      #    rv$maxTRUE <- 1
      #  }
      #})


      
      observeEvent(rv$tmpA(), { rv$tmp <- rv$tmpA()})
      observeEvent(rv$tmpB(), { rv$tmp <- rv$tmpB()})
      observeEvent(rv$tmpC(), { rv$tmp <- rv$tmpC()})
      
      
      # Catch the return value of a module
      observeEvent(req(rv$tmp), ignoreNULL = F, { 
        print("----- MODULE SUPER_TL : reception d'un retour sur rv$tmp")
        rv$dataIn <- rv$tmp
        rv$dataOut <- rv$dataIn
        
        # The last TRUE value of the list is on the current pos
        config$isDone <- Update_isDone_List(config$isDone, new.names=names(rv$tmp) )
        
      })
      
      Update_isDone_List <- function(ll, new.names){
        ind <- lapply(new.names, function(x){grep(x, names(ll))})
        config$isDone <- setNames(lapply(1:length(ll), 
                                         function(x) {ll[[x]]<- x %in% ind}),
                                  config$stepsNames)
      }
      
      
      
      rv$tmpA <- mod_wf_wf1_A_server("mod_A_nav",
                                     dataIn = reactive({rv$dataIn}),
                                     remoteReset = reactive({rv$timeline$rstBtn()}),
                                     forcePosition = reactive({rv$current.pos})
                                      )
      
      rv$tmpB <- mod_wf_wf1_B_server("mod_B_nav",
                                     dataIn = reactive({rv$dataIn}),
                                     remoteReset = reactive({rv$timeline$rstBtn()}),
                                     forcePosition = reactive({rv$current.pos})
                                      )
      
      rv$tmpC <- mod_wf_wf1_C_server("mod_C_nav",
                                     dataIn = reactive({rv$dataIn}),
                                     remoteReset = reactive({rv$timeline$rstBtn()}),
                                     forcePosition = reactive({rv$current.pos})
                                      )
      
      
      
      
      
      
      #####################################################################
      ## screens of the module
      ##
      ############### SCREEN 1 ######################################
      output$screen1 <- renderUI({
        tagList(
          tags$h3(paste0('Pipeline ', config$name))
        )
      })
      
      
      ############### SCREEN 2 ######################################
      
      output$screen2 <- renderUI({
        tagList(
          div(id=ns('screen2'),
              tags$h3(config$stepsName[2]),
              mod_wf_wf1_A_ui(ns('mod_A_nav'))
          )
        )
      })

      
      
         ############### SCREEN 3 ######################################
      output$screen3 <- renderUI({
        
        tagList(
          div(id=ns('screen3'),
              tags$h3(config$stepsName[3]),
              mod_wf_wf1_B_ui(ns('mod_B_nav'))
          )
        )
      })
      
      
     
      ############### SCREEN 4 ######################################
      output$screen4 <- renderUI({
        tagList(
          div(id=ns('screen4'),
              tags$h3(config$stepsName[4]),
              mod_wf_wf1_C_ui(ns('mod_C_nav'))
          )
        )
      })
      
      rv$tmpC <- mod_wf_wf1_C_server("mod_C_nav",
                                   dataIn = reactive({rv$dataIn}),
                                     remoteReset = reactive({rv$timeline$rstBtn()})
                                    )
      
      
      ############### SCREEN 5 ######################################
      output$screen5 <- renderUI({
        
        tagList(
          div(id='screen5',
              tags$h3(config$stepsName[5])
          )
        )
      })
      
      ##########################################################
      
      reactive({rv$dataOut})
    }
  )
}

