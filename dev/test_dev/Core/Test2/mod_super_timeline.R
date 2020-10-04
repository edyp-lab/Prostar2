
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
               uiOutput(ns('show_dataIn'))),
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
mod_super_timeline_server <- function(id, 
                                      dataIn=NULL){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      source(file.path('.', 'debug_ui.R'), local=TRUE)$value
      
      
      

      #################################################################################
      # variables to communicate with the navigation module
      config <- reactiveValues(
        type = 'pipeline',
        process.name = 'Pipeline',
        stepsNames = c("Original", "Filtering", "Normalization", "Imputation"),
         mandatory =  c(TRUE, TRUE, FALSE, FALSE)
      )
      
      rv <- reactiveValues(
        current.pos = 1,
        timeline = NULL,
        remoteReset = NULL,
        dataIn = NULL,
        dataOut = NULL,
        sendPosition = 1,
        event_counter = 0,
        cmd = NULL,
        tmp = reactiveValues()
      )
      
      #--------------------------------------------------------------
      
      Init_isDone <- function(){
        setNames(lapply(1:nbSteps(), 
                        function(x){ x == 1}), 
                 config$stepsNames)
      }
      
      
      # Initialization of the process
      observeEvent(req(dataIn()), {
        print('------ MODULE SUPER_TIMELINE : Initialisation du module ------')
        rv$dataIn <- dataIn()
        rv$dataOut <- NULL
         
        rv$event_counter <- 0
        rv$screens <- InitActions(nbSteps())
        config$screens <- CreateScreens(nbSteps())
        config$isDone <- Init_isDone()
        config$stepsNames <- setNames(config$stepsNames,config$stepsNames)
        
        rv$timeline <- mod_timeline_server("timeline", 
                                   style = 2, 
                                   config = config, 
                                   cmd = reactive({rv$cmd}),
                                   position = reactive({rv$current.pos})
                                  )
        
        Launch_Module_Server()
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
      

      # This function cannot be implemented in the timeline module because 
      # the id of the screens to reset are not known elsewhere.
      # Trying to reset the global 'div_screens' in the timeline module
      # does not work
      ResetScreens <- function(screens){
        lapply(1:nbSteps(), function(x){
          shinyjs::reset(paste0('screen', x))
        })
      }
      
      SendCmdToTimeline <- function(names){
        append(as.list(names), list(rv$event_counter))
        #paste0(name, '_', rv$event_counter)
      }
      
      # Catches an clic on the next or previous button in the timeline
      # and updates the event_counter
      observeEvent(req(c(rv$timeline$nxtBtn()!=0, rv$timeline$prvBtn()!=0)),{
        # Add external events to counter
        rv$event_counter <- rv$event_counter + rv$timeline$rstBtn()
      })
      # ------------ END OF COMMON FUNCTIONS --------------------
      
      
      #Catch a reset command from timeline
      observeEvent(req(rv$timeline$rstBtn()!=0), {
        print("---- MODULE SUPER_TIMELINE : reset activated")
        
        # Add external events to counter
        rv$event_counter <- rv$event_counter + rv$timeline$rstBtn()
        
        rv$cmd <- SendCmdToTimeline(c('EnableAllSteps', 'ResetActionBtns'))
        config$isDone <- Init_isDone()
        
        rv$current.pos <- 1
        rv$event_counter <- 0
        ResetScreens()
        
        # Update datasets logics
        Reset_Pipeline_Data_logics()
      })
      
      Reset_Pipeline_Data_logics <- function(){
        rv$dataIn <- dataIn()
        rv$dataOut <- NULL
      }
      
      GetMaxTrue <- reactive({ max(which(unlist(config$isDone)==T)) })
      
      #To avoid intempestive initializations of modules due to dataIn changes
      # one define the following logics :
      #  A dataset is loaded in a module only if this module is not yet
      # validated and if it has not been skipped (this is why we use the
      # max True function
      # To rerun a validated module, the user has to reset it
      # This function returns a non-NULL value to the module which name
      # corresponds to the current position
      SendCurrentDataset <- function(name){
        #browser()
        data <- NULL
        if (names(config$isDone)[rv$current.pos] == name && rv$current.pos > GetMaxTrue())
            data <- rv$dataIn
        print(paste0('MODULE TL : SendCurrentDataset from pos = ', rv$current.pos, ', name = ', name, ' = ', names(data)))
        
        return(data)
      }
      
      #Catch a new position from timeline
      observeEvent(req(rv$timeline$pos()),{ 
        print('---- MODULE TL = new position detected')
        rv$current.pos <- rv$timeline$pos()
        })
      
### End of part for managing the timeline
      
      
     # observeEvent(rv$tmpA(), { rv$tmp <- rv$tmpA()})
     # observeEvent(rv$tmpB(), { rv$tmp <- rv$tmpB()})
     # observeEvent(rv$tmpC(), { rv$tmp <- rv$tmpC()})
      
      
      # Catch the return value of a module and update the list of isDone modules
      # This list is updated with the names of datasets present in the rv$tmp
      # variable. One set to TRUE all the elements in isDone which have a corresponding
      # element in names(rv$tmp).
      # One cannot simply set to TRUE the last element of rv$tmp because it will does
      # not work in case of a reseted module (it is not in the names(rv$tmp) list
      # anymore)
      # If a value (not NULL) is received, then it corresponds to the module
      # pointed by the current position
      observeEvent(req(lapply(reactiveValuesToList(rv$tmp), function(x){x()})), ignoreNULL = T, ignoreInit=T, { 
        print("----- MODULE SUPER_TL : reception d'un retour sur rv$tmp")
        #browser()
        module_which_returned <- names(which(lapply(reactiveValuesToList(rv$tmp), 
                                                    function(x){!is.null(x())}) == T))
        rv$dataIn <- rv$tmp[[module_which_returned]]()
        rv$dataOut <- rv$dataIn
        # The last TRUE value of the list is on the current pos
        #current.name <- names(config$stepsNames)[rv$current.pos]
        config$isDone[[module_which_returned]] <- TRUE
      })


      
      
     
      Launch_Module_Server <- function(){
        rv$tmp[['Filtering']] <- mod_wf_wf1_A_server("mod_A_nav",
                                                     dataIn = reactive({SendCurrentDataset('Filtering')}),
                                                     remoteReset = reactive({rv$timeline$rstBtn()}),
                                                     forcePosition = reactive({NULL})
        )
        
        rv$tmp[['Normalization']] <- mod_wf_wf1_B_server("mod_B_nav",
                                                         dataIn = reactive({SendCurrentDataset('Normalization')}),
                                                         remoteReset = reactive({rv$timeline$rstBtn()}),
                                                         forcePosition = reactive({rv$current.pos})
        )
        
        rv$tmp[['Imputation']] <- mod_wf_wf1_C_server("mod_C_nav",
                                                      dataIn = reactive({SendCurrentDataset('Imputation')}),
                                                      remoteReset = reactive({rv$timeline$rstBtn()}),
                                                      forcePosition = reactive({rv$current.pos})
        )
      }
      
      
      
      
      
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

