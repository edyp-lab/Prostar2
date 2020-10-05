
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
        column(width=4,
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
        mandatory =  c(TRUE, FALSE, FALSE, FALSE)
      )
      
      rv <- reactiveValues(
        # Current position of the cursor in the timeline
        current.pos = 1,
        
        # Use to pass the reset info to the process modules
        remoteReset = NULL,
        
        dataIn = NULL,
        dataOut = NULL,
        sendPosition = 1,
        
        # Used to create a different value for some variables in order to 
        # trigger observeEvent functions
        event_counter = 0,
        
        # A vector of strings which are commands keywords to manipulate the timeline
        cmd = NULL,
        
        # Variable to store the results of process modules
        tmp = reactiveValues()
      )
      
      #--------------------------------------------------------------
      
      Init_isDone <- function(){
        setNames(lapply(1:nbSteps(), 
                        function(x){ x == 1}), 
                 names(config$stepsNames))
      }
      
      
      # Initialization of the process
      observeEvent(req(dataIn()), {
       # print('------ MODULE SUPER_TIMELINE : Initialisation du module ------')
        rv$dataIn <- dataIn()
        rv$dataOut <- dataIn()
         
        rv$event_counter <- 0
        rv$screens <- InitActions(nbSteps())
        config$screens <- CreateScreens(nbSteps())
        config$stepsNames <- setNames(config$stepsNames,config$stepsNames)
        config$stepsNames[1] <- 'Description'
        config$isDone <- Init_isDone()
        
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
        #print("---- MODULE SUPER_TIMELINE : reset activated")
        
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
        rv$dataOut <- dataIn()
      }
      
      GetMaxTrue <- function(bound = nbSteps()){
         max(which(unlist(config$isDone)[1:bound]==T))
        }
      
      #To avoid intempestive initializations of modules due to dataIn changes
      # one define the following logics :
      #  A dataset is loaded in a module only if this module is not yet
      # validated and if it has not been skipped (this is why we use the
      # max True function
      # To rerun a validated module, the user has to reset it
      # This function returns a non-NULL value to the module which name
      # corresponds to the current position and one send always the last
      # non-NULL dataset before current position
      SendCurrentDataset <- function(name){
        #browser()
        data2send <- NULL
        if (names(config$isDone)[rv$current.pos] != name) return(NULL)
        #print(paste0('--- MODULE SUPER TL, current.pos = ', rv$current.pos))
        if (config$isDone[[rv$current.pos]]){
          # The processus is already validated and the reception of a value
          # means that the user has reseted it and want to revalidate this module
          data2send <- rv$dataOut
          #browser()
        } else {
          # The processus has not yet been validated and one want to run it and 
          # validate it
          if (GetMaxTrue() < rv$current.pos) {
            data2send <- rv$dataOut
          } else {
            # it is a skipped module
            name <- names(config$isDone)[GetMaxTrue(bound = rv$current.pos-1)]
            ind.name <- grep(name, names(rv$dataOut))
            data2send <- rv$dataOut[,,-c((ind.name+1):length(rv$dataOut))]
          }
          # Test if the process has been skipped or if it is a new process
          # Name of the last validated dataset before the current one
          
        }
        
      
        #print(paste0('MODULE TL : SendCurrentDataset from pos = ', rv$current.pos, ', name = ', name, ' = ', names(data2send)))
        
        return(data2send)
      }
      
      #Catch a new position from timeline
      observeEvent(req(rv$timeline$pos()),{ 
        #print('---- MODULE TL = new position detected')
        rv$current.pos <- rv$timeline$pos()
        })
      
### End of part for managing the timeline
      

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
        #print("----- MODULE SUPER_TL : reception d'un retour sur rv$tmp")
        #browser()
        if ((length(unlist(lapply(reactiveValuesToList(rv$tmp), function(x){x()}))) == 1) 
          && (length(which(config$isDone==T))) ){
          print("It is a global reset")
          return(NULL)
        }
        # Compare the current config$isDone with rv$tmp to see which dataset
        # has changed. One compares the item which has changed between the two lists
        # if a dataset passed from NULL to a non-NULL value then is has been validated
        # else if the dataset passed from a non-NULL value to NULL, it has been reseted
        module_which_returned <- unlist(lapply(names(config$isDone), 
                                               function(x){
                                                 if (!is.null(reactiveValuesToList(rv$tmp)[[x]]()) != config$isDone[[x]]) {x}
                                                 }
                                               )
                                        )
       
        #module_which_returned <- names(which(lapply(reactiveValuesToList(rv$tmp), 
        #                                            function(x){!is.null(x())}) == T))
        
        if (is.null(rv$tmp[[module_which_returned]]())){
          # This means that the corresponding module has been reseted
          config$isDone[[module_which_returned]] <- FALSE
         browser() 
          # renvoyer le dernier dataset non NULL avant la position courante
          name <- names(config$isDone)[ GetMaxTrue(rv$current.pos - 1)]
          ind.name <- grep(name, names(rv$dataOut))
          rv$dataOut <- rv$dataOut[,,-c((ind.name+1):length(rv$dataOut))]
          #rv$dataOut <- rv$dataOut[,,-c(rv$current.pos:length(rv$dataIn))]
          #rv$dataIn <- rv$dataIn[,,-c(rv$current.pos:length(rv$dataIn))]
        } else {
          # This means that the corresponding module has return a value
          # It has been validated
          config$isDone[[module_which_returned]] <- TRUE
          # Set to FALSE all further steps (in case of rerun a process
          #browser()
          ind <- which(names(config$isDone)==module_which_returned)
          if (ind < nbSteps())
            config$isDone[(1+ind):nbSteps()] <- FALSE
          #rv$dataIn <- rv$tmp[[module_which_returned]]()
          
          rv$dataOut <- rv$tmp[[module_which_returned]]()
        }
        
      })


      
      
     
      Launch_Module_Server <- function(){
        BuildServer <- function(name){
          if (name == 'Original'){
            rv$tmp[[name]] <- reactive({dataIn()})
          } else {
            rv$tmp[[name]] <- do.call('mod_wf_wf1_Filtering_server', 
                                      list(paste0("mod_",name, "_nav"),
                                           dataIn = reactive({SendCurrentDataset(name)}),
                                           remoteReset = reactive({rv$timeline$rstBtn()})))
          }
        }
        
        lapply(1:nbSteps(), function(x){BuildServer(names(config$stepsNames)[x])})
        
        
        # rv$tmp[['Original']] <- reactive({dataIn()})
        # 
        # rv$tmp[['Filtering']] <- mod_wf_wf1_Filtering_server("mod_Filtering_nav",
        #                                              dataIn = reactive({SendCurrentDataset('Filtering')}),
        #                                              remoteReset = reactive({rv$timeline$rstBtn()})
        # )
        # 
        # rv$tmp[['Normalization']] <- mod_wf_wf1_Normalization_server("mod_Normalization_nav",
        #                                                  dataIn = reactive({SendCurrentDataset('Normalization')}),
        #                                                  remoteReset = reactive({rv$timeline$rstBtn()})
        # )
        # 
        # rv$tmp[['Imputation']] <- mod_wf_wf1_Imputation_server("mod_Imputation_nav",
        #                                               dataIn = reactive({SendCurrentDataset('Imputation')}),
        #                                               remoteReset = reactive({rv$timeline$rstBtn()})
        # )
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
              mod_wf_wf1_Filtering_ui(ns('mod_Filtering_nav'))
          )
        )
      })

      
      
         ############### SCREEN 3 ######################################
      output$screen3 <- renderUI({
        tagList(
          div(id=ns('screen3'),
              tags$h3(config$stepsName[3]),
              mod_wf_wf1_Normalization_ui(ns('mod_Normalization_nav'))
          )
        )
      })

     
      ############### SCREEN 4 ######################################
      output$screen4 <- renderUI({
        tagList(
          div(id=ns('screen4'),
              tags$h3(config$stepsName[4]),
              mod_wf_wf1_Imputation_ui(ns('mod_Imputation_nav'))
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

