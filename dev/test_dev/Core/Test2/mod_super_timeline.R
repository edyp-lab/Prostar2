

mod_super_timeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    uiOutput(ns('show_screens')),
    hr(),
    wellPanel(
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
                                      dataIn=NULL,
                                      config){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      verbose <- T
      source(file.path('.', 'debug_ui.R'), local=TRUE)$value
      #source(file.path('.', 'code_general.R'), local=TRUE)$value
      
      rv <- reactiveValues(
        # Current position of the cursor in the timeline
        current.pos = 1,
        
        # Use to pass the reset info to the process modules
        remoteReset = NULL,
        
        #dataIn = NULL,
        dataOut = NULL,
        
        # A vector of strings which are commands keywords to manipulate the timeline
        cmd = NULL,
        
        # Variable to store the results of process modules
        tmp = reactiveValues()
      )
      
      #--------------------------------------------------------------
     
      VALIDATED <- 1
      UNDONE <- 0
      SKIPPED <- -1
      
      InitActions <- function(n){
        setNames(lapply(1:n,
                        function(x){T}),
                 paste0('screen', 1:n)
        )
      }
      
      CreateScreens <- function(names){
        setNames(
          lapply(1:length(names), 
                 function(x){
                   do.call(uiOutput, list(outputId=ns(names)[x]))}),
          paste0('screen_', names(config$steps)))
      }
      
      nbSteps <- reactive({
        req(config$steps)
        length(config$steps)
      })
      
      
      
      InsertDescriptionUI <- reactive({
        output[['Description']] <- renderUI({
          mod_insert_md_ui(ns(paste0(config$process.name, "_md")))
        })
        mod_insert_md_server(paste0(config$process.name, "_md"), 
                             paste0('./md/',config$process.name, '.md'))
      })
      
      
      
      
      
      # Initialization of the process
      observeEvent(req(dataIn()), {
        if(verbose)
          print(paste0(config$process.name, ' :  Initialisation du module ------'))

        #rv$dataIn <- dataIn()
        rv$dataOut <- dataIn()
        
        rv$screens <- InitActions(nbSteps())
         
        # Must be placed after the initialisation of the 'config$stepsNames' variable
        config$screens <- CreateScreens(names(config$steps))
        rv$timeline <- mod_timeline_server("timeline", 
                                   style = 2, 
                                   config = config)
        BuildScreensUI()
        Launch_Module_Server()
        })
      

      
     

      # This function cannot be implemented in the timeline module because 
      # the id of the screens to reset are not known elsewhere.
      # Trying to reset the global 'div_screens' in the timeline module
      # does not work
      ResetScreens <- function(screens){
        lapply(1:nbSteps(), function(x){
          shinyjs::reset(names(config$steps)[x])
        })
      }
      
      # ------------ END OF COMMON FUNCTIONS --------------------
      
      
      #Catch a reset command from timeline
      observeEvent(req(rv$timeline$rstBtn()!=0), {
        if(verbose)
          print(paste0(config$process.name, " : reset activated"))
        
        rv$cmd <- SendCmdToTimeline(c('EnableAllSteps', 'ResetActionBtns'))
        config$isDone <- Init_isDone()
        
        rv$current.pos <- 1
        ResetScreens()
        
        # Update datasets logics
        Reset_Pipeline_Data_logics()
      })
      
      
      
      
     
      
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
        if(verbose)
          print(paste0(config$process.name, ' : SendCurrentDataset()'))
        browser()
        data2send <- NA
        
        # Returns NULL to all modules except the one pointed by the current position

        if (names(config$isDone)[rv$current.pos] == name){
        if (name == 'Original')
          data2send <- NA
        else {
        #print(paste0('--- MODULE SUPER TL, current.pos = ', rv$current.pos))
        if (config$isDone[[rv$current.pos]]){
          # This case does not normally exists because a validated process
          # is disabled and the user cannot validate it.
          # For that, it must be reseted
        } else {
          # The processus is not validated
          if (GetMaxValidated(config$isDone, nbSteps()) < rv$current.pos) {
            # The current position is after the last validated process (the one
            # just after or further (there will be skipped processes))
            data2send <- rv$dataOut
          } else {
            # it is a skipped module
            # previous.validated.name <- names(config$isDone)[GetMaxValidated(tab = config$isDone, bound = rv$current.pos-1)]
            # ind.name <- grep(previous.validated.name, names(rv$dataOut))
            # data2send <- rv$dataOut[,,-c((ind.name+1):length(rv$dataOut))]
             data2send <- rv$dataOut
          }
}

        }
        }
      
if(verbose){
          if (length(names(data2send))==0)
            print(paste0(config$process.name, ' : SendCurrentDataset() to ', name, '(',rv$current.pos, ') => NA'))
          else
            print(paste0(config$process.name, ' : SendCurrentDataset() to ', name, '(',rv$current.pos, ') => ', paste0(names(data2send), collapse=' ')))
        }

        return(data2send)
      }
      
      #Catch a new position from timeline
      observeEvent(req(rv$timeline$pos()),{
        if(verbose)
          print(paste0(config$process.name, ' : observeEvent(req(rv$timeline$pos(). New position = ', rv$timeline$pos()))
        rv$current.pos <- rv$timeline$pos()
        })
      
### End of part for managing the timeline
      
    BuildStatus <- reactive({
      status <- setNames(lapply(1:nbSteps(), 
                                function(x){ if (x == 1) VALIDATED else UNDONE}), 
                         names(config$steps))
      
      
      
      status
    })
      
      # Catch the return value of a module and update the list of isDone modules
      # This list is updated with the names of datasets present in the rv$tmp
      # variable. One set to TRUE all the elements in isDone which have a corresponding
      # element in names(rv$tmp).
      # One cannot simply set to TRUE the last element of rv$tmp because it will does
      # not work in case of a reseted module (it is not in the names(rv$tmp) list
      # anymore)
      # If a value (not NULL) is received, then it corresponds to the module
      # pointed by the current position
      # This function also updates the list isDone
      observeEvent(req(lapply(reactiveValuesToList(rv$tmp), function(x){x()})), ignoreNULL = T, ignoreInit=T, { 
        if(verbose)
          print(paste0(config$process.name, " : reception d'un retour sur rv$tmp"))
        
        #browser()
        # if ((length(unlist(lapply(reactiveValuesToList(rv$tmp), function(x){x()}))) == 1) 
        #   && (length(which(config$isDone==T))==1) ){
        #   print("It is a global reset")
        #   return(NULL)
        # }
        # Compare the current config$isDone with rv$tmp to see which dataset
        # has changed. One compares the item which has changed between the two lists
        # if a dataset passed from NULL to a non-NULL value then is has been validated
        # else if the dataset passed from a non-NULL value to NULL, it has been reseted
        browser()
        print(GetModuleHasReturned())
        module_which_has_returned <- GetModuleHasReturned()
       
        #module_which_returned <- names(which(lapply(reactiveValuesToList(rv$tmp), 
        #                                            function(x){!is.null(x())}) == T))
        
        if (is.null(rv$tmp[[module_which_has_returned]]())){
          # The corresponding module has been reseted
          config$isDone[[module_which_has_returned]] <- FALSE
         #browser() 
          # renvoyer le dernier dataset non NULL avant la position courante
          name <- names(config$isDone)[ GetMaxValidated(config$isDone, rv$current.pos - 1)]
          ind.name <- grep(name, names(rv$dataOut))
          rv$dataOut <- rv$dataOut[,,-c((ind.name+1):length(rv$dataOut))]
          #rv$dataOut <- rv$dataOut[,,-c(rv$current.pos:length(rv$dataIn))]
          #rv$dataIn <- rv$dataIn[,,-c(rv$current.pos:length(rv$dataIn))]
        } else {
          # This means that the corresponding module has return a value
          # It has been validated
          config$isDone[[module_which_has_returned]] <- TRUE
          # Set to FALSE all further steps (in case of rerun a process
          # or if one points to a skipped process)
          ind <- which(names(config$isDone)==module_which_has_returned)
          # Check if the current position is on the last step
          if (ind < nbSteps())
            config$isDone[(1+ind):nbSteps()] <- FALSE
          #rv$dataIn <- rv$tmp[[module_which_returned]]()
          
          rv$dataOut <- rv$tmp[[module_which_has_returned]]()
        }
        
        rv$tmp.old <- rv$tmp
      })


      GetModuleHasReturned <- reactive({
        #browser()
        req(rv$tmp)
        name.current <- names(unlist(lapply(reactiveValuesToList(rv$tmp), function(x){x()})))
        name.old <- names(which(!is.na(rv$tmp.old)))
        
        c(name.current, name.old)
      })
      
      
      
      # Test if a process module (identified by its name) has been skipped.
      # This function is called each time the list config$isDone is updated
      # because one can know the status 'Skipped' only when a further module
      # has been validated
      isSkipped <- function(name){
        is.validated <- config$isDone[[name]]
        ind.name <- which(name == names(config$isDone))
        is.skipped <- !is.validated && ind.name < GetMaxValidated(config$isDone, nbSteps())
        if(verbose)
          print(paste0(config$process.name, " : is.skipped(", name, ") = ", is.skipped))
        return(is.skipped)
      }
      
     
      # This function calls the server part of each module composing the pipeline
      Launch_Module_Server <- function(){
        if(verbose)
          print(paste0(config$process.name, " : Launch_Module_Server"))
        
        BuildServer <- function(name){
          rv$tmp[[name]] <- do.call(as.character(paste0('mod_wf_wf1_', name, '_server')), 
                                      list(id = as.character(paste0("mod_",name, "_nav")),
                                           dataIn = reactive({SendCurrentDataset(name)}),
                                           remoteReset = reactive({rv$timeline$rstBtn()}),
                                           isSkipped = reactive({isSkipped(name)})))
        }
        
        lapply(1:nbSteps(), function(x){BuildServer(names(config$steps)[x])})
         }
      
      
      
      
      
      #####################################################################
      ## screens of the module
      ##
      ############### SCREEN 1 ######################################
      
      
      # This function creates the UI parts of the screens (dynamically set 
      # renderUIs). 
      BuildScreensUI <- function(){
        if(verbose)
          print(paste0(config$process.name, " : BuildScreensUI() "))
        
        
        # Accessory function
        FillScreen <- function(name){
        ll <- tagList(
                div(id=ns(name),
                    h3(paste0('Pipeline ', config$name)),
                    do.call(paste0('mod_wf_wf1_', name, '_ui'),
                            list(ns(paste0('mod_',name, '_nav'))))
                          )
                  )
        ll
        }
        
        #Creates the renderUI for the process modules. The first id is bypassed
        # because it is the description screen and is not linked to a process
        # module
        lapply(2:nbSteps(), 
               function(x){
                 name <- names(config$steps)[x]
                 output[[name]] <- renderUI(FillScreen(name))
        })
        
        # Creates the renderUI for the Description screen
        output[[names(config$steps)[1]]] <- renderUI({
          mod_insert_md_ui(ns(paste0(config$process.name, "_md")))
          })
        mod_insert_md_server(paste0(config$process.name, "_md"), 
                             paste0('./md/',config$process.name, '.md'))

      }
      
      
      ##########################################################
      
      reactive({rv$dataOut})
    }
  )
}

