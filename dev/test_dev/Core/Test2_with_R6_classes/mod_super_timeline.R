

mod_super_timeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    hr(),
    uiOutput(ns('show_screens')),
    hr(),
    h3('Module pipeline'),
    wellPanel(
      fluidRow(
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Input")),
               uiOutput(ns('show_dataIn'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Current dataset")),
               uiOutput(ns('show_rv_dataIn'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Output")),
               uiOutput(ns('show_rv_dataOut'))),
        column(width=4,
               tags$b(h4(style = 'color: blue;', "Status")),
               uiOutput(ns('show_status')))
        # column(width=2,
        #        tags$b(h4(style = 'color: blue;', "Current pos")),
        #        uiOutput(ns('show_currentPos')))
      )
    )
  )
}

#' @param dataIn xxx
#'
#' @param dataOut xxx
#' 
#' @param config
#' 
mod_super_timeline_server <- function(id, 
                                      dataIn=NULL,
                                      dataOut = NULL,
                                      config){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      verbose <- T
      source(file.path('.', 'debug_ui.R'), local=TRUE)$value
      source(file.path('.', 'code_general.R'), local=TRUE)$value
      source(file.path('.', 'private_methods.R'), local=TRUE)$value
      
      rv <- reactiveValues(
        # Current position of the cursor in the timeline
        current.pos = 1,
        
        # Use to pass the reset info to the process modules
        remoteReset = NULL,
        
        #dataIn = NULL,
        dataOut = NULL,
        
        data2send = NULL
      )
      
      
      
      return_of_process <- reactiveValues(
        name = NULL,
        trigger = NULL,
        obj = NULL
      )
      
      #--------------------------------------------------------------
     
      observeEvent(req(dataIn()), ignoreNULL=T, ignoreInit = F, { 
        if(verbose)
          print(paste0(config$process.name, " :  reception d'un nouveau dataIn() : ", paste0(names(dataIn()), collapse=' ')))
        
        inputExists <- length(dataIn()) > 0
        tmpExists <- !is.null(rv$dataIn)
        rv$wake <- FALSE
        
        
        if (tmpExists){
          # this case is either the module is skipped or validated
          if(verbose)
            print(paste0(config$process.name, ' : Just repositioning cursor'))
          rv$current.pos <- nbSteps()
        }
        else{
          if (inputExists){
          # The current position is pointed on a new module
          if(verbose)
            print(paste0(config$process.name, ' : InitializeModule()'))
          
          InitializeModule()
          rv$data2send <- setNames(lapply(1:nbSteps(), 
                                          function(x){NA}), 
                                   names(config$steps))
          Launch_Module_Server()
          PrepareData2Send()
        }
        else if (!inputExists){
          # Initialization of Prostar
        }
        }
        
        if(verbose)
          print(paste0(config$process.name, " :  END OF reception d'un nouveau dataIn() : ", paste0(names(rv$dataIn), collapse=' ')))

      })
      
      # Initialization of the process
      InitializeModule <- function(){
        if(verbose)
          print(paste0(config$process.name, ' :  Initialisation du module ------'))
        

        Initialize_Status_Process()
        InitializeDataIn()
        rv$screens <- InitScreens(nbSteps())
        # Must be placed after the initialisation of the 'config$stepsNames' variable
        config$screens <- CreateScreens(names(config$steps))
 
        rv$timeline <- mod_timeline_server("timeline", 
                                   style = 2, 
                                   config = config,
                                   wake = reactive({rv$wake}))
        BuildScreensUI()
        rv$wake <- Wake()
        } # END OF observeEvent(dataIn())
      
      
      InitializeDataIn <- function(){rv$dataIn <- dataIn()}
      
      
      
      
      #Catch a reset command from timeline
      observeEvent(req(rv$timeline$rstBtn()), {
        if(verbose)
          print(paste0(config$process.name, " : reset activated"))
        
        
        ResetScreens()
        rv$dataIn <- NA
        rv$current.pos <- 1
        rv$wake <- Wake()
        Initialize_Status_Process()
        Send_Result_to_Caller()
        InitializeDataIn()
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
      observeEvent(req(return_of_process$trigger), ignoreNULL = T, ignoreInit=F, { 
        if(verbose){
          print(paste0(config$process.name, " : reception d'un retour sur rv$tmp"))
          print(paste0(config$process.name, " : return_of_process$trigger = ", return_of_process$trigger))
          print(paste0(config$process.name, " : return_of_process$name = ", return_of_process$name))
          print(paste0(config$process.name, " : return_of_process$obj = ", paste0(names(return_of_process$obj), collapse=' ')))
        }
        
        #browser()
        valid_obj_val <-  class(return_of_process$obj) == 'QFeatures'
        # Update the status
        config$status[return_of_process$name] <- valid_obj_val
        Set_Skipped_Status()
        
        # Store the result of a process module
        if (valid_obj_val)
          rv$dataIn <- return_of_process$obj
        else
          rv$dataIn <- rv$dataIn[,,1:GetMaxValidated_BeforeCurrentPos()]
        
        
        Send_Result_to_Caller()
      })
      
      
      ## -----------------------------------------------------------------------
      ## Functions to manage the config$status vector
      ##
      ## _______________________________________________________________________
      
      
      
      
      
      
      
      #To avoid "intempestive" initializations of modules due to dataIn changes
      # one define the following logics :
      #  A dataset is loaded in a module only if this module is not yet
      # validated and if it has not been skipped (this is why we use the
      # max True function
      # To rerun a validated module, the user has to reset it
      # This function returns a non-NULL value to the module which name
      # corresponds to the current position and one send always the last
      # non-NULL dataset before current position
      PrepareData2Send <- reactive({
        if(verbose)
          print(paste0(config$process.name, " : Run PrepareData2Send()"))

        #browser()
        
        # Returns NULL to all modules except the one pointed by the current position
        # Initialization of the pipeline : one send dataIn() to the
        # original module
        update <- function(name){
          data <- NA
          if (name == GetCurrentStepName()){
            # One treat the dataset for the current position
            ind.last.validated <- GetMaxValidated_BeforeCurrentPos()
            if (is.null(ind.last.validated)){
              data <- dataIn()
            } else {
              data <- rv$dataIn[,,c(1:ind.last.validated)]
            }
          }

          if(verbose){
            print(paste0(config$process.name, ' : SendCurrentDataset() to ', name, ' => ', paste0(names(data), collapse=' ')))
          }
          return(data)
        }
        
        lapply(names(rv$data2send), function(x){rv$data2send[[x]] <- update(x)})
        return_of_process$obj <- NA
        #browser()
      })
      
      
      
      #Catch a new position from timeline
      observeEvent(req(rv$timeline$pos()), ignoreNULL=T, ignoreInit = T,{
        if(verbose)
          print(paste0(config$process.name, ' : observeEvent(req(rv$timeline$pos(). New position = ', rv$timeline$pos()))
        rv$current.pos <- rv$timeline$pos()
        #browser()
        PrepareData2Send()
      })
      

      
      Send_Result_to_Caller <- reactive({
        if(verbose)
          print(paste0(config$process.name, ' : Execution of Send_Result_to_Caller() : '))
       # browser()
        dataOut$obj <- rv$dataIn
        dataOut$name <- config$process.name
        dataOut$trigger <- runif(1,0,1)
        if(verbose)
          print(paste0(config$process.name, ' : dataOut$obj =  : ', paste0(names(dataOut$obj), collapse=' ')))
        
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

     

     
      # This function calls the server part of each module composing the pipeline
      Launch_Module_Server <- function(){
        if(verbose)
          print(paste0(config$process.name, " : Launch_Module_Server : "))
        
       lapply(names(config$steps), function(x){
          if(verbose)
            print(paste0(config$process.name, " : Launch_Module_Server : ",x))
          
          do.call(as.character(paste0('mod_wf_wf1_', x, '_server')), 
                  list(id = as.character(paste0("mod_",x, "_nav")),
                       dataIn = reactive({rv$data2send[[x]]}),
                       dataOut = return_of_process,
                       remoteReset = reactive({rv$timeline$rstBtn()}),
                       isSkipped = reactive({is.skipped(x)})
                  )
          )
        })
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
        
        
        #Creates the renderUI for the process modules. The first id is bypassed
        # because it is the description screen and is not linked to a process
        # module
        lapply(names(config$steps), 
               function(x){
                 output[[x]] <- renderUI(tagList(
                   div(id=ns(x),
                       h3(paste0('Pipeline ', config$name)),
                       do.call(paste0('mod_wf_wf1_', x, '_ui'),
                               list(ns(paste0('mod_',x, '_nav'))))
                   )
                 ))
        })

      }

    }
  )
}

