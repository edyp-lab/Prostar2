

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
               tags$b(h4(style = 'color: blue;', "Data input")),
               uiOutput(ns('show_rv_dataIn'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Data output")),
               uiOutput(ns('show_rv_dataOut')))
        # column(width=2,
        #        tags$b(h4(style = 'color: blue;', "Current pos")),
        #        uiOutput(ns('show_currentPos')))
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
                                      dataOut = NULL,
                                      config){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      verbose <- T
      
      rv <- reactiveValues(
        # Current position of the cursor in the timeline
        current.pos = 1,
        
        # Use to pass the reset info to the process modules
        remoteReset = NULL,
        
        #dataIn = NULL,
        dataOut = NULL,
        
        data2send = NULL,
        
        old.rst = 0
      )
      
      source(file.path('.', 'debug_ui.R'), local=TRUE)$value
      source(file.path('.', 'code_general.R'), local=TRUE)$value
      
      
      modules_dataOut <- reactiveValues(
        name = NULL,
        trigger = NULL,
        obj = NULL
      )
      
      #--------------------------------------------------------------
     
      observeEvent(req(dataIn()), ignoreNULL=T, ignoreInit = F, { 
        if(verbose)
          print(paste0(config$process.name, " :  reception d'un nouveau dataIn() : ", paste0(names(dataIn()), collapse=' ')))
        
        ##browser()
        BuildStatus()
        inputExists <- length(dataIn()) > 0
        tmpExists <- !is.null(rv$dataIn)
        
        
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
        
        rv$old.rst <- 0
        rv$dataIn <- dataIn()
        BuildStatus()
        rv$screens <- InitActions(nbSteps())
        # Must be placed after the initialisation of the 'config$stepsNames' variable
        config$screens <- CreateScreens(names(config$steps))
        
        
        rv$timeline <- mod_timeline_server("timeline", 
                                   style = 2, 
                                   config = config,
                                   showSaveBtn = TRUE,
                                   wake = reactive({F}))
        ##browser()
        BuildScreensUI()
        
        #PrepareData2Send()
        
        
        } # END OF observeEvent(dataIn())
      
      
      
      
      #Catch a reset command from timeline
      observeEvent(req(rv$timeline$rstBtn() != rv$old.rst), {
        if(verbose)
          print(paste0(config$process.name, " : reset activated"))
        
        ResetScreens()
        InitializeModule()
        rv$old.rst <- rv$timeline$rstBtn()
        BuildStatus()
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
      observeEvent(req(modules_dataOut$trigger), ignoreNULL = T, ignoreInit=F, { 
        if(verbose){
          print(paste0(config$process.name, " : reception d'un retour sur rv$tmp"))
          print(paste0(config$process.name, " : modules_dataOut$trigger = ", modules_dataOut$trigger))
          print(paste0(config$process.name, " : modules_dataOut$name = ", modules_dataOut$name))
          print(paste0(config$process.name, " : modules_dataOut$obj = ", paste0(names(modules_dataOut$obj), collapse=' ')))
        }
        
        #browser()
        
        Update_dataIn()
       # PrepareData2Send()
        BuildStatus()
      })
      
      
      
      
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
      

      observeEvent(req(rv$timeline$saveBtn()),{
        if(verbose)
          print(paste0(config$process.name, " : Clic on the 'Save & Exit' button", rv$timeline$saveBtn()))
        rv$current.pos <- rv$timeline$pos()
        
        UpdateDataOut()
      })
      
      UpdateDataOut <- reactive({
        if(verbose)
          print(paste0(config$process.name, ' : Execution of UpdateDataOut() : '))
        
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

      
      ## IMPORTANT FUNCTION !!!
     Update_dataIn <- reactive({
       if(verbose)
         print(paste0(config$process.name, " : Update_dataIn()"))
       ##browser() 
       rv$dataIn <- modules_dataOut$obj

    ##browser()
     })
      
      
      
      
      
      GetMaxValidated_AllSteps <- reactive({
        last.name <- names(rv$dataIn)[length(rv$dataIn)]
        ind <- which(last.name == names(config$steps))
        if (length(ind) == 0)
          ind <- NULL
        ind
      })
      
      GetMaxValidated_BeforeCurrentPos <- reactive({
        ind.max <- NULL
        current.name <- GetCurrentStepName()
        ind.current <- which(current.name == names(config$steps))
        indices.validated <- match(names(rv$dataIn),names(config$steps))
        if (length(indices.validated) > 0){
          ind <- which(indices.validated < ind.current)
          if(length(ind) > 0)
            ind.max <- max(ind)
        }
        ind.max
      })
      
      GetCurrentStepName <- reactive({ names(config$steps)[rv$current.pos] })
      
      Unskip <- function(pos){
        config$status[[pos]] <- UNDONE
      }
      
      GetStatusPosition <- function(pos){
        status <- NULL
        ##browser()
        if (length(grep(names(config$steps)[[pos]], names(rv$dataIn)))== 1) 
          status <- VALIDATED
        else {
          if (is.null(GetMaxValidated_AllSteps()) ||  GetMaxValidated_AllSteps() < pos)
            status <- UNDONE
          else if (GetMaxValidated_AllSteps() > pos)
            status <- SKIPPED
        }
        status
      }
      
      
      
      
### End of part for managing the timeline
      
    BuildStatus <- reactive({
      if(verbose)
        print(paste0(config$process.name, " : BuildStatus()"))
      
      config$status <- setNames(lapply(1:nbSteps(), 
                      function(x){GetStatusPosition(x)}), 
               names(config$steps))
    })

      
      # Test if a process module (identified by its name) has been skipped.
      # This function is called each time the list config$isDone is updated
      # because one can know the status 'Skipped' only when a further module
      # has been validated
      isSkipped <- function(name){
       # #browser()
        pos <- which(name == names(config$steps))
        return(GetStatusPosition(pos) == SKIPPED)
      }

     
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
                       dataOut = modules_dataOut,
                       remoteReset = reactive({rv$timeline$rstBtn()}),
                       isSkipped = reactive({isSkipped(x)})
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
      
      
      ##########################################################
      
      reactive({rv$dataOut})
    }
  )
}

