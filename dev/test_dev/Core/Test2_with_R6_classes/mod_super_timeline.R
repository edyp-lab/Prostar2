
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

        inputExists <- length(dataIn()) > 0
        tmpExists <- !is.null(rv$dataIn)
        rv$wake <- FALSE
        
        if (tmpExists){
          # this case is either the module is skipped or validated
          rv$current.pos <- nbSteps()
        }
        else{
          if (inputExists){
          # The current position is pointed on a new module
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

      })
      
      # Initialization of the process
      InitializeModule <- function(){
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

      
      # This function calls the server part of each module composing the pipeline
      Launch_Module_Server <- function(){
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
      

      # This function creates the UI parts of the screens (dynamically set 
      # renderUIs). 
      BuildScreensUI <- function(){
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

