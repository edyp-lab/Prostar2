
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

      
     
      

      

    }
  )
}

