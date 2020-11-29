

mod_wf_wf1_Original_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    hr(),
    wellPanel(
      h3('Module Original'),
      fluidRow(
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Input")),
               uiOutput(ns('show_dataIn'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Output")),
               uiOutput(ns('show_rv_dataOut'))),
        column(width=4,
               tags$b(h4(style = 'color: blue;', "status")),
               uiOutput(ns('show_status')))
      )
    )
  )
}

#' @param dataIn xxx
#'
#' 
#' 
mod_wf_wf1_Original_server <- function(id, 
                                        dataIn=NULL,
                                        dataOut = NULL,
                                        remoteReset=FALSE,
                                        isSkipped = FALSE){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      verbose = T
      source(file.path('.', 'debug_ui.R'), local=TRUE)$value
      source(file.path('.', 'code_general.R'), local=TRUE)$value
      source(file.path('.', 'private_methods.R'), local=TRUE)$value
      
      #################################################################################
      config <- reactiveValues(
        type = 'process',
        process.name = 'Original',
        steps = list(Description = T)
      )
      
      #################################################################################
      
      rv <- reactiveValues(
        current.pos = 1,
        timeline = NULL,
        dataIn = NULL
        )
      

      # Main listener of the module which initialize it
      
     
      
      
      observeEvent(req(dataIn()), ignoreNULL=T, ignoreInit = F, { 
        if(verbose)
          print(paste0(config$process.name, " :  reception d'un nouveau dataIn() : ", paste0(names(dataIn()), collapse=' ')))
        
        #browser()
        inputExists <- length(dataIn()) > 0
        tmpExists <- !is.null(rv$dataIn)
        rv$wake <- FALSE
        
        if (tmpExists){
          # this case is either the module is skipped or validated
          if(verbose)
            print(paste0(config$process.name, ' : Just repositioning cursor'))
          rv$current.pos <- nbSteps()
          rv$wake <- Wake()
        } else {
          if (inputExists){
            if(verbose)
              print(paste0(config$process.name, ' : InitializeModule()'))
            # The current position is pointed on a new module
            InitializeModule()
            InitializeTimeline()
          } else if (!inputExists){
            # Initialization of Prostar
          }
        }
        
      })
      
      
      observeEvent(req(isSkipped()), {
        if(verbose)
          print(paste0(config$process.name, ' : New value for isSkipped() : ', isSkipped()))
        
        if (isSkipped())
          Initialize_Status_Process()
      })
      

      InitializeModule <- function(){
        if(verbose)
          print(paste0(config$process.name, ' : InitializeModule() ------- '))
        #browser()
        Initialize_Status_Process()
        rv$screens <- InitScreens(nbSteps())
        # Must be placed after the initialisation of the 'config$stepsNames' variable
        config$screens <- CreateScreens(names(config$steps))
        InitializeDataIn()
        rv$current.pos <- 1
      }
        
      
      InitializeDataIn <- function(){rv$dataIn <- dataIn()}

        InitializeTimeline <- function(){
          rv$timeline <- mod_timeline_server("timeline",
                                             style = 2,
                                             config = config,
                                             onlyReset = TRUE,
                                             wake = reactive({rv$wake})
                                             )
          #Catch a new position from timeline
          observeEvent(req(rv$timeline$pos()), ignoreInit=T, { 
            if(verbose)
              print(paste0(config$process.name, ' : observeEvent(req(rv$timeline$pos()) ------- ',  rv$timeline$pos() ))
            rv$current.pos <- rv$timeline$pos() 
          })
          
          
          
          # Remember that, at a given position, the process module alsways receive the dataset
          # it has to analyze (ie dataIn() is always instantiated)
          # When the user reset a module, the ojective is to put the module in the same state as
          # if it is the first time he put the focus on
          # Thus, the only thing to do is to reset the interface. and to put dataIn() in rv$dataIn
          # DO not forget to reset config$status
          observeEvent(req(c(rv$timeline$rstBtn(), remoteReset())),{
            if(verbose)
              print(paste0(config$process.name, ' : ------reset activated------, rv$timeline$rstBtn()=',rv$timeline$rstBtn()))
            
            ResetScreens()
            rv$dataIn <- NA
            rv$current.pos <- 1
            rv$wake <- Wake()
            Initialize_Status_Process()
            Send_Result_to_Caller()
            InitializeDataIn()
            #browser()
          })
          
        }
      

        
        
        # This function cannot be implemented in the timeline module because 
        # the id of the screens to reset are not known elsewhere.
        # Trying to reset the global 'div_screens' in the timeline module
        # does not work
        ResetScreens <- function(screens){
          lapply(1:nbSteps(), function(x){
            shinyjs::reset(names(config$steps)[x])
          })
        }
        
        
        ValidateCurrentPos <- reactive({
          if(verbose)
            print(paste0(config$process.name, ' : ValidateCurrentPos() = '))
          
          config$status[rv$current.pos] <- VALIDATED
          Set_Skipped_Status()
          if (config$status[nbSteps()] == VALIDATED)
            # Either the process has been validated, one can prepare data to ben sent to caller
            # Or the module has been reseted
            Send_Result_to_Caller()
        })
        
        
        
        Send_Result_to_Caller <- reactive({
        if(verbose)
          print(paste0(config$process.name, ' : Execution of Send_Result_to_Caller() : '))

        dataOut$obj <- rv$dataIn
        dataOut$name <- config$process.name
        dataOut$trigger <- Wake()
        
        if(verbose)
          print(paste0(config$process.name, ' : dataOut$obj =  : ', paste0(names(dataOut$obj), collapse=' ')))
      })
      

      
      #####################################################################
      ## screens of the module
      ##
      ############### SCREEN 1 ######################################
      output$Description <- renderUI({
        tagList(
          actionButton(ns('validate_btn'), 'Start pipeline',class=btn_success_color),
          mod_insert_md_ui(ns(paste0(config$process.name, "_md")))
        )
      })
       mod_insert_md_server(paste0(config$process.name, "_md"), 
                            paste0('./md/',config$process.name, '.md'))


      observeEvent(input$validate_btn, {
        if(verbose)
          print(paste0(config$process.name, ' : Clic on validate_btn, pos = ', rv$current.pos))
        
        ValidateCurrentPos()
      })

      
    })
  
}

