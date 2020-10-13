

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
               tags$b(h4(style = 'color: blue;', "Data input")),
               uiOutput(ns('show_dataIn'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Data output")),
               uiOutput(ns('show_rv_dataOut'))),
        column(width=2,
               tags$b(h4(style = 'color: blue;', "Current pos")),
               uiOutput(ns('show_currentPos'))),
        column(width=4,
               tags$b(h4(style = 'color: blue;', "List 'status'")),
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
        dataIn = NULL,
        dataOut = NULL,
        old.rst = 0)
      

      # Main listener of the module which initialize it
      
      
      observeEvent(isSkipped(), {
        if(verbose)
          print(paste0(config$process.name, ' : New value for isSkipped() : ', isSkipped()))
        #browser()
        rv$skipped <- isSkipped()
        if (isSkipped())
          config$status <- setNames(lapply(1:nbSteps(), 
                                           function(x){SKIPPED}), 
                                    names(config$steps))
      })
      
      
      
      observeEvent(req(dataIn()), ignoreNULL=T, ignoreInit = F, { 
        if(verbose)
          print(paste0(config$process.name, " :  reception d'un nouveau dataIn() : ", paste0(names(dataIn()), collapse=' ')))
        
        #browser()
        BuildStatus()
        inputExists <- length(dataIn()) > 0
        tmpExists <- !is.null(rv$dataIn)
        
        
        if (inputExists && tmpExists){
          # this case is either the module is skipped or validated
          #rv$current.pos <- nbSteps()
          rv$wake <- runif(1,0,1)
        }
        else if (inputExists && !tmpExists){
          # The current position is pointed on a new module
          InitializeModule()
          InitializeTimeline()
          if(verbose)
            print(paste0(config$process.name, ' : InitializeModule()'))
        }
        else if (!inputExists && tmpExists){
          #The current position points to a validated module
          rv$current.pos <- nbSteps()
          if(verbose)
            print(paste0(config$process.name, ' : Just repositioning cursor'))
        }
        else if (!inputExists && !tmpExists){
          # Initialization of Prostar
        }
        
        if(verbose)
          print(paste0(config$process.name, " :  END OF reception d'un nouveau dataIn() : ", paste0(names(rv$dataIn), collapse=' ')))
        
      })
      
      
      
      
      
      
      GetMaxValidated_AllSteps <- reactive({
        last.name <- names(rv$dataOut)[length(rv$dataOut)]
        ind <- which(last.name == names(config$steps))
        if (length(ind) == 0)
          ind <- NULL
        ind
      })
      
      GetMaxValidated_BeforeCurrentPos <- reactive({
        current.name <- GetCurrentStepName()
        ind.current <- which(current.name == names(config$steps))
        indices.validated <- match(names(rv$dataIn),names(config$steps))
        ind.max <- max(which(indices.validated < ind.current))
        ind.max
      })
      
      GetCurrentStepName <- reactive({ names(config$steps)[rv$current.pos] })
      
      
      
      GetStatusPosition <- function(pos){
        status <- NULL
        #browser()
        if (length(grep(names(config$steps)[[pos]], names(rv$dataOut)))== 1) 
          status <- VALIDATED
        else {
          if (is.null(GetMaxValidated_AllSteps()) ||  GetMaxValidated_AllSteps() < pos)
            status <- UNDONE
          else if (GetMaxValidated_AllSteps() > pos)
            status <- SKIPPED
        }
        status
      }
      
      
      BuildStatus <- reactive({
        rv$dataOut
        config$status <- setNames(lapply(1:nbSteps(), 
                                         function(x){GetStatusPosition(x)}), 
                                  names(config$steps))
      })
      
      
      UpdateDataOut <- reactive({
        if(verbose)
          print(paste0(config$process.name, ' : Execution of UpdateDataOut() : '))
        
        dataOut$obj <- rv$dataOut
        dataOut$name <- config$process.name
        dataOut$trigger <- runif(1,0,1)
        if(verbose)
          print(paste0(config$process.name, ' : dataOut$obj =  : ', paste0(names(dataOut$obj), collapse=' ')))
        
      })
      
 
      InitializeTimeline <- function(){
        rv$timeline <- mod_timeline_server("timeline",
                                           style = 2,
                                           config = config,
                                           onlyReset = TRUE,
                                           wake = reactive({rv$wake})
                                           )
      }
      
      
      InitializeModule <- function(){
        if(verbose)
          print(paste0(config$process.name, ' : InitializeModule() ------- '))
        rv$current.pos <- 1
        rv$dataIn <- dataIn()
        rv$dataOut <- NULL
        CommonInitializeFunctions()
        
        BuildStatus()

        #Catch a new position from timeline
        observeEvent(req(rv$timeline$pos()), ignoreInit=T, { 
          if(verbose)
            print(paste0(config$process.name, ' : observeEvent(req(rv$timeline$pos()) ------- ',  rv$timeline$pos() ))
          rv$current.pos <- rv$timeline$pos() 
          if(verbose)
            print(paste0(config$process.name, ' : observeEvent(req(rv$timeline$pos()) ------- ', paste0(config$status, collapse=' ') ))
          
        })
        
     }
      ############ ---   END OF REACTIVE PART OF THE SERVER   --- ###########
    
      
      # This function cannot be implemented in the timeline module because 
      # the id of the screens to reset are not known elsewhere.
      # Trying to reset the global 'div_screens' in the timeline module
      # does not work
      ResetScreens <- function(screens){
        lapply(1:nbSteps(), function(x){
          shinyjs::reset(names(config$steps)[x])
        })
      }
      
      
      #--- Catch a reset from timeline or caller
      observeEvent(req(c(rv$timeline$rstBtn(), remoteReset())),{
        if(verbose){
          print(paste0(config$process.name, ' : ------reset activated------, rv$timeline$rstBtn()=',rv$timeline$rstBtn()))
          #print(' ------- MODULE _A_ : observeEvent(req(c(rv$timeline$rstBtn()!=0, remoteReset()!=0)) ------- ')
        }
        
        ResetScreens()
        InitializeModule()
        rv$old.rst <- rv$timeline$rstBtn()
        BuildStatus()
      })
      
      #####################################################################
      ## screens of the module
      ##
      ############### SCREEN 1 ######################################
      output$Description <- renderUI({
        tagList(
          actionButton(ns('validate_btn'), 'Start'),
          mod_insert_md_ui(ns(paste0(config$process.name, "_md")))
        )
      })
       mod_insert_md_server(paste0(config$process.name, "_md"), 
                            paste0('./md/',config$process.name, '.md'))


      observeEvent(input$validate_btn, {
        if(verbose)
          print(paste0(config$process.name, ' : Clic on validate_btn'))
        #browser()
        rv$dataOut <- rv$dataIn
        UpdateDataOut()
        config$status[[rv$current.pos]] <- VALIDATED
      })

      
    })
  
}

