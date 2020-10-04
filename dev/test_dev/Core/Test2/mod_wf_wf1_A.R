

mod_wf_wf1_A_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    hr(),
    wellPanel(
      h3('Module A'),
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
mod_wf_wf1_A_server <- function(id, 
                                dataIn=NULL,
                                remoteReset=FALSE,
                                forcePosition = 1){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      source(file.path('.', 'debug_ui.R'), local=TRUE)$value
      
      

  
      #################################################################################
      config <- reactiveValues(
        type = 'process',
        name = 'Filtering',
        stepsNames = c("Description", "Step 1", "Step 2", "Step 3"),
        mandatory =  c(FALSE, TRUE, FALSE, TRUE)
      )
      
      
      rv <- reactiveValues(
        current.pos = 1,
        timeline = NULL,
        dataIn = NULL,
        dataOut = NULL,
        event_counter = 0,
        cmd = NULL)

      

      
      Init_isDone <- function(){
        setNames(lapply(1:nbSteps(), 
                        function(x){ x == 1}), 
                 config$stepsNames)
      }
      #################################################################################
      
      # Main listener of the module which initialize it
      observeEvent(req(dataIn() ), ignoreNULL=T,{ 
        print(' ------- MODULE A : Initialisation de rv$dataIn ------- ')
        browser()
        if (is.null(rv$dataIn))
          InitializeModule()
      })
      
      
      
      
      
      InitializeModule <- function(){
        print(' ------- MODULE A : InitializeModule() ------- ')
        rv$dataIn <- dataIn()
        rv$dataOut <- NULL
        
        
        rv$event_counter <- 0
        rv$screens <- InitActions(nbSteps())
        config$screens <- CreateScreens(nbSteps())
        config$isDone <- Init_isDone()
        
        rv$timeline <- mod_timeline_server("timeline", 
                                           style = 2, 
                                           config = config, 
                                           cmd = reactive({rv$cmd}),
                                           position = reactive({rv$current.pos})
        )
        
        
        # This listener appears only in modules that are called by another one.
        # It allows the caller to force a new position
        observeEvent(forcePosition(),{
          print(' ------- MODULE A : observeEvent(forcePosition()) ------- ')
          rv$current.pos <- forcePosition() })
        
        
        
        #Catch a new position from timeline
        observeEvent(req(rv$timeline$pos()),{ 
          print(' ------- MODULE A : observeEvent(req(rv$timeline$pos()) ------- ')
          rv$current.pos <- rv$timeline$pos() })
        
        
        # Catches an clic on the next or previous button in the timeline
        # and updates the event_counter
        observeEvent(req(c(rv$timeline$nxtBtn()!=0, rv$timeline$prvBtn()!=0)),{
          print(' ------- MODULE A : observeEvent(req(c(rv$timeline$nxtBtn()!=0, rv$timeline$prvBtn()!=0))) ------- ')
          
          # Add external events to counter
          rv$event_counter <- rv$event_counter + rv$timeline$rstBtn() + remoteReset()
        })
        
        
        #--- Catch a reset from timeline or caller
        observeEvent(req(c(rv$timeline$rstBtn()!=0, remoteReset()!=0)), {
          print("---- MODULE A : reset activated ----------------")
          print(' ------- MODULE A : observeEvent(req(c(rv$timeline$rstBtn()!=0, remoteReset()!=0)) ------- ')
          
          # Add external events to counter
          rv$event_counter <- rv$event_counter + rv$timeline$rstBtn() + remoteReset()
          
          rv$cmd <- SendCmdToTimeline(c('EnableAllSteps', 'ResetActionBtns'))
          config$isDone <- Init_isDone()
          
          rv$current.pos <- 1
          rv$event_counter <- 0
          ResetScreens()
          
          Reset_Module_Data_logics()
          # Update datasets logics
        })
        
        
        # Catch a change in isDone (validation of a step)
        # Specific to the modules of process and do not appear in pipeline module
        observeEvent(config$isDone,  ignoreInit = T, {
          print(' ------- MODULE A : A new step is validated ------- ')
          print(' ------- MODULE A : observeEvent(config$isDone,  ignoreInit = T) ------- ')
          
          rv$cmd <- SendCmdToTimeline('DisableAllPrevSteps')
        })
        
        # This listener catches the changes in the local input but not
        # those which come from caller or called modules
        # It is not necessary in the pipeline module because the toggle state
        # of process ui are managed by the process module itself.
        observe({
          reactiveValuesToList(input)
          rv$event_counter <- sum(as.numeric(unlist(reactiveValuesToList(input))), na.rm=T)
          #print(paste0('----MODULE A : new event detected on reactiveValuesToList(input) : ', rv$event_counter))
        })
        
      }
      
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
      
      
     
      
      #Catch 
      # observeEvent(forcePosition(),{
      #   print(paste0('---- MODULE A : new value for forcePosition() : ', forcePosition()))
      # 
      #   if (length(which(unlist(config$isDone)==T))>0){
      #     rv$maxTRUE <- max(which(unlist(config$isDone)==T))
      #   } else {
      #     rv$maxTRUE <- 1
      #   }
      #   
      #   rv$current.pos <- rv$maxTRUE
      # })
      
      
      SendCmdToTimeline <- function(names){
        append(as.list(names), list(rv$event_counter))
        #paste0(name, '_', rv$event_counter)
      }
      
      
     
      
      Reset_Module_Data_logics <- function(){
        # Update datasets logics
        rv$dataIn <- RemoveItemFromDataset(dataIn(), config$name)
        rv$dataOut <- rv$dataIn
      }
      
     
      
       #####################################################################
       ## screens of the module
       ##
       ############### SCREEN 1 ######################################
       output$screen1 <- renderUI({
         tagList(
           tags$h3(paste0('Process ', config$name))
         )
       })

       ############### SCREEN 2 ######################################
       
       output$screen2 <- renderUI({
         
         observeEvent(input$perform_screen2_btn, {
           config$isDone[[2]] <- TRUE
         })
         
         tagList(
           div(id=ns('screen2'),
               div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
               tags$h2('Step 1')),
               div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                   selectInput(ns('select1'), 'Select step 1', 
                           choices = 1:5, 
                           selected = 1,
                           width = '150px')
               ),
               div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                   actionButton(ns('perform_screen2_btn'), 'Perform'))
           )
         )
       })
       
       
       
       
       ############### SCREEN 3 ######################################
       output$screen3 <- renderUI({
         
         tagList(
           div(id=ns('screen3'),
               div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                   tags$h3('Step 2')),
               div(style="display:inline-block; vertical-align: middle;padding-right: 40px;",
                   selectInput(ns('select2'), 'Select step 2',
                           choices = 1:5,
                           selected = 1,
                           width = '150px')),
               div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                   actionButton(ns('perform_screen3_btn'), 'Perform'))
           )
         )
       })
       
       ## Logics to implement: here, we must take the last data not null
       # in previous datas. The objective is to take account
       # of skipped steps
       observeEvent(input$perform_screen3_btn, {
         config$isDone[[3]] <- TRUE
       })
       
       
       ############### SCREEN 4 ######################################
       output$screen4 <- renderUI({
         
         tagList(
           div(id=ns('screen4'),
               div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                   tags$h3('Step 4')),
               div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                   actionButton(ns('validate_btn'), 'Validate'))
           )
         )
       })
         
          observeEvent(input$validate_btn, {
            #isolate({
            #browser()
            Validate_Module_Data_logics()
              config$isDone[[4]] <- TRUE
           # })
       })
       
          Validate_Module_Data_logics <- function(){
            rv$dataIn <- AddItemToDataset(rv$dataIn, config$name)
            rv$dataOut <- rv$dataIn
          }
         
       
          
          
       ##########################################################
        
  reactive({rv$dataOut})
    })
  
}

