

mod_wf_wf1_Normalization_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    hr(),
    wellPanel(
      h3('Module _B_'),
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
mod_wf_wf1_Normalization_server <- function(id, 
                                        dataIn=NULL,
                                        remoteReset=FALSE,
                                        forcePosition = 1){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      source(file.path('.', 'debug_ui.R'), local=TRUE)$value
      source(file.path('.', 'code_general.R'), local=TRUE)$value
      
      
      
      #################################################################################
      config <- reactiveValues(
        type = 'process',
        process.name = 'Normalization',
        steps = setNames(lapply(c(T,T,F,T), 
                                function(x){x}),
                         c("Description", "Step1", "Step2", "Step3"))
      )
      
      
      rv <- reactiveValues(
        current.pos = 1,
        timeline = NULL,
        dataIn = NULL,
        dataOut = NULL,
        event_counter = 0,
        cmd = NULL)
      
      #################################################################################
      
      # Main listener of the module which initialize it
      observeEvent(req(dataIn() ), ignoreNULL=T,{ 
        #print(' ------- MODULE _B_ : Initialisation de rv$dataIn ------- ')
        
        if (is.null(rv$dataIn))
        {
          #print(' ------- MODULE _B_ : Entering for the first time ------')
          InitializeModule()
        }
        if (config$isDone[[nbSteps()]])
          rv$current.pos <- nbSteps()
        
      })
      
      
      
      InitializeModule <- function(){
        #print(' ------- MODULE _B_ : InitializeModule() ------- ')
        rv$dataIn <- dataIn()
        rv$dataOut <- NULL
        
        CommonInitializeFunctions()
        
        rv$timeline <- mod_timeline_server("timeline", 
                                           style = 2, 
                                           config = config, 
                                           cmd = reactive({rv$cmd}),
                                           position = reactive({rv$current.pos})
        )
        
        
        # This listener appears only in modules that are called by another one.
        # It allows the caller to force a new position
        # observeEvent(forcePosition(),{
        #   print(' ------- MODULE _B_ : observeEvent(forcePosition()) ------- ')
        #   print(paste0('force position to : ', forcePosition()))
        #   rv$current.pos <- forcePosition() })
        
        
        
        #Catch a new position from timeline
        observeEvent(req(rv$timeline$pos()),{ 
          #print(' ------- MODULE _B_ : observeEvent(req(rv$timeline$pos()) ------- ')
          rv$current.pos <- rv$timeline$pos() })
        
        
        # Catches an clic on the next or previous button in the timeline
        # and updates the event_counter
        observeEvent(req(c(rv$timeline$nxtBtn()!=0, rv$timeline$prvBtn()!=0)),{
          #print(' ------- MODULE _B_ : observeEvent(req(c(rv$timeline$nxtBtn()!=0, rv$timeline$prvBtn()!=0))) ------- ')
          
          # Add external events to counter
          rv$event_counter <- rv$event_counter + rv$timeline$rstBtn() + remoteReset()
        })
        
        
        #--- Catch a reset from timeline or caller
        observeEvent(req(c(rv$timeline$rstBtn()!=0, remoteReset()!=0)), {
          #print("---- MODULE _B_ : reset activated ----------------")
          #print(' ------- MODULE _B_ : observeEvent(req(c(rv$timeline$rstBtn()!=0, remoteReset()!=0)) ------- ')
          
          # Add external events to counter
          rv$event_counter <- rv$event_counter + rv$timeline$rstBtn() + remoteReset()
          
          rv$cmd <- SendCmdToTimeline(c('EnableAllSteps', 'ResetActionBtns'))
          
          rv$current.pos <- 1
          rv$event_counter <- 0
          ResetScreens()
          
          Reset_Module_Data_logics()
          
          
          # Update datasets logics
        })
        
        
        # Catch a change in isDone (validation of a step)
        # Specific to the modules of process and do not appear in pipeline module
        observeEvent(config$isDone,  ignoreInit = T, {
          #print(' ------- MODULE _B_ : A new step is validated ------- ')
          #print(' ------- MODULE _B_ : observeEvent(config$isDone,  ignoreInit = T) ------- ')
          
          rv$cmd <- SendCmdToTimeline('DisableAllPrevSteps')
        })
        
        # This listener catches the changes in the local input but not
        # those which come from caller or called modules
        # It is not necessary in the pipeline module because the toggle state
        # of process ui are managed by the process module itself.
        observe({
          reactiveValuesToList(input)
          rv$event_counter <- sum(as.numeric(unlist(reactiveValuesToList(input))), na.rm=T)
          #print(paste0('----MODULE _B_ : new event detected on reactiveValuesToList(input) : ', rv$event_counter))
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
        
        
        SendCmdToTimeline <- function(names){
          append(as.list(names), list(rv$event_counter))
          #paste0(name, '_', rv$event_counter)
        }
        
        
        Reset_Module_Data_logics <- function(){
          # Update datasets logics
          #browser()
          
          #rv$dataIn <- RemoveItemFromDataset(dataIn(), config$process.name)
          #rv$dataOut <- RemoveItemFromDataset(dataIn(), config$process.name)
          rv$dataOut <- NULL
          config$isDone <- Init_isDone()
        }
        
        
        
      }
      ############ ---   END OF REACTIVE PART OF THE SERVER   --- ###########
      
      
      
      
      #####################################################################
      ## screens of the module
      ##
      ############### SCREEN 1 ######################################
      InsertDescriptionUI()
      
      ############### SCREEN 2 ######################################
      
      output$Step1 <- renderUI({
        name <- 'Step1'
        
        
        
        tagList(
          div(id=ns(name),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  tags$h2('Step 1')),
              div(style="display:inline-block; vertical-align: middle; padding-right: 40px;",
                  selectInput(ns('select1'), 'Select step 1', 
                              choices = 1:5, 
                              selected = 1,
                              width = '150px')
              ),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  actionButton(ns(paste0('perform_', name, '_btn')), 'Perform'))
          )
        )
      })
      
      
      observeEvent(input$perform_Step1_btn, {
        config$isDone[['Step1']] <- TRUE
      })
      
      ############### SCREEN 3 ######################################
      output$Step2 <- renderUI({
        name <- 'Step2'
        tagList(
          div(id=ns(name),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  tags$h3('Step 2')),
              div(style="display:inline-block; vertical-align: middle;padding-right: 40px;",
                  selectInput(ns('select2'), 'Select step 2',
                              choices = 1:5,
                              selected = 1,
                              width = '150px')),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  actionButton(ns(paste0('perform_', name, '_btn')), 'Perform'))
          )
        )
      })
      
      ## Logics to implement: here, we must take the last data not null
      # in previous datas. The objective is to take account
      # of skipped steps
      observeEvent(input$perform_Step2_btn, {
        config$isDone[['Step2']] <- TRUE
      })
      
      
      
      
      ############### SCREEN 4 ######################################
      output$Step3 <- renderUI({
        name <- 'Step3'
        
        
        
        
        
        
        tagList(
          div(id=ns(name),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  tags$h3('Step 3')),
              div(style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                  actionButton(ns('validate_btn'), 'Validate'))
          )
        )
        
        
        
        
        
      })
      
      
      Validate_Module_Data_logics <- function(){
        #rv$dataIn <- AddItemToDataset(rv$dataIn, config$process.name)
        rv$dataOut <- AddItemToDataset(rv$dataIn, config$process.name)
        print(rv$dataOut)
      }
      
      
      observeEvent(input$validate_btn, {
        #browser()
        Validate_Module_Data_logics()
        config$isDone[['Step3']] <- TRUE
      })
      
      
      
      
      
      ##########################################################
      
      reactive({rv$dataOut})
    })
  
}

