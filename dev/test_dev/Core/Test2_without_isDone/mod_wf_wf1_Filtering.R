

mod_wf_wf1_Filtering_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    hr(),
    wellPanel(
      h3('Module Filtering'),
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
mod_wf_wf1_Filtering_server <- function(id, 
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
        process.name = 'Filtering',
        steps = list(Description = T,
                     Step1 = T,
                     Step2 = F,
                     Step3 = T)
      )
      
      
      rv <- reactiveValues(
        current.pos = NULL,
        timeline = NULL,
        dataIn = NULL,
        wake = FALSE)
      
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
        Initialize_Status_Process()
        rv$screens <- InitScreens(nbSteps())
        # Must be placed after the initialisation of the 'config$stepsNames' variable
        config$screens <- CreateScreens(names(config$steps))
        #InitializeDataIn()
        rv$current.pos <- 1
      }
      
      InitializeDataIn <- function(){
        if(verbose)
          print(paste0(config$process.name, ' : InitializeDataIn() ------- '))
        
        rv$dataIn <- dataIn()
      }
      
      
      
      InitializeTimeline <- function(){
        rv$timeline <- mod_timeline_server("timeline", 
                                           style = 2, 
                                           config = config,
                                           wake = reactive({rv$wake}))
        
        #Catch a new position from timeline
        observeEvent(req(rv$timeline$pos()), ignoreInit=T, { 
          if(verbose)
            print(paste0(config$process.name, ' : observeEvent(req(rv$timeline$pos()) ------- ',  rv$timeline$pos() ))
          rv$current.pos <- rv$timeline$pos() 
          
        })
        
        
        #--- Catch a reset from timeline or caller
        observeEvent(req(c(rv$timeline$rstBtn(), remoteReset()!=0)), {
          if(verbose)
            print(paste0(config$process.name, ' : reset activated ----------------'))
          
          ResetScreens()
          rv$dataIn <- NA
          rv$current.pos <- 1
          rv$wake <- Wake()
          Initialize_Status_Process()
          Send_Result_to_Caller()
          InitializeDataIn()
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
      


      
      Send_Result_to_Caller <- reactive({
        if(verbose)
          print(paste0(config$process.name, ' : Execution of Send_Result_to_Caller() : '))
        
        dataOut$obj <- rv$dataIn
        dataOut$name <- config$process.name
        dataOut$trigger <- Wake()
        
        if(verbose)
          print(paste0(config$process.name, ' : dataOut$obj =  : ', paste0(names(dataOut$obj), collapse=' ')))
      })
      
      
      observeEvent(config$status,{Set_Skipped_Status()})
      
      ValidateCurrentPos <- reactive({
        if(verbose)
          print(paste0(config$process.name, ' : ValidateCurrentPos() = '))
        
        config$status[rv$current.pos] <- VALIDATED
        Set_Skipped_Status()
        #browser()
        if (config$status[nbSteps()] == VALIDATED)
          # Either the process has been validated, one can prepare data to ben sent to caller
          # Or the module has been reseted
          Send_Result_to_Caller()
      })
      
      
      GetValidationBtnIds <- reactive({
        
        validated.btns <-grep('_validate_btn', names(input))
      })
      
      
      #####################################################################
      ## screens of the module
      ##
      ############### SCREEN 1 ######################################
      output$Description <- renderUI({
        tagList(
          actionButton(ns('start_btn'), 
                       paste0('Start ', config$process.name),
                       class = btn_success_color),
          mod_insert_md_ui(ns(paste0(config$process.name, "_md")))
        )
      })
       mod_insert_md_server(paste0(config$process.name, "_md"), 
                            paste0('./md/',config$process.name, '.md'))
      
       observeEvent(input$start_btn, {
         if(verbose)
           print(paste0(config$process.name, ' : Clic on start_btn'))

         InitializeDataIn()
         ValidateCurrentPos()
       })
       
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
        ValidateCurrentPos()
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
        ValidateCurrentPos()
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
      
      
      observeEvent(input$validate_btn, {
        rv$dataIn <- AddItemToDataset(rv$dataIn, config$process.name)
        ValidateCurrentPos()
      })
      
      
    })
  
}

