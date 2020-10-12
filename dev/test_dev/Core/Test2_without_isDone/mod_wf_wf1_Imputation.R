

mod_wf_wf1_Imputation_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    hr(),
    wellPanel(
      h3('Module Imputation'),
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
mod_wf_wf1_Imputation_server <- function(id, 
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
        process.name = 'Imputation',
        steps = list(Description = F,
                     Step1 = T,
                     Step2 = F,
                     Step3 = T)
      )
      
      #################################################################################
      
      rv <- reactiveValues(
        current.pos = 1,
        timeline = NULL,
        dataIn = NULL,
        dataOut = NULL,
        old.rst = 0,
        wake = FALSE)
      
      # Main listener of the module which initialize it
      
      
      observeEvent(isSkipped(), {
        if(verbose)
          print(paste0(config$process.name, ' : New value for isSkipped() : ', isSkipped()))
        
        rv$skipped <- isSkipped()
        if (isSkipped())
          config$status <- setNames(lapply(1:nbSteps(), 
                                           function(x){ if (x==1) VALIDATED else SKIPPED}), 
                                    names(config$steps))
      })
      
      
      
      BuildStatus <- reactive({
        #browser()
        config$status <- setNames(lapply(1:nbSteps(), 
                                         function(x){if (x==1) VALIDATED else GetStatusPosition(x)}), 
                                  names(config$steps))
      })
      
      
      
      GetCurrentStepName <- reactive({ names(config$steps)[rv$current.pos] })
      GetStatusPosition <- function(pos){ config$status[[pos]] }
      
      observeEvent(req(dataIn()), ignoreNULL=T, ignoreInit = F, { 
        if(verbose)
          print(paste0(config$process.name, " :  reception d'un nouveau dataIn() : ", paste0(names(dataIn()), collapse=' ')))
        
        #browser()
        inputExists <- length(dataIn()) > 0
        tmpExists <- !is.null(rv$dataIn)
        rv$wake <- FALSE
        if (inputExists && tmpExists){
          # this case is either the module is skipped or validated
          rv$wake <- runif(1,0,1)
          # if(rv$skipped){
          #   if(verbose)
          #     print(paste0(config$process.name, ' : Skipped process'))
          # 
          # }
        }
        else if (inputExists && !tmpExists){
          # The current position is pointed on a new module
          InitializeModule()
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
        
      })
      
      
      
      
      
      
      InitializeModule <- function(){
        if(verbose)
          print(paste0(config$process.name, ' : InitializeModule() ------- '))
        rv$dataIn <- dataIn()
        rv$current.pos <- 1
        
        CommonInitializeFunctions()
        BuildStatus()
        
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
        observeEvent(req(c(rv$timeline$rstBtn() > rv$old.rst, remoteReset()!=0)), {
          if(verbose)
            print(paste0(config$process.name, ' : reset activated ----------------'))
          
          ResetScreens()
          rv$old.rst <- rv$timeline$rstBtn()
          
          InitializeModule()
          BuildStatus()
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
        
      }
      ############ ---   END OF REACTIVE PART OF THE SERVER   --- ###########
      
      
      
      
      UpdateDataOut <- reactive({
        print(paste0(config$process.name, ' : Execution of UpdateDataOut() : '))
        
        dataOut$obj <- rv$dataOut
        dataOut$name <- config$process.name
        dataOut$trigger <- runif(1,0,1)
        
        if(verbose)
          print(paste0(config$process.name, ' : dataOut$obj =  : ', paste0(names(dataOut$obj), collapse=' ')))
      })
      
      #####################################################################
      ## screens of the module
      ##
      ############### SCREEN 1 ######################################
      output$Description <- renderUI({
        # mod_insert_md_ui(ns(paste0(config$process.name, "_md")))
      })
      # mod_insert_md_server(paste0(config$process.name, "_md"), 
      #                      paste0('./md/',config$process.name, '.md'))
      
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
        config$status[['Step1']] <- VALIDATED
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
        config$status[['Step2']] <- VALIDATED
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
        rv$dataOut <- AddItemToDataset(rv$dataIn, config$process.name)
        UpdateDataOut()
        config$status[['Step3']] <- VALIDATED
      })
      
      
    })
  
}

