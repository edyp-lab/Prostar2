

mod_wf_wf1_Imputation_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    mod_timeline_ui(ns("timeline")),
    hr(),
    wellPanel(
      h3('Module _A_'),
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
mod_wf_wf1_Imputation_server <- function(id, 
                                        dataIn=NULL,
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
        steps = list(Description = T,
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
        cmd = NULL)
      
      
      # Main listener of the module which initialize it
      
      
      observeEvent(isSkipped(), {
        if(verbose)
          print(paste0(config$process.name, ' : New value for isSkipped() : ', isSkipped()))
        
        rv$skipped <- isSkipped()
        if (isSkipped())
          config$isDone <- setNames(lapply(1:nbSteps(), 
                                           function(x){ if (x==1) VALIDATED else SKIPPED}), 
                                    names(config$steps))
      })
      

      
      observeEvent(dataIn(), ignoreNULL=T, ignoreInit = T, { 
        if(verbose)
          print(paste0(config$process.name, ' :  Initialization de rv$dataIn ------- '))
        
        # browser()
        inputExists <- length(names(dataIn())) > 0
        tmpExists <- length(names(rv$dataIn)) > 0
        
        if (inputExists && tmpExists){
          # this case is either the module is skipped or validated
          #rv$current.pos <- nbSteps()
          if(rv$skipped){
            if(verbose)
              print(paste0(config$process.name, ' : Skipped process'))
            
          }
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
        rv$dataOut <- NULL
        
        CommonInitializeFunctions()
        
        rv$timeline <- mod_timeline_server("timeline", 
                                           style = 2, 
                                           config = config, 
                                           position = reactive({rv$current.pos})
        )
        
        #Catch a new position from timeline
        observeEvent(req(rv$timeline$pos()), ignoreInit=T, { 
          if(verbose)
            print(paste0(config$process.name, ' : observeEvent(req(rv$timeline$pos()) ------- ',  rv$timeline$pos() ))
          rv$current.pos <- rv$timeline$pos() 
          if(verbose)
            print(paste0(config$process.name, ' : observeEvent(req(rv$timeline$pos()) ------- ', paste0(config$isDone, collapse=' ') ))
          
        })
        
        
        
        
        #--- Catch a reset from timeline or caller
        observeEvent(req(c(rv$timeline$rstBtn()!=0, remoteReset()!=0)), {
          if(verbose)
            print(paste0(config$process.name, ' : reset activated ----------------'))
          #print(' ------- MODULE _A_ : observeEvent(req(c(rv$timeline$rstBtn()!=0, remoteReset()!=0)) ------- ')
          
          rv$current.pos <- 1
          
          ResetScreens()
          
          Reset_Module_Data_logics()
          
          
          # Update datasets logics
        })
        
        
        # Catch a change in isDone (validation of a step)
        # Specific to the modules of process and do not appear in pipeline module
        # observeEvent(config$isDone,  ignoreInit = T, {
        #   if(verbose)
        #     print(paste0(config$process.name, ' : A new step is validated ---- ', paste0(unlist(config$isDone), collapse=' ')))
        #   #print(' ------- MODULE _A_ : observeEvent(config$isDone,  ignoreInit = T) ------- ')
        #   #print(paste0(unlist(config$isDone), collapse=' '))
        #   })
        # 
        
        
        # This function cannot be implemented in the timeline module because 
        # the id of the screens to reset are not known elsewhere.
        # Trying to reset the global 'div_screens' in the timeline module
        # does not work
        ResetScreens <- function(screens){
          lapply(1:nbSteps(), function(x){
            shinyjs::reset(names(config$steps)[x])
          })
        }
        
        Reset_Module_Data_logics <- function(){
          if(verbose)
            print(paste0(config$process.name, '# Update datasets logics'))
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
        config$isDone[['Step1']] <- VALIDATED
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
        config$isDone[['Step2']] <- VALIDATED
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
      }
      
      
      observeEvent(input$validate_btn, {
        #browser()
        Validate_Module_Data_logics()
        config$isDone[['Step3']] <- VALIDATED
      })
      
      
      
      
      
      ##########################################################
      
      reactive({rv$dataOut})
    })
  
}

