

mod_wf_wf1_Original_ui <- function(id){
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
mod_wf_wf1_Original_server <- function(id, 
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
        process.name = 'Original',
        steps = list(Description = T)
      )
      
      #################################################################################
      
      rv <- reactiveValues(
        dataIn = NULL,
        dataOut = NULL
        )
      
      
      # Main listener of the module which initialize it
      
      observeEvent(dataIn(), ignoreNULL=T, ignoreInit = T, { 
        if(verbose)
          print(paste0(config$process.name, ' :  Initialization de rv$dataIn ------- '))
        
        # browser()
        inputExists <- length(names(dataIn())) > 0
        tmpExists <- length(names(rv$dataIn)) > 0
        
        if (inputExists && tmpExists){
          # this case is either the module is skipped or validated
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
        
        
        #--- Catch a reset from timeline or caller
        observeEvent(req(c(rv$timeline$rstBtn()!=0, remoteReset()!=0)), {
          if(verbose)
            print(paste0(config$process.name, ' : reset activated ----------------'))
          #print(' ------- MODULE _A_ : observeEvent(req(c(rv$timeline$rstBtn()!=0, remoteReset()!=0)) ------- ')
          
          rv$current.pos <- 1
          
          ResetScreens()
          
          Reset_Module_Data_logics()
        })

        
        Reset_Module_Data_logics <- function(){
          if(verbose)
            print(paste0(config$process.name, '# Update datasets logics'))
          #browser()
          rv$dataOut <- NULL
          config$isDone <- Init_isDone()
        }
        
        
        
      }
      
      
      #####################################################################
      ## screens of the module
      ##
      ############### SCREEN 1 ######################################
      output$Description <- renderUI({
        tagList(
        actionButton(ns('start'), "Start"),
         mod_insert_md_ui(ns(paste0(config$process.name, "_md")))
        )
      })
       mod_insert_md_server(paste0(config$process.name, "_md"), 
                            paste0('./md/',config$process.name, '.md'))
      
      
      observeEvent(input$perform_Description_btn, {
        Validate_Module_Data_logics()
        config$isDone[['Description']] <- VALIDATED
      })
      
      Validate_Module_Data_logics <- function(){
        rv$dataOut <- AddItemToDataset(rv$dataIn, config$process.name)
      }
      

      ##########################################################
      
      reactive({rv$dataOut})
    })
  
}

