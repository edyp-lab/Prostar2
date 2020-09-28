
mod_super_timeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_tl_engine_ui(ns('tl_engine'))
  )
}

#' @param dataIn xxx
#'
#' 
#' 
mod_super_timeline_server <- function(id, dataIn=NULL){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      rv <- reactiveValues(
        tmp_super = F,
        screens=NULL
      )
      
      
      # variables to communicate with the navigation module
      rv.process_config <- reactiveValues(
        type = 'pipeline',
        process.name = 'Pipeline protein',
        stepsNames = c("Description", "Filtering", "Normalization", "Imputation", "Summary"),
        isDone =  c(TRUE, rep(FALSE,4)),
        mandatory =  c(FALSE, FALSE, TRUE, TRUE, TRUE)
      )
      
      # Initialization of the process
      observeEvent(req(dataIn()), {
        print("--------------------------------------------------")
        print('MODULE SUPER_TIMELINE : Initialisation du module')
        rv$dataIn <- dataIn()
        print(paste0("      names(dataIn()) = ", paste0(names(dataIn()), collapse=' - ')))
        print(paste0("      names(rv$dataIn) = ", paste0(names(rv$dataIn), collapse=' - ')))
        print(paste0("      names(rv$dataOut) =" , paste0(names(rv$dataOut), collapse=' - ')))
        
        # Instantiation of the screens
        rv$screens <- lapply(1:length(rv.process_config$stepsNames), function(x){
          do.call(uiOutput, list(outputId=ns(paste0("screen", x))))}) 
        })
      
      # Here, there is no remoteReset because there is no upper level
      rv$tmp_super <- mod_tl_engine_server('tl_engine',
                                     process_config = rv.process_config,
                                     screens = rv$screens,
                                     remoteReset = reactive(FALSE))

      
      # Catch the reset events (local or remote)
      observeEvent(req(rv$tmp_super()), { 
        print(paste0('MODULE SUPER_TL : new value for rv$tmp_super() = ', rv$tmp_super()))
        UpdateDataIn()
        
        
        #rv$dataOut <- NULL
        
        print("MODULE SUPER_TL : after updating datasets")
        print(paste0("      names(dataIn()) = ", paste0(names(dataIn()), collapse=' - ')))
        print(paste0("      names(rv$dataIn) = ", paste0(names(rv$dataIn), collapse=' - ')))
        print(paste0("      names(rv$dataOut) =" , paste0(names(rv$dataOut), collapse=' - ')))
        
      })
      
    
      
      # If this step has been validated, then one need to delete the last
      # record in the dataset,
      # else on change have to reload the current dataset to reinit the module
      # The condition is on the presence of the name in the dataset rather then
      # on the value of the last element of isDone vector because if the value is set 
      # to TRUE and, for any reason, the dataset is not updated, it may have a bug
      
      # If there are further elements in the dataset after the current one, 
      # then they are deleted
      
      # In order to trigger the initialization of the module, one change 
      # the value of rv$dataOut in the case where it is necessary
      UpdateDataIn <- reactive({
        print('MODULE SUPER_TL : UpdateDataIn(). RESET ALL')
        rv$dataIn <- dataIn()
      })
      

      #####################################################################
      ## screens of the module
      ##
      ############### SCREEN 1 ######################################
      output$screen1 <- renderUI({
        tagList(
          tags$h3(paste0('Pipeline ', rv.process_config$name))
        )
      })
      
      
      ############### SCREEN 2 ######################################
      
      output$screen2 <- renderUI({
        
        tagList(
          div(id=ns('screen2'),
              tags$h3('Processus 1'),
              mod_wf_wf1_A_ui(ns('mod_A_nav'))
          )
        )
      })

      rv$tmpA <- mod_wf_wf1_A_server("mod_A_nav",
                                     dataIn = reactive({rv$dataIn}),
                                     remoteReset = reactive({rv$tmp_super()}) )
      
      observeEvent(req(rv$tmpA$dataOut()),  { 
        print('MODULE SUPER_TL : New value for rv$tmpA() :')
        print(paste0("      names(rv$tmpA$dataOut()) = ", paste0(names(rv$tmpA$dataOut()), collapse=' - ')))
        rv$dataIn <- rv$tmpA$dataOut()
        rv$dataOut <- rv$dataIn
      })
      
      observeEvent(rv$tmpA$validated(), ignoreInit = T, { 
        print('MODULE SUPER_TL : New value for rv$validated() :')
        print(paste0("      rv$tmpA$validated() = ", rv$tmpA$validated()))
        rv.process_config$isDone[2] <- rv$tmpA$validated()
      })
      
      ############### SCREEN 3 ######################################
      output$screen3 <- renderUI({
        
        tagList(
          div(id=ns('screen3'),
              tags$h3('Processus 2'),
              mod_wf_wf1_A_ui(ns('mod_B_nav'))
          )
        )
      })
      
      rv$tmpB <- mod_wf_wf1_B_server("mod_B_nav",
                                     dataIn = reactive({rv$dataIn}),
                                     remoteReset = reactive({rv$tmp_super()}) )
      observeEvent(req(rv$tmpB$dataOut()),  { 
        print('MODULE SUPER_TL : New value for rv$tmpB$dataOut() :')
        print(paste0("      names(rv$tmpB$dataOut()) = ", paste0(names(rv$tmpB$dataOut()), collapse=' - ')))
        rv$dataIn <- rv$tmpB$dataOut()
        rv$dataOut <- rv$dataIn
      })
      
      observeEvent(rv$tmpB$validated(),  { 
        print('MODULE SUPER_TL : New value for rv$tmpB$validated() :')
        print(paste0("      rv$tmpB$validated() = ", rv$tmpB$validated()))
        rv.process_config$isDone[2] <- rv$tmpB$validated()
      })
      
      
      
      
      ############### SCREEN 4 ######################################
      output$screen4 <- renderUI({
        
        tagList(
          div(id=ns('screen4'),
              tags$h3('Processus 3'),
              mod_wf_wf1_A_ui(ns('mod_C_nav'))
          )
        )
      })
      
      rv$tmpC <- mod_wf_wf1_A_server("mod_C_nav",
                                     dataIn = reactive({rv$dataIn}),
                                     remoteReset = reactive({rv$tmp_super()}) )
      observeEvent(req(rv$tmpC$dataOut()),  { 
        print('MODULE SUPER_TL : New value for rv$tmpC$dataOut() :')
        print(paste0("      names(rv$tmpC$dataOut()) = ", paste0(names(rv$tmpC$dataOut()), collapse=' - ')))
        rv$dataIn <- rv$tmpC$dataOut()
        rv$dataOut <- rv$dataIn
      })
      
      observeEvent(rv$tmpC$validated(),  ignoreInit=T, { 
        print('MODULE SUPER_TL : New value for rv$tmpC$validated() :')
        print(paste0("      rv$tmpC$validated() = ", rv$tmpC$validated()))
        rv.process_config$isDone[2] <- rv$tmpC$validated()
      })
      
      
      
      ############### SCREEN 5 ######################################
      output$screen5 <- renderUI({
        
        tagList(
          div(id='screen5',
              tags$h3('Summary')
          )
        )
      })
      
      ##########################################################
      
      reactive({rv$dataOut})
    }
  )
}

