

mod_wf_wf1_B_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_tl_engine_ui(ns('tl_engine'))
  )
}

#' @param dataIn xxx
#'
#' 
#' 
mod_wf_wf1_B_server <- function(id, dataIn=NULL, remoteReset=FALSE){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      rv <- reactiveValues(
        tmp_engine = F,
        screens=NULL
      )
      
      
      
      
      # variables to communicate with the navigation module
      rv.process_config <- reactiveValues(
        process.name = 'Process B',
        stepsNames = c("Description", "Step 1", "Step 2", "Step 3"),
        isDone =  c(TRUE, FALSE, FALSE, FALSE),
        mandatory =  c(FALSE, FALSE, TRUE, TRUE)
      )
      
      # Initialization of the process
      observeEvent(req(dataIn()), { 
        print("--------------------------------------------------")
        print('MODULE TL_ENGINE : Initialisation du module B')
        rv$dataIn <- dataIn()
        rv$process.validated <- rv.process_config$isDone[length(rv.process_config$isDone)]
        print(paste0("      names(dataIn()) = ", paste0(names(dataIn()), collapse=' - ')))
        print(paste0("      names(rv$dataIn) = ", paste0(names(rv$dataIn), collapse=' - ')))
        print(paste0("      names(rv$dataOut) =" , paste0(names(rv$dataOut), collapse=' - ')))
        
        # Instantiation of the screens
        rv$screens <- lapply(1:length(rv.process_config$stepsNames), function(x){
          do.call(uiOutput, list(outputId=ns(paste0("screen", x))))}) 
      })
      
      # output$show_dataIn <- renderPrint({dataIn()})
      # output$show_rv_dataIn <- renderPrint({rv$dataIn})
      # output$show_rv_dataOut <- renderPrint({rv$dataOut})
      
      # The remoteReset argument is used to communicate between the caller
      # and this module
      rv$tmp_engine <- mod_tl_engine_server('tl_engine',
                                            process_config = rv.process_config,
                                            screens = rv$screens,
                                            remoteReset = reactive(remoteReset())
      )
      
      
      observeEvent(rv.process_config$isDone, ignoreInit=T, {
        print(paste0('MODULE B : new value for rv.process_config$isDone = ', rv.process_config$isDone))
        print("     Disable all previous screens")
        pos <- max(grep(TRUE, rv.process_config$isDone))
        lapply(1:pos, function(x){ shinyjs::disable(paste0('screen', x))})
      })
      
      
      # Catch the reset events (local or remote)
      observeEvent(req(c(rv$tmp_engine())), ignoreInit=T, { 
        print(paste0('MODULE B : new value for rv$tmp_engine() = ', rv$tmp_engine()))
        print(paste0('MODULE B : new value for remoteReset() = ', remoteReset()))
        UpdateDataIn()
        # this setting allows to trigger the initialization of the module
        #rv$dataOut <- rv$dataIn
        rv$process.validated <- F
        print("MODULE B : after updating datasets")
        print(paste0("      names(dataIn()) = ", paste0(names(dataIn()), collapse=' - ')))
        print(paste0("      names(rv$dataIn) = ", paste0(names(rv$dataIn), collapse=' - ')))
        print(paste0("      names(rv$dataOut) =" , paste0(names(rv$dataOut), collapse=' - ')))
        print(paste0("      rv.process_config =" , paste0(rv.process_config$isDone, collapse=' - ')))
        
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
        print('MODULE B : UpdateDataIn()')
        #browser()
        ind <- grep(rv.process_config$process.name, names(rv$dataIn))
        if (length(ind) == 0)
          rv$dataIn <- dataIn()
        else
          rv$dataIn <- dataIn()[ , , -c(ind:length(dataIn()))]
      })
      
      output$show_dataIn <- renderPrint({rv$dataIn})
      output$show_dataOut <- renderPrint({rv$dataOut})
      
      
      
      # observeEvent( rv.process_config$isDone[length(rv.process_config$isDone)], {
      #   rv$process.validated <- rv.process_config$isDone[length(rv.process_config$isDone)]
      # } )            
      
      #####################################################################
      ## screens of the module
      ##
      ############### SCREEN 1 ######################################
      output$screen1 <- renderUI({
        tagList(
          tags$h3(paste0('Process ', rv.process_config$name))
        )
      })
      
      
      ############### SCREEN 2 ######################################
      
      output$screen2 <- renderUI({
        tagList(
          div(id=ns('screen2'),
              tags$h2('Step 1'),
              actionButton(ns('perform_screen2_btn'), 'Perform'),
              selectInput(ns('select1'), 'Select step 1', 
                          choices = 1:5, 
                          selected = 1,
                          width = '150px')
          )
        )
      })
      
      observeEvent(input$perform_screen2_btn, {
        # Put here the code for modifying the QF after this step
        shinyjs::disable('screen2')
        rv.process_config$isDone[2] <- TRUE
      })
      
      
      ############### SCREEN 3 ######################################
      output$screen3 <- renderUI({
        
        tagList(
          div(id=ns('screen3'),
              tags$h3('Step 2'),
              actionButton(ns('perform_screen3_btn'), 'Perform'),
              selectInput(ns('select2'), 'Select step 2',
                          choices = 1:5,
                          selected = 1,
                          width = '150px')
          )
        )
      })
      
      ## Logics to implement: here, we must take the last data not null
      # in previous datas. The objective is to take account
      # of skipped steps
      observeEvent(input$perform_screen3_btn, {
        shinyjs::disable('screen3')
        #rv$dataIn <- rv$dataIn[[length(rv$dataIn)]] + as.numeric(input$select2)
        rv.process_config$isDone[3] <- TRUE
      })
      
      
      ############### SCREEN 4 ######################################
      output$screen4 <- renderUI({
        
        tagList(
          div(id=ns('screen4'),
              tags$h3('Step 4'),
              actionButton(ns('validate_btn'), 'Validate')
          )
        )
      })
      
      observeEvent(input$validate_btn, {
        isolate({
          shinyjs::disable('screen4')
          rv$dataIn <- addAssay(rv$dataIn, 
                                rv$dataIn[[length(rv$dataIn)]], 
                                name=rv.process_config$process.name)
          rv$dataOut <- rv$dataIn
          rv$dataIn <- NULL
          rv$process.validated <- TRUE
          rv.process_config$isDone[4] <- TRUE
        })
      })
      
      
      
      
      ##########################################################
      
      list(dataOut = reactive({rv$dataOut}),
           validated = reactive({rv$process.validated})
      )
    }
  )
}

