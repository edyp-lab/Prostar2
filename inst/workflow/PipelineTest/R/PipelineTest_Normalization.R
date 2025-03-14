#' @title Shiny example process module.
#'
#' @description
#' This module contains the configuration information for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package MagellanNTK
#' 
#' The name of the server and ui functions are formatted with keywords separated by '_', as follows:
#' * first string `mod`: indicates that it is a Shiny module
#' * `pipeline name` is the name of the pipeline to which the process belongs
#' * `process name` is the name of the process itself
#' 
#' This convention is important because MagellanNTK call the different
#' server and ui functions by building dynamically their name.
#' 
#' In this example, `PipelineTest_Normalization_ui()` and `PipelineTest_Normalization_server()` define
#' the code for the process `ProcessTest` which is part of the pipeline called `PipelineTest`.
#' 
#' @example inst/workflow/PipelineTest/examples/example_PipelineTest_Normalization.R
#' 
NULL

#' @rdname PipelineTest
#' @export
#' 
PipelineTest_Normalization_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelineTest_Normalization',
    mode = 'process',
    steps = c('Normalization'),
    mandatory = c(FALSE)
    )
}


#' @param id xxx
#' 
#' @rdname PipelineTest
#' 
#' @author Samuel Wieczorek
#' 
#' @export
#'
PipelineTest_Normalization_ui <- function(id){
  ns <- NS(id)
}


#' @param id xxx
#'
#' @param dataIn An instance of the class 
#'
#' @param steps.enabled A vector of boolean which has the same length of the steps
#' of the pipeline. This information is used to enable/disable the widgets. It is not
#' a communication variable between the caller and this module, thus there is no
#' corresponding output variable
#'
#' @param remoteReset It is a remote command to reset the module. A boolean that
#' indicates if the pipeline has been reset by a program of higher level
#' Basically, it is the program which has called this module
#' 
#' @param steps.status xxx
#' 
#' @param current.pos xxx
#'
#' @rdname PipelineTest
#' 
#' @importFrom stats setNames rnorm
#' 
#' @export
#' 
PipelineTest_Normalization_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1})
){
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list(
    Normalization_method = "None",
    Normalization_type = "overall",
    Normalization_spanLOESS = 0.7,
    Normalization_quantile = 0.15,
    Normalization_varReduction = FALSE,
    Normalization_sync = FALSE
  )
  
  rv.custom.default.values <- list(
    tmp.dataset = NULL,
    #init.dataset = NULL,
    history = NULL,
    selectProt = reactive({NULL})
  )
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    core.code <- MagellanNTK::Get_Workflow_Core_Code(
      mode = 'process',
      name = id,
      w.names = names(widgets.default.values),
      rv.custom.names = names(rv.custom.default.values)
    )

    eval(str2expression(core.code))

    
    
    # >>>
    # >>> START ------------- Code for Description UI---------------
    # >>> 
    
    
    output$Description <- renderUI({
      # print('titi')
      # print(session$userData$workflow.path)
      # file <- normalizePath(file.path(session$userData$workflow.path, 
      #   'md', paste0(id, '.md')))
      file <- normalizePath(file.path(
        system.file('workflow', package = 'Prostar2'),
        unlist(strsplit(id, '_'))[1], 
        'md', 
        paste0(id, '.md')))
     
      tagList(
        # In this example, the md file is found in the extdata/module_examples directory
        # but with a real app, it should be provided by the package which
        # contains the UI for the different steps of the process module.
        # Insert validation button
        uiOutput(ns('Description_btn_validate_ui')),
        
        # Used to show some information about the dataset which is loaded
        # This function must be provided by the package of the process module
        uiOutput(ns('datasetDescription_ui')),
        
        if (file.exists(file))
          includeMarkdown(file)
        else
          p('No Description available')
      )
    })
    
    output$datasetDescription_ui <- renderUI({
      # Insert your own code to visualize some information
      # about your dataset. It will appear once the 'Start' button
      # has been clicked
      
    })
    
    output$Description_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Description_btn_validate"),
                             "Start",
                             class = "btn-success")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Description'])
    })
    
    
    observeEvent(input$Description_btn_validate, {
      rv$dataIn <- dataIn()

      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- stepStatus$VALIDATED
    })
    
    
    
    # >>>
    # >>> START ------------- Code for Normalization UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI ------------------------------------
    output$Normalization <- renderUI({
      shinyjs::useShinyjs()
        tagList(
          uiOutput(ns("Normalization_btn_validate_ui"))
          )
    })
    

    
    output$Normalization_btn_validate_ui <- renderUI({
      widget <-  actionButton(ns("Normalization_btn_validate"),
                              "Run Normalization",
                              class = "btn-success")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Normalization'] )
      
    })
    # >>> END: Definition of the widgets
    
    observeEvent(input$Normalization_btn_validate, {
      
      # Do some stuff 
      req(rv.widgets$Normalization_method)
      req(rv$dataIn)
      
        new.dataset <- rv$dataIn[[length(rv$dataIn)]]
        assay(new.dataset) <- rv.custom$tmpAssay
        DaparToolshed::paramshistory(new.dataset) <- NULL
        DaparToolshed::paramshistory(new.dataset) <- rv.custom$history
        rv$dataIn <- QFeatures::addAssay(rv$dataIn, new.dataset, 'Normalization')
        
        
        # DO NOT MODIFY THE THREE FOLLOWING LINES
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status['Normalization'] <- stepStatus$VALIDATED
    })
    
    # <<< END ------------- Code for step 1 UI---------------
    
    
    # >>> START ------------- Code for step 3 UI---------------
    output$Save <- renderUI({
      tagList(
        # Insert validation button
        # This line is necessary. DO NOT MODIFY
        uiOutput(ns('Save_btn_validate_ui'))
      )
    })
    
    output$Save_btn_validate_ui <- renderUI({
      tagList(
        MagellanNTK::toggleWidget( 
          actionButton(ns("Save_btn_validate"), "Save",
            class = "btn-success"),
          rv$steps.enabled['Save']
        ),
        if (config@mode == 'process' && 
            rv$steps.status['Save'] == stepStatus$VALIDATED) {
          download_dataset_ui(ns('createQuickLink'))
        }
      )
      
    })
    observeEvent(input$Save_btn_validate, {
      # Do some stuff
      req(rv.custom$tmpAssay)
      req(rv.custom$history)
      
      
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Save'] <- stepStatus$VALIDATED
      Prostar2::download_dataset_server('createQuickLink', dataIn = reactive({rv$dataIn}))
      
    })
    # <<< END ------------- Code for step 3 UI---------------
    
    
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  }
  )
}
