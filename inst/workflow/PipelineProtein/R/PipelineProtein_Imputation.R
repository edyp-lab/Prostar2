#' @title Shiny example process module.
#'
#' @description
#' This module contains the configuration informations for the corresponding pipeline.
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
#' In this example, `PipelineProtein_Imputation_ui()` and `PipelineProtein_Imputation_server()` define
#' the code for the process `PipelineProtein_Imputation` which is part of the pipeline called `PipelineProtein`.
#' 
#' @examplesIf interactive()
#' library(MagellanNTK)
#' data(Exp1_R25_prot, package = 'DaparToolshedData')
#' path <- system.file('workflow/PipelineProtein', package = 'Prostar2')
#' shiny::runApp(workflowApp("PipelineProtein_Imputation", path, dataIn = Exp1_R25_prot))
#' 
#' 

#' @rdname PipelineProtein
#' @export
#' 
PipelineProtein_Imputation_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelineProtein_Imputation',
    mode = 'process',
    steps = c('POV Imputation', 'MEC Imputation'),
    mandatory = c(FALSE, FALSE)
  )
}

#' @param id xxx
#' 
#' @rdname PipelineProtein
#' 
#' @author Samuel Wieczorek
#' 
#' @export
#'
PipelineProtein_Imputation_ui <- function(id){
  ns <- NS(id)
}


#' @param id xxx
#'
#' @param dataIn The dataset
#'
#' @param steps.enabled A vector of boolean which has the same length of the steps
#' of the pipeline. This information is used to enable/disable the widgets. It is not
#' a communication variable between the caller and this module, thus there is no
#' corresponding output variable
#'
#' @param remoteReset It is a remote command to reset the module. A boolean that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#' 
#' @param steps.status xxx
#' 
#' @param current.pos xxx
#'
#' @rdname PipelineProtein
#' 
#' @importFrom stats setNames rnorm
#' 
#' @export
#' 
PipelineProtein_Imputation_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({FALSE}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  path = NULL
){
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list()
  
  rv.custom.default.values <- list(
    dataIn1 = NULL,
    dataIn2 = NULL,
    tmp.mec = reactive({NULL}),
    tmp.pov = reactive({NULL}),
    history = list()
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
      # file <- normalizePath(file.path(session$userData$workflow.path, 
      #   'md', paste0(id, '.md')))
      
      file <- normalizePath(file.path(
        system.file('workflow', package = 'Prostar2'),
        unlist(strsplit(id, '_'))[1], 
        'md', 
        paste0(id, '.md')))
      
      
      tagList(
        # In this example, the md file is found in the module_examples directory
        # but with a real app, it should be provided by the package which
        # contains the UI for the different steps of the process module.
        # system.file(xxx)

        # Insert validation button
        uiOutput(ns('Description_btn_validate_ui')),
        
        if (file.exists(file))
          includeMarkdown(file)
        else
          p('No Description available'),
        
        
        # Used to show some information about the dataset which is loaded
        # This function must be provided by the package of the process module
        uiOutput(ns('datasetDescription_ui'))
      )
    })
    
    output$datasetDescription_ui <- renderUI({
      # Insert your own code to vizualise some information
      # about your dataset. It will appear once the 'Start' button
      # has been clicked
      
    })
    
    output$Description_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Description_btn_validate"),
                             "Start",
                             class = "btn-success")
      toggleWidget(widget, rv$steps.enabled['Description'])
    })
    
    
    observeEvent(input$Description_btn_validate, {
      req(dataIn())
      rv$dataIn <- dataIn()
      rv.custom$dataIn1 <- dataIn()
      rv.custom$dataIn2 <- dataIn()
      
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- stepStatus$VALIDATED
    })
    
    
    
    # >>>
    # >>> START ------------- Code for step 1 UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI ------------------------------------
    output$POVImputation <- renderUI({
      wellPanel(
        # uiOutput for all widgets in this UI
        # This part is mandatory
        # The renderUI() function of each widget is managed by MagellanNTK
        # The dev only have to define a reactive() function for each
        # widget he want to insert
        # Be aware of the naming convention for ids in uiOutput()
        # For more details, please refer to the dev document.
        
        # Insert validation button
        #uiOutput(ns("POVImputation_btn_validate_ui")),
        uiOutput(ns("POVImputation_ui"))
      )
    })
    
    
    observe({
      rv.custom$tmp.pov <- Prostar2::mod_Prot_Imputation_POV_server(
      id = 'pov',
      obj = reactive({rv$dataIn}),
      i = reactive({length(rv$dataIn)}),
      is.enabled = reactive({rv$steps.enabled["POVImputation"]}),
      remoteReset = reactive({remoteReset()})
      )
    })

    output$POVImputation_ui <- renderUI({
      widget <- Prostar2::mod_Prot_Imputation_POV_ui(ns('pov'))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['POVImputation'] )
    })
    
    # output$POVImputation_btn_validate_ui <- renderUI({
    # 
    #   widget <-  actionButton(
    #     ns("POVImputation_btn_validate"),
    #     "Validate step", class = "btn-success")
    #   MagellanNTK::toggleWidget(widget, rv$steps.enabled['POVImputation'])
    #   
    # })
    # >>> END: Definition of the widgets
    
    
    observeEvent(req(rv.custom$tmp.pov()$value), {
      
      # Do some stuff
      rv.custom$dataIn1 <- rv.custom$tmp.pov()$value
      rv.custom$dataIn2 <- rv.custom$tmp.pov()$value
      
     .history <- rv.custom$tmp.pov()$value[[length(rv.custom$tmp.pov()$value)]]
      rv.custom$params.tmp[['Imputation']][['POVImputation']] <- paramshistory(.history)
      
      # DO NOT MODIFY THE THREE FOLLOWING LINES
      dataOut$trigger <- Timestamp()
      dataOut$value <- NULL
      rv$steps.status['POVImputation'] <- stepStatus$VALIDATED
    })

    # <<< END ------------- Code for step 1 UI---------------
    
    
    # >>> START ------------- Code for step 2 UI---------------
    
    output$MECImputation <- renderUI({
      wellPanel(
        
        # Insert validation button
        # This line is necessary. DO NOT MODIFY
        #uiOutput(ns('MECImputation_btn_validate_ui')),
        uiOutput(ns('MECImputation_ui'))
        
      )
    })
    
 
    #observe({
    #  req(rv$dataIn)
    #  dataIn <- rv$custom
      # # If the previous step has been run and validated,
      # # Update dataIn to its result
      #if (rv$steps.status["Cellmetadatafiltering"] == stepStatus$VALIDATED)
     # browser()
      # if (!is.null(rv.custom$tmp1()$value))
      #   dataIn <- Prostar2::addDatasets(rv$dataIn, 
      #     rv.custom$tmp1()$value, 'POVImputation')
      
    observe({
    rv.custom$tmp.mec <- Prostar2::mod_Prot_Imputation_MEC_server(
      id = 'mec',
      obj = reactive({rv.custom$dataIn1}),
      i = reactive({length(rv.custom$dataIn1)}),
      is.enabled = reactive({rv$steps.enabled["MECImputation"]}),
      remoteReset = reactive({remoteReset()})
    )
    })
    
    output$MECImputation_ui <- renderUI({
      widget <- Prostar2::mod_Prot_Imputation_MEC_ui(ns('mec'))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['MECImputation'] )
    })
    
    # output$MECImputation_btn_validate_ui <- renderUI({
    #   widget <- actionButton(ns("MECImputation_btn_validate"),
    #                          "Perform",
    #                          class = "btn-success")
    #   MagellanNTK::toggleWidget(widget, rv$steps.enabled['MECImputation'] )
    # })
    
    observeEvent(req(rv.custom$tmp.mec()$value), {
      # Do some stuff
      #req(rv.custom$tmp.mec()$value)
      rv.custom$dataIn2 <- rv.custom$tmp.mec()$value
      
      .history <- rv.custom$tmp.mec()$value[[length(rv.custom$tmp.mec()$value)]]
      rv.custom$params.tmp[['Imputation']][['MECImputation']] <- paramshistory(.history)
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- Timestamp()
      dataOut$value <- NULL
      rv$steps.status['MECImputation'] <- stepStatus$VALIDATED
    })
    
    # <<< END ------------- Code for step 2 UI---------------
    
    
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
        toggleWidget( 
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
      len_start <- length(rv$dataIn)
      len_end <- length(rv.custom$dataIn2)
      len_diff <- len_end - len_start
      
      req(len_diff > 0)
      
      if (len_diff == 2)
        rv.custom$dataIn2 <- QFeatures::removeAssay(rv.custom$dataIn2, 
          length(rv.custom$dataIn2) - 1)
      
      
      # Rename the new dataset with the name of the process
      i <- length(rv.custom$dataIn2)
      names(rv.custom$dataIn2)[i] <- 'Imputation'
      DaparToolshed::paramshistory(rv.custom$dataIn2[[i]]) <- 
        rv.custom$params.tmp
      
      
      # DO NOT MODIFY THE THREE FOLLOWING LINES
      dataOut$trigger <- Timestamp()
      dataOut$value <- rv.custom$dataIn2
      rv$steps.status['Save'] <- stepStatus$VALIDATED
      download_dataset_server('createQuickLink', 
        data = reactive({rv.custom$dataIn2}))
      
    })
    # <<< END ------------- Code for step 3 UI---------------
    
    
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = Module_Return_Func()))
  }
  )
}
