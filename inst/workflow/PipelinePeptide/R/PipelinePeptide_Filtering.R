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
#' In this example, `PipelinePeptide_Filtering_ui()` and `PipelinePeptide_Filtering_server()` define
#' the code for the process `PipelinePeptide` which is part of the pipeline called `PipelinePeptide`.
#'
#' @name PipelinePeptide
#' 
#' @param id xxx
#' @param dataIn The dataset
#' @param steps.enabled A vector of boolean which has the same length of the steps
#' of the pipeline. This information is used to enable/disable the widgets. It is not
#' a communication variable between the caller and this module, thus there is no
#' corresponding output variable
#' @param remoteReset It is a remote command to reset the module. A boolean that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#' @param steps.status xxx
#' @param current.pos xxx
#' @param path xxx
#' 
#' 
#' @examples
#' \dontrun{
#' library(MagellanNTK)
#' data(Exp1_R25_prot, package = 'DaparToolshedData')
#' path <- system.file('workflow/PipelinePeptide', package = 'Prostar2')
#' shiny::runApp(workflowApp("PipelinePeptide_Filtering", path, dataIn = Exp1_R25_prot))
#' }
#' 
#' 
#' @author Samuel Wieczorek
#' 
#' 
NULL

#' @rdname PipelinePeptide
#' @export
#' 
PipelinePeptide_Filtering_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelinePeptide_Filtering',
    mode = 'process',
    steps = c("Cell metadata filtering", "Variable filtering"),
    mandatory = c(FALSE, FALSE)
  )
}


#' @rdname PipelinePeptide
#' 
#' @export
#'
PipelinePeptide_Filtering_ui <- function(id){
  ns <- NS(id)
}



#' @rdname PipelinePeptide
#' 
#' @importFrom stats setNames rnorm
#' 
#' @export
#' 
PipelinePeptide_Filtering_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
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
    deleted.stringBased = NULL,
    deleted.metacell = NULL,
    deleted.numeric = NULL,
    tmp.filtering1 = reactive({NULL}),
    tmp.filtering2 = reactive({NULL}),
    history = list()
  )
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
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
    
    
    # observeEvent(remoteReset(), {
    #   browser()
    # })
    
    output$Description <- renderUI({
      # file <- normalizePath(file.path(session$userData$workflow.path, 
      #   'md', paste0(id, '.md')))
      
      file <- normalizePath(file.path(
        system.file('workflow', package = 'Prostar2'),
        unlist(strsplit(id, '_'))[1], 
        'md', 
        paste0(id, '.md')))
      
      tagList(
        # Insert validation button
        uiOutput(ns('Description_btn_validate_ui')),
        
        ### In this example, the md file is found in the extdata/module_examples 
        ### directory but with a real app, it should be provided by the package 
        ### which contains the UI for the different steps of the process module.
        ### system.file(xxx)
        
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
      req(dataIn())
      rv$dataIn <- dataIn()
      rv.custom$dataIn1 <- dataIn()
      rv.custom$dataIn2 <- dataIn()
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- stepStatus$VALIDATED
    })
    
    
    
    # >>>
    # >>> START ------------- Code for step 1 UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI ------------------------------------
    output$Cellmetadatafiltering <- renderUI({
      wellPanel(
        # uiOutput for all widgets in this UI
        # This part is mandatory
        # The renderUI() function of each widget is managed by MagellanNTK
        # The dev only have to define a reactive() function for each
        # widget he want to insert
        # Be aware of the naming convention for ids in uiOutput()
        # For more details, please refer to the dev document.
        # Insert validation button
        uiOutput(ns("Cellmetadatafiltering_btn_validate_ui")),
        uiOutput(ns("mod_metacell_filtering_ui"))
        
      )
    })
    
    
    observe({
      print('tututut')
      print(remoteReset())
      
      rv.custom$tmp.filtering1 <- Prostar2::mod_Metacell_Filtering_server(
        id = "metaFiltering",
        obj = reactive({rv$dataIn}),
        i = reactive({length(rv$dataIn)}),
        is.enabled = reactive({rv$steps.enabled["Cellmetadatafiltering"]}),
        remoteReset = reactive({remoteReset()})
      )
    })
    
    output$mod_metacell_filtering_ui <- renderUI({
      widget <- Prostar2::mod_Metacell_Filtering_ui(ns("metaFiltering"))
      MagellanNTK::toggleWidget(widget, 
        rv$steps.enabled["Cellmetadatafiltering"])
    })
    

    
    output$Cellmetadatafiltering_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Cellmetadatafiltering_btn_validate"),
        "Validate step",
        class = "btn-success"
      )
       MagellanNTK::toggleWidget(widget, 
        rv$steps.enabled["Cellmetadatafiltering"])
    })
    # >>> END: Definition of the widgets
    
    
    
    observeEvent(input$Cellmetadatafiltering_btn_validate, {
      
      req(rv.custom$tmp.filtering1()$value)

      rv.custom$dataIn1 <- rv.custom$tmp.filtering1()$value
      rv.custom$dataIn2 <- rv.custom$tmp.filtering1()$value
    
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status["Cellmetadatafiltering"] <- stepStatus$VALIDATED
    })
    

    # <<< END ------------- Code for step 1 UI---------------
    
    
    # >>> START ------------- Code for step 2 UI---------------
    
    output$Variablefiltering <- renderUI({
      wellPanel(
        # Process the queries
        uiOutput(ns("Variablefiltering_btn_validate_ui")),
        uiOutput(ns("mod_variable_filtering_ui"))
      )
    })
    

    observe({
      # # If the previous step has been run and validated,
      # # Update dataIn to its result

      rv.custom$tmp.filtering2 <- Prostar2::mod_Variable_Filtering_server(
        id = "varFiltering",
        obj = reactive({rv.custom$dataIn1}),
        i = reactive({length(rv.custom$dataIn1)}),
        is.enabled = reactive({rv$steps.enabled["Variablefiltering"]}),
        remoteReset = reactive({remoteReset()})
      )

    })
    
    
    output$mod_variable_filtering_ui <- renderUI({
      
      
    widget <- Prostar2::mod_Variable_Filtering_ui(ns("varFiltering"))
    MagellanNTK::toggleWidget(widget, 
      rv$steps.enabled["Variablefiltering"])
  })
  

    output$Variablefiltering_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Variablefiltering_btn_validate"),
        "Validate step",
        class = "btn-success"
      )
      MagellanNTK::toggleWidget( widget, rv$steps.enabled["Variablefiltering"])
    })

    
    
    observeEvent(input$Variablefiltering_btn_validate, {
      req(rv.custom$tmp.filtering2()$value)
      rv.custom$dataIn2 <- rv.custom$tmp.filtering2()$value
      

      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status["Variablefiltering"] <- stepStatus$VALIDATED
    })
    
    # <<< END ------------- Code for step 2 UI---------------
    
    
    # >>> START ------------- Code for step 'Save' UI---------------
    output$Save <- renderUI({
      tagList(
        # Insert validation button
        # This line is necessary. DO NOT MODIFY
        uiOutput(ns('Save_btn_validate_ui')),
        uiOutput(ns('dl_ui'))
      )
    })
    
    output$dl_ui <- renderUI({
      req(rv$steps.status['Save'] == stepStatus$VALIDATED)
      req(config@mode == 'process')
      
      MagellanNTK::download_dataset_ui(ns('createQuickLink'))
    })
    
    output$Save_btn_validate_ui <- renderUI({
      MagellanNTK::toggleWidget(
        actionButton(ns("Save_btn_validate"), "Save",
                     class = "btn-success"),
        rv$steps.enabled['Save']
        )
    })
    
    observeEvent(input$Save_btn_validate, {
      # Do some stuff
       # Clean the result
      len_start <- length(rv$dataIn)
      len_end <- length(rv.custom$dataIn2)
      len_diff <- len_end - len_start
      

      req(len_diff > 0)
      
      if (len_diff == 2)
        rv.custom$dataIn2 <- QFeatures::removeAssay(rv.custom$dataIn2, 
          length(rv.custom$dataIn2)-1)
      
      
      # Rename the new dataset with the name of the process
        names(rv.custom$dataIn2)[length(rv.custom$dataIn2)] <- 'Filtering'
        DaparToolshed::paramshistory(rv.custom$dataIn2[[length(rv.custom$dataIn2)]]) <- 
          c(DaparToolshed::paramshistory(rv.custom$dataIn2[[length(rv.custom$dataIn2)]]), 
            reactiveValuesToList(rv.widgets))
        
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv.custom$dataIn2
      rv$steps.status['Save'] <- stepStatus$VALIDATED

      Prostar2::download_dataset_server('createQuickLink', 
        dataIn = reactive({rv.custom$dataIn2}))
    })
    # <<< END ------------- Code for step 3 UI---------------
    
    
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  }
  )
}
