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
#' In this example, `PipelineProtein_Normalization_ui()` and `PipelineProtein_Normalization_server()` define
#' the code for the process `ProcessProtein` which is part of the pipeline called `PipelineProtein`.
#' 
#' @example inst/workflow/PipelineProtein/examples/example_PipelineProtein_Normalization.R
#' 
NULL

#' @rdname PipelineProtein
#' @export
#' 
PipelineProtein_Normalization_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelineProtein_Normalization',
    mode = 'process',
    steps = c('Normalization'),
    mandatory = c(FALSE)
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
PipelineProtein_Normalization_ui <- function(id){
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
#' @rdname PipelineProtein
#' 
#' @importFrom stats setNames rnorm
#' 
#' @export
#' 
PipelineProtein_Normalization_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1})
){
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list()
  
  rv.custom.default.values <- list(
    history = NULL,
    tmp.norm = reactive({NULL})
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

    # core <- paste0(
    #   MagellanNTK::Insert_Call_to_Config(id),
    #   MagellanNTK::Get_Code_Declare_widgets(names(widgets.default.values)),
    #   MagellanNTK::Get_Code_for_ObserveEvent_widgets(names(widgets.default.values)),
    #   MagellanNTK::Get_Code_for_rv_reactiveValues(),
    #   MagellanNTK::Get_Code_Declare_rv_custom(names(rv.custom.default.values)),
    #   MagellanNTK::Get_Code_for_dataOut(),
    #   MagellanNTK::Get_Code_for_remoteReset(widgets = TRUE,
    #     custom = TRUE,
    #     dataIn = 'dataIn()'),
    #   sep = "\n"
    # )
    # 
    # eval(str2expression(core))
    
    timeline_process_server(
      id = 'Description_timeline',
      config = PipelineProtein_Normalization_conf(),
      status = reactive({steps.status()}),
      position = reactive({current.pos()}),
      enabled = reactive({steps.enabled()})
    )
    
    
    
    timeline_process_server(
      id = 'Normalization_timeline',
      config = PipelineProtein_Normalization_conf(),
      status = reactive({steps.status()}),
      position = reactive({current.pos()}),
      enabled = reactive({steps.enabled()})
    )

    
    timeline_process_server(
      id = 'Save_timeline',
      config = PipelineProtein_Normalization_conf(),
      status = reactive({steps.status()}),
      position = reactive({current.pos()}),
      enabled = reactive({steps.enabled()})
    )
    
    
    observeEvent(input$Description_Sidebar, ignoreNULL = TRUE, {
      dataOut$sidebarState <- input$Description_Sidebar
    })
    
    observeEvent(input$Normalization_Sidebar, ignoreNULL = TRUE, {
      dataOut$sidebarState <- input$Normalization_Sidebar
    })
    
    
    observeEvent(input$Save_Sidebar, ignoreNULL = TRUE, {
      dataOut$sidebarState <- input$Save_Sidebar
    })
    
    # >>>
    # >>> START ------------- Code for Description UI---------------
    # >>> 
    
    
    output$Description <- renderUI({
      file <- normalizePath(file.path(
        system.file('workflow', package = 'Prostar2'),
        unlist(strsplit(id, '_'))[1], 
        'md', 
        paste0(id, '.md')))
     
      
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          id = ns("Description_Sidebar"),  # Add an explicit ID
          tags$style(".shiny-input-panel {background-color: lightblue;}"),
          
          timeline_process_ui(ns('Description_timeline')),
          
          inputPanel(
            uiOutput(ns('Description_btn_validate_ui'))
          ),
          width = 200,
          position = "left",
          bg='lightblue',
          padding = c(100, 0) # 1ere valeur : padding vertical, 2eme : horizontal
          #style = "p1"
        ),
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
    
    
    observeEvent(input$Description_btn_validate, ignoreInit = FALSE, {
      req(dataIn())
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
      path <- file.path(system.file('www/css', package = 'MagellanNTK'),'MagellanNTK.css')
      includeCSS(path)
      
      .style <- "display:inline-block; vertical-align: middle; 
      padding-right: 20px;"
      # 
      # wellPanel(
      #   # uiOutput for all widgets in this UI
      #   # This part is mandatory
      #   # The renderUI() function of each widget is managed by MagellanNTK
      #   # The dev only have to define a reactive() function for each
      #   # widget he want to insert
      #   # Be aware of the naming convention for ids in uiOutput()
      #   # For more details, please refer to the dev document.
      #   
      #   uiOutput(ns("Prot_Normalization_ui"))
      # )
      
      .localStyle <- "display:inline-block; vertical-align: top;
                  padding-right: 20px;"
      
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          id = ns('POVImputation_Sidebar'),
          timeline_process_ui(ns('POVImputation_timeline')),
          hr(style = "border-top: 3px solid #000000;"),
          tags$style(".shiny-input-panel {background-color: lightblue;}"),
          uiOutput(ns("POVImputation_btn_validate_ui")),
          inputPanel(
            tags$div(
              tags$div(style = .localStyle, uiOutput(ns("POVImputation_algorithm_UI"))),
              tags$div(style = .localStyle, uiOutput(ns("POVImputation_KNN_nbNeighbors_UI"))),
              tags$div(style = .localStyle, uiOutput(ns("POVImputation_detQuant_UI")))
            )
          ),
          width = 200,
          position = "left",
          bg='lightblue',
          padding = c(100, 0), # 1ere valeur : padding vertical, 2eme : horizontal
          style = "z-index: 0;"
        ),
        uiOutput(ns("POVImputation_showDetQuantValues")),
        htmlOutput("helpForImputation"),
        tags$hr(),
        uiOutput(ns('mvplots_ui'))
      )
      
      
      
      
    })
    
    observe({
      rv.custom$tmp.norm <- Prostar2::mod_Prot_Normalization_server(
        id = 'norm',
        dataIn = reactive({rv$dataIn}),
        i = reactive({length(rv$dataIn)}),
        is.enabled = reactive({rv$steps.enabled["Normalization"]}),
        remoteReset = reactive({remoteReset()})
      )
    })
    
    output$Prot_Normalization_ui <- renderUI({
      widget <- Prostar2::mod_Prot_Normalization_ui(ns('norm'))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Normalization'] )
    })
    
    
    observeEvent(rv.custom$tmp.norm()$value, {
      # Do some stuff
      # Do some stuff
      rv$dataIn <- rv.custom$tmp.norm()$value
      
      #.history <- rv.custom$tmp.norm()$value[[length(rv.custom$tmp.norm()$value)]]
      #rv.custom$params.tmp[['Normalization']] <- paramshistory(.history)
      
      # DO NOT MODIFY THE THREE FOLLOWING LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status['Normalization'] <- stepStatus$VALIDATED
    })
    
    
    # >>> START ------------- Code for step 3 UI---------------
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
