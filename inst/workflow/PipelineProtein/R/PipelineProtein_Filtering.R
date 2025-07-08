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
#' In this example, `PipelineProtein_Filtering_ui()` and `PipelineProtein_Filtering_server()` define
#' the code for the process `PipelineProtein` which is part of the pipeline called `PipelineProtein`.
#'
#' @name PipelineProtein
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
#' path <- system.file('workflow/PipelineProtein', package = 'Prostar2')
#' shiny::runApp(workflowApp("PipelineProtein_Filtering", path, dataIn = Exp1_R25_prot))
#' }
#' 
#' 
#' @author Samuel Wieczorek
#' 
#' 
NULL

#' @rdname PipelineProtein
#' @export
#' 
PipelineProtein_Filtering_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelineProtein_Filtering',
    mode = 'process',
    steps = c("Cell metadata filtering", "Variable filtering"),
    mandatory = c(FALSE, FALSE)
  )
}


#' @rdname PipelineProtein
#' 
#' @export
#'
PipelineProtein_Filtering_ui <- function(id){
  ns <- NS(id)
}



#' @rdname PipelineProtein
#' 
#' @importFrom stats setNames rnorm
#' 
#' @export
#' 
PipelineProtein_Filtering_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  path = NULL
){
  
  
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list(
    Cellmetadatafiltering_tag = "None",
    Cellmetadatafiltering_scope = "None",
    Cellmetadatafiltering_keep_vs_remove = "delete",
    Cellmetadatafiltering_valueTh = 0,
    Cellmetadatafiltering_percentTh = 0,
    Cellmetadatafiltering_valPercent = "Count",
    Cellmetadatafiltering_operator = "None")
  
  
  rv.custom.default.values <- list(
    dataIn1 = NULL,
    dataIn2 = NULL,
    deleted.stringBased = NULL,
    deleted.metacell = NULL,
    deleted.numeric = NULL,
    tmp.filtering1 = reactive({NULL}),
    tmp.filtering2 = reactive({NULL}),
    history = list(),
    
    indices = NULL,
    functionFilter = NULL,
    query = list(),
    fun.list = list(),
    widgets.value = list(),
    tmp.tags = reactive({NULL})
  )
  
  GetFiltersScope <- function()
    c("Whole Line" = "WholeLine",
      "Whole matrix" = "WholeMatrix",
      "For every condition" = "AllCond",
      "At least one condition" = "AtLeastOneCond"
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
    
    
    # core <- paste0(
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
        # Insert validation button
        uiOutput(ns('Description_btn_validate_ui')),
        
        # # Used to show some information about the dataset which is loaded
        # # This function must be provided by the package of the process module
        uiOutput(ns('datasetDescription_ui')),
        # 
        if (file.exists(file)){
          htmltools::includeMarkdown(file)
        } else
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
      
      
      #####################################################"
      shinyjs::useShinyjs()
      path <- file.path(system.file('www/css', package = 'MagellanNTK'),'MagellanNTK.css')
      includeCSS(path)
      
      .localStyle <- "display:inline-block; vertical-align: top; padding-right: 20px;"
      
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          id = ns('Cellmetadatafiltering_Sidebar'),
          timeline_process_ui(ns('Cellmetadatafiltering_timeline')),
          hr(style = "border-top: 3px solid #000000;"),
          tags$style(".shiny-input-panel {background-color: lightblue;}"),
          uiOutput(ns("Cellmetadatafiltering_btn_validate_ui")),
          inputPanel(
            tags$div(
              tags$div(style = .localStyle, uiOutput(ns("Cellmetadatafiltering_tree_UI"))),
              tags$div(style = .localStyle, uiOutput(ns("Cellmetadatafiltering_chooseKeepRemove_ui"))),
              tags$div(style = .localStyle, uiOutput(ns("Cellmetadatafiltering_chooseScope_ui"))),
              tags$div(style = .localStyle, uiOutput(ns("Cellmetadatafiltering_qMetacellScope_widgets_set2_ui")))
            )
          ),
          width = 200,
          position = "left",
          bg='lightblue',
          padding = c(100, 0), # 1ere valeur : padding vertical, 2eme : horizontal
          style = "z-index: 0;"
        ),
        div(
          style = "display:inline-block; 
                vertical-align: middle; align: center;",
          uiOutput(ns("qMetacellScope_request_ui"))
        ),
        uiOutput(ns('Add_btn_UI')),
        uiOutput(ns("qMetacell_Filter_DT")),
        uiOutput(ns('plots_ui'))
      )
      #############################################
    })
    
    
    
    output$Cellmetadatafiltering_Add_btn_UI <- renderUI({
      widget <- actionButton(ns("Cellmetadatafiltering_BuildFilter_btn"), "Add filter",
        class = "btn-info")
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Cellmetadatafiltering"])
    })
    
    MagellanNTK::mod_popover_for_help_server("tag_help",
      title = "Nature of data to filter",
      content = "Define xxx"
    )
    
    
    help.txt1 <- "To filter the missing values, the choice of the lines to 
        be kept is made by different options:
    <ul>
    <li><strong>None</strong>: No filtering, the quantitative data is left 
    unchanged.</li>
    <li><strong>(Remove) Empty lines</strong>: All the lines with 100% of 
    missing values are filtered out.</li>
    <li><strong>Whole Matrix</strong>: The lines (across all conditions) 
    which contain less quantitative value than a user-defined threshold are 
    kept;</li>
    <li><strong>For every condition</strong>: The lines for which each 
    condition contain less quantitative value than a user-defined threshold 
    are deleted;</li>
    <li><strong>At least one condition</strong>: The lines for which at least 
    one condition contain less quantitative value than a user-defined 
    threshold are deleted.</li>
    </ul>"
    
    MagellanNTK::mod_popover_for_help_server("filterScope_help",
      title = "Scope",
      content = HTML(help.txt1)
    )
    
    
    
    observeEvent(req(remoteReset()), ignoreInit = FALSE, {
      rv$dataIn <- dataIn()
      req(rv$dataIn)
      rv.custom$qMetacell_Filter_SummaryDT <- data.frame(
        query = "-",
        nbDeleted = "0",
        TotalMainAssay = nrow(rv$dataIn[[length(rv$dataIn)]]),
        stringsAsFactors = FALSE
      )
    })
    
    observeEvent(req(dataIn()), ignoreInit = FALSE, ignoreInit = FALSE, {
      stopifnot(inherits(dataIn(), 'QFeatures'))
      rv$dataIn <- dataIn()
      rv.custom$qMetacell_Filter_SummaryDT <- data.frame(
        query = "-",
        nbDeleted = "0",
        TotalMainAssay = nrow(rv$dataIn[[length(rv$dataIn)]]),
        stringsAsFactors = FALSE
      )
    }, priority = 1000)
    
    
    output$tree_UI <- renderUI({
      widget <- div(
        mod_metacell_tree_ui(ns('tree')))
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    #rv.custom$tmp.tags <- reactive({NULL})
    rv.custom$tmp.tags <- mod_metacell_tree_server('tree',
      dataIn = reactive({rv$dataIn}),
      remoteReset = reactive({remoteReset()}),
      is.enabled = reactive({rv$steps.enabled["Cellmetadatafiltering"]})
    )
    
    observeEvent(rv.custom$tmp.tags()$trigger, ignoreInit = FALSE, {
      
      rv.widgets$tag <- rv.custom$tmp.tags()$values
      dataOut$trigger <- as.numeric(Sys.time())
      dataOut$value <- list(
        ll.fun = NULL,
        ll.query = NULL,
        ll.widgets.value = NULL,
        ll.pattern = rv.widgets$tag,
        ll.indices = NULL
      )
    })
    
    
    
    output$qMetacellScope_widgets_set2_ui <- renderUI({
      req(!(rv.widgets$Cellmetadatafiltering_scope %in% c("None", "WholeLine")))
      req(rv.widgets$Cellmetadatafiltering_tag != "None")
      
      MagellanNTK::mod_popover_for_help_server("chooseValPercent_help",
        title = paste("#/% of values to ", rv.widgets$Cellmetadatafiltering_keep_vs_remove),
        content = "Define xxx"
      )
      
      widget1 <- radioButtons(ns("Cellmetadatafiltering_valPercent"),
        MagellanNTK::mod_popover_for_help_ui(ns("chooseValPercent_help")),
        choices = val_vs_percent(),
        selected = rv.widgets$Cellmetadatafiltering_valPercent
      )
      
      widget2 <- selectInput(ns("Cellmetadatafiltering_operator"),
        "Choose operator",
        choices = operator(),
        selected = rv.widgets$Cellmetadatafiltering_operator,
        width = "100px"
      )
      
      
      tagList(
        fluidRow(
          column(4, MagellanNTK::toggleWidget(widget1, rv$steps.enabled["Cellmetadatafiltering"])),
          column(8, 
            MagellanNTK::toggleWidget(widget2, rv$steps.enabled["Cellmetadatafiltering"]),
            uiOutput(ns("value_ui")),
            uiOutput(ns("percentage_ui"))
          )
        )
      )
    })
    
    
    output$value_ui <- renderUI({
      req(rv.widgets$Cellmetadatafiltering_valPercent == "Count")
      req(!(rv.widgets$Cellmetadatafiltering_scope %in% c("None", "WholeLine")))
      MagellanNTK::mod_popover_for_help_server("value_th_help",
        title = "Count threshold",
        content = "Define xxx"
      )
      
      widget <- selectInput(ns("Cellmetadatafiltering_valueTh"),
        MagellanNTK::mod_popover_for_help_ui(ns("value_th_help")),
        choices = getListNbValuesInLines(
          object = rv$dataIn,
          conds = conds(),
          type = rv.widgets$Cellmetadatafiltering_scope
        ),
        selected = rv.widgets$Cellmetadatafiltering_valueTh,
        width = "150px"
      )
      tagList(
        MagellanNTK::mod_popover_for_help_ui(ns("keepVal_help")),
        MagellanNTK::toggleWidget(widget, rv$steps.enabled["Cellmetadatafiltering"])
      )
    })
    
    output$percentage_ui <- renderUI({
      req(rv.widgets$Cellmetadatafiltering_valPercent == "Percentage")
      req(!(rv.widgets$Cellmetadatafiltering_scope %in% c("None", "WholeLine")))
      
      MagellanNTK::mod_popover_for_help_server("percentTh_help",
        title = "Percentage threshold",
        content = "Define xxx"
      )
      widget <- sliderInput(ns("Cellmetadatafiltering_percentTh"),
        MagellanNTK::mod_popover_for_help_ui(ns("percentTh_help")),
        min = 0,
        max = 100,
        step = 1,
        value = rv.widgets$Cellmetadatafiltering_percentTh,
        width = "250px"
      )
      tagList(
        MagellanNTK::mod_popover_for_help_ui(ns("keepVal_percent_help")),
        MagellanNTK::toggleWidget(widget, rv$steps.enabled["Cellmetadatafiltering"])
      )
    })
    
    
    
    WriteQuery <- reactive({
      txt_summary <- NULL
      if (rv.widgets$Cellmetadatafiltering_scope == "None") {
        txt_summary <- "No filtering is processed."
      } else if (rv.widgets$Cellmetadatafiltering_scope == "WholeLine") {
        
        txt_summary <- paste(
          rv.widgets$Cellmetadatafiltering_keep_vs_remove,
          "lines that contain only (",
          toString(rv.widgets$Cellmetadatafiltering_tag), ")"
        )
      } else {
        text_method <- switch(rv.widgets$Cellmetadatafiltering_scope,
          WholeMatrix = "the whole matrix.",
          AllCond = "each condition.",
          AtLeastOneCond = "at least one condition."
        )
        
        if (rv.widgets$Cellmetadatafiltering_valPercent == "Count") {
          text_threshold <- rv.widgets$Cellmetadatafiltering_valueTh
        } else {
          text_threshold <- paste(as.character(rv.widgets$Cellmetadatafiltering_percentTh), 
            " %", sep = "")
        }
        
        txt_summary <- paste(
          rv.widgets$Cellmetadatafiltering_keep_vs_remove,
          " lines where number of (",
          toString(rv.widgets$Cellmetadatafiltering_tag),
          ") data ",
          rv.widgets$Cellmetadatafiltering_operator,
          " ",
          text_threshold,
          " in ",
          text_method
        )
      }
      txt_summary
    })
    
    
    output$qMetacellFilter_request_ui <- renderUI({
      tags$p(paste("You are going to ", WriteQuery()),
        style = "font-size: small; text-align : center; color: purple;"
      )
    })
    
    
    # Set useless widgets to default values
    observeEvent(rv.widgets$Cellmetadatafiltering_scope == "WholeLine", {
      rv.widgets$Cellmetadatafiltering_percentThh <- 0
      rv.widgets$Cellmetadatafiltering_valueTh <- 0
      rv.widgets$Cellmetadatafiltering_valPercent <- "Percentage"
    }, priority = 1000)
    
    
    
    
    BuildFunctionFilter <- reactive({
      req(rv$dataIn)
      req(rv.widgets$Cellmetadatafiltering_tag != "None")
      req(rv.widgets$Cellmetadatafiltering_scope != "None")
      
      th <- switch(rv.widgets$Cellmetadatafiltering_valPercent,
        Percentage = rv.widgets$Cellmetadatafiltering_percentTh / 100,
        Count = as.integer(rv.widgets$Cellmetadatafiltering_valueTh)
      )
      
      ff <- switch(rv.widgets$Cellmetadatafiltering_scope,
        WholeLine = FunctionFilter("qMetacellWholeLine",
          cmd = rv.widgets$Cellmetadatafiltering_keep_vs_remove,
          pattern = rv.widgets$Cellmetadatafiltering_tag
        ),
        WholeMatrix = FunctionFilter("qMetacellWholeMatrix",
          cmd = rv.widgets$Cellmetadatafiltering_keep_vs_remove,
          pattern = rv.widgets$Cellmetadatafiltering_tag,
          percent = rv.widgets$Cellmetadatafiltering_valPercent,
          th = th,
          operator = rv.widgets$Cellmetadatafiltering_operator
        ),
        AllCond = FunctionFilter("qMetacellOnConditions",
          cmd = rv.widgets$Cellmetadatafiltering_keep_vs_remove,
          mode = rv.widgets$Cellmetadatafiltering_scope,
          pattern = rv.widgets$Cellmetadatafiltering_tag,
          conds = conds(),
          percent = rv.widgets$Cellmetadatafiltering_valPercent,
          th = th,
          operator = rv.widgets$Cellmetadatafiltering_operator
        ),
        AtLeastOneCond = FunctionFilter("qMetacellOnConditions",
          cmd = rv.widgets$Cellmetadatafiltering_keep_vs_remove,
          mode = rv.widgets$Cellmetadatafiltering_scope,
          pattern = rv.widgets$Cellmetadatafiltering_tag,
          conds = conds(),
          percent = rv.widgets$Cellmetadatafiltering_valPercent,
          th = th,
          operator = rv.widgets$Cellmetadatafiltering_operator
        )
      )
      ff
    })
    
    
    GuessIndices <- reactive({
      
    })
    
    observeEvent(input$BuildFilter_btn, ignoreInit = TRUE,{
      req(BuildFunctionFilter())
      req(WriteQuery())
      
      rv.custom$ll.fun <- list(BuildFunctionFilter())
      rv.custom$ll.query <- list(WriteQuery())
      rv.custom$ll.widgets.value <-  list(reactiveValuesToList(rv.widgets))
      
      # Append a new FunctionFilter to the list
      dataOut$trigger <- as.numeric(Sys.time())
      dataOut$value <- list(
        ll.fun = rv.custom$ll.fun,
        ll.query = rv.custom$ll.query,
        ll.widgets.value = rv.custom$ll.widgets.value,
        ll.pattern = rv.widgets$Cellmetadatafiltering_tag,
        ll.indices = GetIndices_FunFiltering(
          obj = rv$dataIn,
          conds = conds(), 
          level = DaparToolshed::typeDataset(rv$dataIn), 
          pattern = rv.custom$ll.fun[[1]]@params$pattern,
          type = rv.widgets$Cellmetadatafiltering_scope,
          percent = rv.custom$ll.fun[[1]]@params$percent, 
          op = rv.custom$ll.fun[[1]]@params$operator, 
          th = rv.custom$ll.fun[[1]]@params$th)
      )
      
    })
    
    
    output$plots_ui <- renderUI({
      req(rv.custom$funFilter()$value$ll.pattern)
      
      mod_ds_metacell_Histos_server(
        id = "plots",
        dataIn = reactive({rv$dataIn[[length(rv$dataIn)]]}),
        pattern = reactive({rv.custom$funFilter()$value$ll.pattern}),
        group = reactive({omXplore::get_group(rv$dataIn)})
      )
      
      
      widget <- mod_ds_metacell_Histos_ui(ns("plots"))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Cellmetadatafiltering"])
    })
    
    
    MagellanNTK::format_DT_server("dt", 
      dataIn = reactive({rv.custom$qMetacell_Filter_SummaryDT}))
    
    
    output$qMetacell_Filter_DT <- renderUI({
      req(rv.custom$qMetacell_Filter_SummaryDT)
      MagellanNTK::format_DT_ui(ns("dt"))
    })
    
    
    
    
    
    
    observe({
      
      rv.custom$tmp.filtering1 <- Prostar2::mod_Metacell_Filtering_server(
        id = "metaFiltering",
        dataIn = reactive({rv$dataIn}),
        i = reactive({length(rv$dataIn)}),
        is.enabled = reactive({rv$steps.enabled["Cellmetadatafiltering"]}),
        remoteReset = reactive({remoteReset()})
      )
    })
    
    output$mod_metacell_filtering_ui <- renderUI({
      widget <- Prostar2::mod_Metacell_Filtering_ui(ns("metaFiltering"))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Cellmetadatafiltering"])
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
      rv.custom$tmp.filtering2 <- Prostar2::mod_Variable_Filtering_server(
        id = "varFiltering",
        dataIn = reactive({rv.custom$dataIn1}),
        i = reactive({length(rv.custom$dataIn1)}),
        is.enabled = reactive({rv$steps.enabled["Variablefiltering"]}),
        remoteReset = reactive({remoteReset()})
      )
    })
    
    
    output$mod_variable_filtering_ui <- renderUI({
      widget <- Prostar2::mod_Variable_Filtering_ui(ns("varFiltering"))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Variablefiltering"])
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
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          id = ns('Save_Sidebar'),
          timeline_process_ui(ns('Save_timeline')),
          tags$style(".shiny-input-panel {background-color: lightblue;}"),
          hr(style = "border-top: 3px solid #000000;"),
          inputPanel(
            uiOutput(ns('Save_btn_validate_ui'))
          ),
          width = 200,
          position = "left",
          bg='lightblue',
          padding = c(100, 0) # 1ere valeur : padding vertical, 2eme : horizontal
          #style = "p1"
        ),
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
