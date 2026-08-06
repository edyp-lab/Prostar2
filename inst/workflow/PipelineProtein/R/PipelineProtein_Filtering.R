#' @title PipelineProtein Filtering module
#'
#' @description
#' This module contains the filtering step of the protein pipeline.
#' 
#' @param id A `character(1)` which is the 'id' of the module.
#'
#' @param dataIn An instance of the class `MultiAssayExperiment`
#'
#' @param steps.enabled A vector of boolean which has the same length of the steps
#' of the pipeline. This information is used to enable/disable the widgets. It is not
#' a communication variable between the caller and this module, thus there is no
#' corresponding output variable
#'
#' @param remoteReset It is a remote command to reset the module. An `integer()` that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#'
#' @param steps.status A vector of `character()` which indicates the status of each step
#' which can be either 'validated', 'undone' or 'skipped'. Enabled or disabled in the UI.
#' 
#' @param current.pos A `integer(1)` which acts as a remote command to make
#'  a step active in the timeline. Default is 1.
#'  
#' @param path A `character()` which is the path to the directory which 
#' contains the files and directories of the pipeline.
#' 
#' @examples
#' if (interactive()){
#'   Prostar2("PipelineProtein_Filtering")
#' }
#' 
#' @name PipelineProtein_Filtering
#' 
#' @importFrom stats setNames rnorm
#' @importFrom shinyFeedback showFeedbackWarning hideFeedback
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' 
#' @return An instance of the class `MultiAssayExperiment`
#' 
NULL


#' @rdname PipelineProtein_Filtering
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


#' @rdname PipelineProtein_Filtering
#' @export
#'
PipelineProtein_Filtering_ui <- function(id){
  ns <- NS(id)
}


#' @rdname PipelineProtein_Filtering
#' @export
#' 
PipelineProtein_Filtering_server <- function(id,
                                             dataIn = reactive({NULL}),
                                             steps.enabled = reactive({NULL}),
                                             remoteReset = reactive({0}),
                                             steps.status = reactive({NULL}),
                                             current.pos = reactive({1}),
                                             path = NULL,
                                             btnEvents = reactive({NULL})
){
  
  pkgs_require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list(
    Cellmetadatafiltering_tag = "None",
    Cellmetadatafiltering_scope = "None",
    Cellmetadatafiltering_keep_vs_remove = "delete",
    Cellmetadatafiltering_valueTh = 0,
    Cellmetadatafiltering_percentTh = 0,
    Cellmetadatafiltering_valPercent = "Count",
    Cellmetadatafiltering_operator = "None",
    
    
    Variablefiltering_cname = "None",
    Variablefiltering_value = NA,
    Variablefiltering_keep_vs_remove = "delete",
    Variablefiltering_operator = "None"
  )
  
  
  rv.custom.default.values <- list(
    result_open_dataset = reactive({NULL}),
    dataIn1 = NULL,
    dataIn2 = NULL,
    deleted.stringBased = NULL,
    deleted.metacell = NULL,
    deleted.numeric = NULL,
    tmp.filtering1 = reactive({NULL}),
    tmp.filtering2 = reactive({NULL}),
    
    indices = NULL,
    functionFilter = NULL,
    query = list(),
    fun.list = list(),
    widgets.value = list(),
    tmp.tags = reactive({NULL}),
    indices = NULL,
    Filtering = NULL,
    query = list(),
    fun.list = list(),
    widgets.value = list(),
    funFilter = reactive({NULL}),
    qMetacell_Filter_SummaryDT = data.frame(
      query = "-",
      nbDeleted = "0",
      TotalMainAssay = '0',
      stringsAsFactors = FALSE
    ), 
    df = data.frame(),
    history = MagellanNTK::InitializeHistory(),
    
    
    # Variable Filtering variables
    # indices = NULL,
    Variablefiltering_query = list(),
    Variablefiltering_widgets.value = list(),
    Variablefiltering_variable_Filter_SummaryDT = data.frame(
      Variablefiltering_query = NA,
      Variablefiltering_nbDeleted = NA,
      Variablefiltering_TotalMainAssay = NA,
      stringsAsFactors = FALSE
    ),
    
    Variablefiltering_ll.var = list(),
    Variablefiltering_ll.query = list(),
    Variablefiltering_ll.widgets.value = list(),
    wrongValueType = NULL
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
    add_resourcePath()
    
    
    ###########################################################################-
    #
    #-----------------------------DESCRIPTION-----------------------------------
    #
    ###########################################################################-
    output$Description <- renderUI({
      file <- normalizePath(file.path(
        system.file('workflow', package = 'Prostar2'),
        unlist(strsplit(id, '_'))[1], 
        'md', 
        paste0(id, '.Rmd')))
      
      MagellanNTK::process_layout(session,
                                  ns = NS(id),
                                  sidebar = div(id = 'div_sidebar_Description',
                                                uiOutput(ns('open_dataset_UI'))
                                  ),
                                  content = div(id = ns('div_content'),
                                                if (file.exists(file))
                                                  includeMarkdown(file)
                                                else
                                                  p('No Description available')
                                                #uiOutput(ns('Description_infos_dataset_UI'))
                                  )
      )
    })
    
    #### _sidebar -----
    output$open_dataset_UI <- renderUI({
      req(session$userData$runmode == 'process')
      req(is.null(dataIn()))
      req(NULL)
      rv.custom$result_open_dataset <- MagellanNTK::open_dataset_server(
        id = "open_dataset",
        class = 'QFeatures',
        extension = "qf",
        remoteReset = reactive({remoteReset()})
      )
      
      MagellanNTK::open_dataset_ui(id = ns("open_dataset"))
    })
    
    #### _content -----
    # output$Description_infos_dataset_UI <- renderUI({
    #   req(rv$dataIn)
    #   
    #   infos_dataset_server(
    #     id = "Description_infosdataset",
    #     dataIn = reactive({rv$dataIn})
    #   )
    #   
    #   infos_dataset_ui(id = ns("Description_infosdataset"))
    # })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Description', btnEvents()))
      req(dataIn())
      req(inherits(dataIn(), 'QFeatures'))
      
      rv$dataIn <- dataIn()
      
      if(!is.null(rv.custom$result_open_dataset()$dataset))
        rv$dataIn <- rv.custom$result_open_dataset()$dataset
      
      rv.custom$dataIn1 <- rv$dataIn
      rv.custom$dataIn2 <- rv$dataIn
      
      rv.custom$qMetacell_Filter_SummaryDT <- data.frame(
        query = "-",
        nbDeleted = "0",
        TotalMainAssay = nrow(rv$dataIn[[length(rv$dataIn)]]),
        stringsAsFactors = FALSE
      )
      
      rv.custom$Variablefiltering_variable_Filter_SummaryDT <- data.frame(
        Variablefiltering_query = "-",
        Variablefiltering_nbDeleted = "0",
        Variablefiltering_TotalMainAssay = nrow(rv$dataIn[[length(rv$dataIn)]]),
        stringsAsFactors = FALSE
      )
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status['Description'] <- MagellanNTK::stepStatus$VALIDATED
    })
    
    
    ###########################################################################-
    #
    #------------------------CELL METADATA FILTERING----------------------------
    #
    ###########################################################################-
    output$Cellmetadatafiltering <- renderUI({
      MagellanNTK::process_layout(session,
                                  ns = NS(id),
                                  sidebar = tagList(
                                    uiOutput(ns("Cellmetadatafiltering_buildQuery_ui"))
                                  ),
                                  content = tagList(
                                    uiOutput(ns('qMetacell_Filter_DT_UI')),
                                    uiOutput(ns("Cellmetadatafiltering_qMetacell_Filter_DT")),
                                    uiOutput(ns('Cellmetadatafiltering_plots_ui'))
                                  )
      )
    })
    
    #### _sidebar -----
    observe({
      req(rv$steps.enabled["Cellmetadatafiltering"])
      req(rv.custom$dataIn1)
      
      rv.custom$funFilter <- mod_qMetacell_FunctionFilter_Generator_server(
        id = "query",
        dataIn = reactive({rv.custom$dataIn1[[length(rv.custom$dataIn1)]]}),
        conds = reactive({DaparToolshed::design_qf(rv.custom$dataIn1)$Condition}),
        keep_vs_remove = reactive({stats::setNames(c('Push p-value', 'Keep original p-value'), nm = c("delete", "keep"))}),
        val_vs_percent = reactive({stats::setNames(nm = c("Count", "Percentage"))}),
        operator = reactive({stats::setNames(nm = DaparToolshed::SymFilteringOperators())}),
        remoteReset = reactive({remoteReset()}),
        is.enabled = reactive({rv$steps.enabled["Cellmetadatafiltering"]})
      )
    })
    
    output$Cellmetadatafiltering_buildQuery_ui <- renderUI({
      
      widget <- mod_qMetacell_FunctionFilter_Generator_ui(ns("query"))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Cellmetadatafiltering"])
    })
    
    #### _content -----
    observeEvent(req(length(rv.custom$funFilter()$value$ll.fun) > 0), ignoreInit = TRUE,{
      req(rv.custom$dataIn1)
      
      tmp <- DaparToolshed::filterFeaturesOneSE(
        object = rv.custom$dataIn1,
        i = length(rv.custom$dataIn1),
        name = paste0("qMetacellFiltered", MagellanNTK::Timestamp()),
        filters = rv.custom$funFilter()$value$ll.fun
      )
      
      # Add infos
      nBefore <- nrow(tmp[[length(tmp) - 1]])
      nAfter <- nrow(tmp[[length(tmp)]])
      
      .html <- rv.custom$funFilter()$value$ll.query
      .nbDeleted <- nBefore - nAfter
      .nbRemaining <- nrow(SummarizedExperiment::assay(tmp[[length(tmp)]]))
      
      rv.custom$qMetacell_Filter_SummaryDT <- rbind(
        rv.custom$qMetacell_Filter_SummaryDT ,
        c(.html, .nbDeleted, .nbRemaining))
      
      # Keeps only the last filtered SE
      len_start <- length(dataIn())
      len_end <- length(tmp)
      len_diff <- len_end - len_start
      
      req(len_diff > 0)
      
      if (len_diff == 2)
        rv.custom$dataIn1 <- QFeatures::removeAssay(tmp, length(tmp)-1)
      else
        rv.custom$dataIn1 <- tmp
      
      # Rename the new dataset with the name of the process
      names(rv.custom$dataIn1)[length(rv.custom$dataIn1)] <- 'Cellmetadatafiltering'
      
      # Add params
      query <- rv.custom$funFilter()$value$ll.query
      
      rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Filtering', 'Cellmetadatafiltering', 'query', query)
      DaparToolshed::paramshistory(rv.custom$dataIn1[['Cellmetadatafiltering']]) <- rbind(DaparToolshed::paramshistory(rv.custom$dataIn1[['Cellmetadatafiltering']])
                                                                                          ,rv.custom$history)
    })
    
    output$Cellmetadatafiltering_plots_ui <- renderUI({
      req(rv.custom$funFilter()$value$ll.pattern)
      
      mod_ds_metacell_Histos_server(
        id = "plots",
        dataIn = reactive({rv.custom$dataIn1[[length(rv.custom$dataIn1)]]}),
        pattern = reactive({rv.custom$funFilter()$value$ll.pattern}),
        group = reactive({DaparToolshed::design_qf(rv.custom$dataIn1)$Condition}),
        pal = DaparToolshed::GetColorsForConditions(unique(DaparToolshed::design_qf(rv.custom$dataIn1)$Condition), 
                                                    DaparToolshed::ExtendPalette(length(unique(DaparToolshed::design_qf(rv.custom$dataIn1)$Condition))))
      )
      
      widget <- mod_ds_metacell_Histos_ui(ns("plots"))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Cellmetadatafiltering"])
    })
    
    MagellanNTK::format_DT_server("dt", 
                                  dataIn = reactive({rv.custom$qMetacell_Filter_SummaryDT}))
    
    output$qMetacell_Filter_DT_UI <- renderUI({
      req(rv.custom$qMetacell_Filter_SummaryDT)
      MagellanNTK::format_DT_ui(ns("dt"))
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Cellmetadatafiltering', btnEvents()))
      
      shiny::withProgress(message = paste0("Reseting process", id), {
        shiny::incProgress(0.5)
        
        if ( isTRUE(all.equal(SummarizedExperiment::assays(rv.custom$dataIn1),
                              SummarizedExperiment::assays(dataIn()))) 
             || !("Cellmetadatafiltering" %in% names(rv.custom$dataIn1)))
          shinyjs::info(btnVentsMasg)
        else {
          req(rv.custom$dataIn1)
          
          rv.custom$dataIn2 <- rv.custom$dataIn1
          
          rv.custom$Variablefiltering_variable_Filter_SummaryDT <- data.frame(
            Variablefiltering_query = "-",
            Variablefiltering_nbDeleted = "0",
            Variablefiltering_TotalMainAssay = nrow(rv.custom$dataIn2[[length(rv.custom$dataIn2)]]),
            stringsAsFactors = FALSE
          )
          
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["Cellmetadatafiltering"] <- MagellanNTK::stepStatus$VALIDATED
        }
        shiny::incProgress(1)
      })
    })
    
    
    ###########################################################################-
    #
    #--------------------------VARIABLE FILTERING-------------------------------
    #
    ###########################################################################-
    output$Variablefiltering <- renderUI({
      MagellanNTK::process_layout(session,
                                  ns = NS(id),
                                  sidebar = tagList(
                                    tags$style(HTML("
            .radio-inline {
              margin-right: 20px;  /* Adjust spacing between choices */
              margin-left: 10px;   /* Adjust spacing around the group */
              margin-bottom: -10px;
            }
          ")),
                                    uiOutput(ns("Variablefiltering_chooseKeepRemove_ui")),
                                    uiOutput(ns("Variablefiltering_cname_ui")),
                                    uiOutput(ns("Variablefiltering_operator_ui")),
                                    uiOutput(ns("Variablefiltering_value_ui")),
                                    uiOutput(ns("Variablefiltering_wrongValueType_ui")),
                                    # uiOutput(ns('Variablefiltering_Preview_UI')),
                                    uiOutput(ns("Variablefiltering_addFilter_btn_ui"))
                                  ),
                                  content = tagList(
                                    uiOutput(ns("Variablefiltering_DT_UI"))
                                  )
      )
      
    })
    
    #### _sidebar -----
    output$Variablefiltering_chooseKeepRemove_ui <- renderUI({
      req(rv.custom$dataIn2)
      
      widget <- radioButtons(ns("Variablefiltering_keep_vs_remove"),
                             "Type of filter operation",
                             choices = rv.widgets$Variablefiltering_keep_vs_remove,
                             selected = rv.widgets$Variablefiltering_keep_vs_remove
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Variablefiltering"])
    })
    
    output$Variablefiltering_cname_ui <- renderUI({
      req(rv.custom$dataIn2)
      
      .choices <- c("None", colnames(SummarizedExperiment::rowData(rv.custom$dataIn2[[length(rv.custom$dataIn2)]])))
      
      widget <- selectInput(ns("Variablefiltering_cname"),
                            "Column name",
                            choices = stats::setNames(.choices, nm = .choices),
                            selected = rv.widgets$Variablefiltering_cname,
                            width = "225px"
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Variablefiltering"])
    })
    
    output$Variablefiltering_operator_ui <- renderUI({
      req(rv.custom$dataIn2)
      req(rv.widgets$Variablefiltering_cname %in% colnames(SummarizedExperiment::rowData(rv.custom$dataIn2[[length(rv.custom$dataIn2)]])))
      
      if (is.numeric(SummarizedExperiment::rowData(rv.custom$dataIn2[[length(rv.custom$dataIn2)]])[, rv.widgets$Variablefiltering_cname])) {
        .operator <- DaparToolshed::SymFilteringOperators()
      } else {
        .operator <- c("==", "!=", "startsWith", "endsWith", "contains")
      }
      
      .operator <- c("None" = "None", .operator)
      
      widget <- selectInput(ns("Variablefiltering_operator"),
                            "Operator",
                            choices = stats::setNames(nm = .operator),
                            selected = rv.widgets$Variablefiltering_operator,
                            width = "125px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Variablefiltering"])
    })
    
    output$Variablefiltering_value_ui <- renderUI({
      req(rv.custom$dataIn2)
      req(rv.widgets$Variablefiltering_cname %in% colnames(SummarizedExperiment::rowData(rv.custom$dataIn2[[length(rv.custom$dataIn2)]])))
      
      widget <- textInput(ns("Variablefiltering_value"),
                          "Value",
                          placeholder = 'Enter value...',
                          width = "175px"
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Variablefiltering"])
    })
    
    output$Variablefiltering_wrongValueType_ui <- renderUI({
      req(rv.custom$wrongValueType)
      req(rv.widgets$Variablefiltering_value != "")
      p(style = "margin-top: -15px; font-weight: bold; color: red; font-size: 13px;", 
        "/!\\ Numeric value expected")
    })
    
    observeEvent(c(rv.widgets$Variablefiltering_value, rv.widgets$Variablefiltering_cname), ignoreInit = TRUE, {
      req(rv.custom$dataIn2)
      req(!is.null(rv.widgets$Variablefiltering_value))
      req(rv.widgets$Variablefiltering_cname != "None")
      
      if (is.numeric(SummarizedExperiment::rowData(rv.custom$dataIn2[[length(rv.custom$dataIn2)]])[, rv.widgets$Variablefiltering_cname]) ) {
        rv.custom$wrongValueType <- is.na(Extract_Value(rv.widgets$Variablefiltering_value, "numeric"))
      } else {
        rv.custom$wrongValueType <- is.na(Extract_Value(rv.widgets$Variablefiltering_value, "character"))
      }
      
      # if (is.na(Extract_Value(rv.widgets$Variablefiltering_value))) {
      #   shinyFeedback::showFeedbackWarning( 
      #     inputId = "Variablefiltering_value",
      #     text = "wrong type of value"
      #   )  
      # } else {
      #   shinyFeedback::hideFeedback("Variablefiltering_value")
      # }
    })
    
    output$Variablefiltering_addFilter_btn_ui <- renderUI({
      widget <- actionButton(ns("Variablefiltering_addFilter_btn"), "Add filter",
                             class = "btn-info")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Variablefiltering"])
    })
    
    Variablefiltering_BuildVariableFilter <- function(
    value = NULL,
    operator = NULL,
    cname = NULL,
    keep_vs_remove = NULL,
    data = NULL,
    i = NULL){
      req(value != "Enter value..." && !is.null(value))
      req(operator != "None" && !is.null(operator))
      req(cname != "None" && !is.null(cname))
      req(!is.null(keep_vs_remove))
      req(!is.null(data))
      
      if (is.null(i)){ 
        i <- length(data)
      }
      
      rowdata <- SummarizedExperiment::rowData(data[[i]])
      col_data <- rowdata[, cname, drop = TRUE]
      expected_type <- if (is.numeric(col_data)) "numeric" else "character"
      
      val <- tryCatch(
        Extract_Value(value, expected_type),
        warning = function(w) NULL,
        error = function(e) NULL
      )
      req(val)
      
      QFeatures::VariableFilter(
        field = cname,
        value = val,
        condition = operator,
        not = keep_vs_remove == "delete"
      )
    }
    
    Variablefiltering_WriteQuery <- function(
    value = NULL,
    operator = NULL,
    cname = NULL,
    keep_vs_remove = NULL,
    data = NULL,
    i = NULL){
      req(value != "Enter value..." && !is.null(value))
      req(operator != "None" && !is.null(operator))
      req(cname != "None" && !is.null(cname))
      req(!is.null(keep_vs_remove))
      req(!is.null(data))
      
      if (is.null(i)){ 
        i <- length(data)
      }
      
      rowdata <- SummarizedExperiment::rowData(data[[i]])
      col_data <- rowdata[, cname, drop = TRUE]
      expected_type <- if (is.numeric(col_data)) "numeric" else "character"
      
      val <- tryCatch(
        Extract_Value(value, expected_type),
        warning = function(w) NULL,
        error = function(e) NULL
      )
      req(val)
      
      query <- paste0(
        keep_vs_remove, " values for which ",
        cname, " ", operator, " ", value)
      query
    }
    
    #### _content -----
    observeEvent(input$Variablefiltering_addFilter_btn,
                 ignoreInit = TRUE, ignoreNULL = TRUE, {
                   req(rv.custom$dataIn2)
                   if ((rv.widgets$Variablefiltering_cname == "None") || 
                       (rv.widgets$Variablefiltering_operator == "None") || 
                       (rv.widgets$Variablefiltering_value == "") ||
                       rv.custom$wrongValueType) {
                     shinyjs::info(btnVentsMasg)
                     
                   } else {
                     req(rv.widgets$Variablefiltering_value)
                     req(rv.widgets$Variablefiltering_operator)
                     req(rv.widgets$Variablefiltering_cname)
                     
                     rv.custom$Variablefiltering_ll.var <- list(
                       Variablefiltering_BuildVariableFilter(
                         value = rv.widgets$Variablefiltering_value,
                         operator = rv.widgets$Variablefiltering_operator,
                         cname = rv.widgets$Variablefiltering_cname,
                         keep_vs_remove = rv.widgets$Variablefiltering_keep_vs_remove,
                         data = rv.custom$dataIn2)
                     )
                     
                     rv.custom$Variablefiltering_ll.query <- list(
                       Variablefiltering_WriteQuery(
                         value = rv.widgets$Variablefiltering_value,
                         operator = rv.widgets$Variablefiltering_operator,
                         cname = rv.widgets$Variablefiltering_cname,
                         keep_vs_remove = rv.widgets$Variablefiltering_keep_vs_remove,
                         data = rv.custom$dataIn2)
                     )
                     
                     rv.custom$Variablefiltering_ll.widgets.value <- reactiveValuesToList(rv.widgets)
                     ind <- grepl('Variablefiltering', names(rv.custom$Variablefiltering_ll.widgets.value))
                     ind <- which(ind == TRUE)
                     rv.custom$Variablefiltering_ll.widgets.value <- rv.custom$Variablefiltering_ll.widgets.value[ind]
                     
                     rv.custom$Variablefiltering_funFilter <- list(
                       ll.var = rv.custom$Variablefiltering_ll.var,
                       ll.query = rv.custom$Variablefiltering_ll.query,
                       ll.widgets.value = rv.custom$Variablefiltering_ll.widgets.value
                     )
                     
                     req(length(rv.custom$Variablefiltering_funFilter$ll.var) > 0)
                     
                     tmp <- DaparToolshed::filterFeaturesOneSE(
                       object = rv.custom$dataIn2,
                       i = length(rv.custom$dataIn2),
                       name = paste0("variableFiltered", MagellanNTK::Timestamp()),
                       filters = rv.custom$Variablefiltering_funFilter$ll.var
                     )
                     
                     # Add infos
                     nBefore <- nrow(tmp[[length(tmp) - 1]])
                     nAfter <- nrow(tmp[[length(tmp)]])
                     
                     .html <- rv.custom$Variablefiltering_funFilter$ll.query
                     .nbDeleted <- nBefore - nAfter
                     .nbBefore <- nrow(SummarizedExperiment::assay(rv.custom$dataIn2[[length(rv.custom$dataIn2)]]))
                     .nbAfter <- nrow(SummarizedExperiment::assay(tmp[[length(tmp)]]))
                     
                     rv.custom$Variablefiltering_variable_Filter_SummaryDT <- rbind(
                       rv.custom$Variablefiltering_variable_Filter_SummaryDT , 
                       c(.html, .nbDeleted, .nbAfter))
                     
                     # Keeps only the last filtered SE
                     len_start <- length(dataIn())
                     len_end <- length(tmp)
                     len_diff <- len_end - len_start
                     
                     req(len_diff > 0)
                     
                     if (len_diff == 2)
                       rv.custom$dataIn2 <- QFeatures::removeAssay(tmp, length(tmp)-1)
                     else 
                       rv.custom$dataIn2 <- tmp
                     
                     # Rename the new dataset with the name of the process
                     names(rv.custom$dataIn2)[length(rv.custom$dataIn2)] <- 'Variablefiltering'
                     
                     query <- rv.custom$Variablefiltering_funFilter$ll.query
                     i <- length(rv.custom$dataIn2)
                     rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Filtering', 'Variablefiltering', 'query', rv.custom$Variablefiltering_ll.query)
                     
                     DaparToolshed::paramshistory(rv.custom$dataIn2[['Variablefiltering']]) <- rbind(DaparToolshed::paramshistory(rv.custom$dataIn2[['Variablefiltering']]),
                                                                                                     rv.custom$history)
                   }
                 })
    
    output$Variablefiltering_DT_UI <- renderUI({
      MagellanNTK::format_DT_server("Variablefiltering_dt", 
                                    dataIn = reactive({rv.custom$Variablefiltering_variable_Filter_SummaryDT}))
      
      MagellanNTK::format_DT_ui(ns("Variablefiltering_dt"))
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Variablefiltering', btnEvents()))
      
      shiny::withProgress(message = paste0("Reseting process", id), {
        shiny::incProgress(0.5)
        
        if (isTRUE(all.equal(SummarizedExperiment::assays(rv.custom$dataIn2),
                             SummarizedExperiment::assays(rv.custom$dataIn1)))
            || !("Variablefiltering" %in% names(rv.custom$dataIn2)))
          shinyjs::info(btnVentsMasg)
        else {
          
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["Variablefiltering"] <- MagellanNTK::stepStatus$VALIDATED
        }
      })
    })
    
    
    ###########################################################################-
    #
    #-------------------------------------SAVE----------------------------------
    #
    ###########################################################################-
    output$Save <- renderUI({
      MagellanNTK::process_layout(session,
                                  ns = NS(id),
                                  sidebar = tagList(),
                                  content = tagList(
                                    uiOutput(ns('save_txt')),
                                    uiOutput(ns('dl_ui'))
                                  )
      )
    })
    
    #### _content -----
    output$save_txt <- renderUI({
      req(rv$steps.status['Save'] != MagellanNTK::stepStatus$VALIDATED)
      req(config@mode == 'process')
      
      div(
        style = "margin: 25px;",
        p(HTML("Click <b>'Run'</b> to validate this step.<br>
                If you need to make changes, click <b>'Reset'</b>."),
          style = "font-size: 17px;
                   line-height: 1.6;
                   margin: 0;
                   padding: 12px 16px;
                   background-color: #EAEAEA;
                   border-radius: 4px;"
        )
      )
    })
    
    output$dl_ui <- renderUI({
      req(rv$steps.status['Save'] == MagellanNTK::stepStatus$VALIDATED)
      req(config@mode == 'process')
      
      Prostar2::download_dataset_ui(ns(paste0(id, '_createQuickLink')))
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Save', btnEvents()))
      
      shiny::withProgress(message = paste0("Saving process", id), {
        shiny::incProgress(0.5)
        
        if (isTRUE(all.equal(SummarizedExperiment::assays(rv.custom$dataIn2),
                             SummarizedExperiment::assays(dataIn()))))
          shinyjs::info(btnVentsMasg)
        
        else {
          # Rename the new dataset with the name of the process
          names(rv.custom$dataIn2)[length(rv.custom$dataIn2)] <- 'Filtering'
          S4Vectors::metadata(rv.custom$dataIn2)$name.pipeline <- 'PipelineProtein'
          
          # DO NOT MODIFY THE THREE FOLLOWING LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- rv.custom$dataIn2
          rv$steps.status['Save'] <- MagellanNTK::stepStatus$VALIDATED
          
          Prostar2::download_dataset_server(paste0(id, '_createQuickLink'), dataIn = reactive({dataOut$value}))
        }
      })
    })
    
    ####### _END_ -----
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  }
  )
}
