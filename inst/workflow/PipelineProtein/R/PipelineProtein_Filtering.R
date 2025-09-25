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
#' if (interactive()){
#' library(MagellanNTK)
#' data(Exp1_R25_prot, package = 'DaparToolshedData')
#' path <- system.file('workflow/PipelineProtein', package = 'Prostar2')
#' shiny::runApp(workflowApp("PipelineProtein_Filtering", path, dataIn = Exp1_R25_prot))
#' }
#' 
#' 
#' @author Samuel Wieczorek
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
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
  path = NULL,
  btnEvents = reactive({NULL})
){
  
  pkgs.require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))
  
  
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
    history = list(),
    
    
    # Variable Filtering variables
    # indices = NULL,
    Variablefiltering_query = list(),
    Variablefiltering_widgets.value = list(),
    Variablefiltering_variable_Filter_SummaryDT = data.frame(
      Variablefiltering_query = NA,
      Variablefiltering_nbDeleted = NA,
      Variablefiltering_TotalBeforeFiltering = NA,
      Variablefiltering_TotalAfterFiltering = NA,
      stringsAsFactors = FALSE
    ),
    
    Variablefiltering_ll.var = list(),
    Variablefiltering_ll.query = list(),
    Variablefiltering_ll.widgets.value = list()
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
      file <- normalizePath(file.path(
        system.file('workflow', package = 'Prostar2'),
        unlist(strsplit(id, '_'))[1], 
        'md', 
        paste0(id, '.Rmd')))
      
      
      MagellanNTK::process_layout(
        ns = NS(id),
        sidebar = tagList(),
        content = tagList(
          if (file.exists(file))
            includeMarkdown(file)
          else
            p('No Description available')
        )
      )
    })

    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Description', btnEvents()))
      req(inherits(dataIn(), 'QFeatures'))
      
      rv$dataIn <- dataIn()
      rv.custom$dataIn1 <- dataIn()
      rv.custom$dataIn2 <- dataIn()
      
      rv.custom$qMetacell_Filter_SummaryDT <- data.frame(
        query = "-",
        nbDeleted = "0",
        TotalMainAssay = nrow(rv$dataIn[[length(rv$dataIn)]]),
        stringsAsFactors = FALSE
      )
      
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- stepStatus$VALIDATED
    })
    
    
    
    # >>>
    # >>> START ------------- Code for step 1 UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI ------------------------------------
    output$Cellmetadatafiltering <- renderUI({
      
      MagellanNTK::process_layout(
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
    
    
    observe({
      req(rv$steps.enabled["Cellmetadatafiltering"])
      
      rv.custom$funFilter <- mod_qMetacell_FunctionFilter_Generator_server(
        id = "query",
        dataIn = reactive({rv$dataIn[[length(rv$dataIn)]]}),
        conds = reactive({DaparToolshed::design.qf(rv$dataIn)$Condition}),
        keep_vs_remove = reactive({stats::setNames(c('Push p-value', 'Keep original p-value'), nm = c("delete", "keep"))}),
        val_vs_percent = reactive({stats::setNames(nm = c("Count", "Percentage"))}),
        operator = reactive({stats::setNames(nm = SymFilteringOperators())}),
        remoteReset = reactive({remoteReset()}),
        is.enabled = reactive({rv$steps.enabled["Cellmetadatafiltering"]})
      )
    })
    
    output$Cellmetadatafiltering_buildQuery_ui <- renderUI({
      
      widget <- mod_qMetacell_FunctionFilter_Generator_ui(ns("query"))
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
    
    
    observeEvent(req(length(rv.custom$funFilter()$value$ll.fun) > 0), ignoreInit = FALSE,{
      req(rv$dataIn)
      
      
      tmp <- filterFeaturesOneSE(
        object = rv$dataIn,
        i = length(rv$dataIn),
        name = paste0("qMetacellFiltered", MagellanNTK::Timestamp()),
        filters = rv.custom$funFilter()$value$ll.fun
      )
      indices <- rv.custom$funFilter()$value$ll.indices
      
      
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
        rv.custom$dataIn1 <- removeAssay(tmp, length(tmp)-1)
      else
        rv.custom$dataIn1 <- tmp
      
      # Rename the new dataset with the name of the process
      names(rv.custom$dataIn1)[length(rv.custom$dataIn1)] <- 'qMetacellFiltering'
      
      # Add params
      #par <- rv.custom$funFilter()$ll.widgets.value
      query <- rv.custom$funFilter()$value$ll.query
      i <- length(rv.custom$dataIn1)
      .history <- DaparToolshed::paramshistory(rv.custom$dataIn1[[i]])[['Metacell_Filtering']]
      
      .history[[paste0('query_', length(.history))]] <- query
      DaparToolshed::paramshistory(rv.custom$dataIn1[[i]])[['Metacell_Filtering']] <- .history
      
      #rv.custom$dataIn2 <-rv$dataIn
    })
    
    
    output$plots_ui <- renderUI({
      req(rv.custom$funFilter()$value$ll.pattern)
      
      mod_ds_metacell_Histos_server(
        id = "plots",
        dataIn = reactive({rv$dataIn[[length(rv$dataIn)]]}),
        pattern = reactive({rv.custom$funFilter()$value$ll.pattern}),
        group = reactive({DaparToolshed::design.qf(rv$dataIn)$Condition})
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
    
    
    # >>> END: Definition of the widgets
    
    
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Cellmetadatafiltering', btnEvents()))
      
      if ( is.null(rv.custom$dataIn1) || !("qMetacellFiltering" %in% names(rv.custom$dataIn2)))
        info(btnVentsMasg)
      else {
        req(rv.custom$dataIn1)
      
      rv.custom$dataIn2 <- rv.custom$dataIn1
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status["Cellmetadatafiltering"] <- stepStatus$VALIDATED
      }
    })
    
    
    # <<< END ------------- Code for step 1 UI---------------
    
    
    # >>> START ------------- Code for step 2 UI---------------
    
    output$Variablefiltering <- renderUI({
      MagellanNTK::process_layout(
        ns = NS(id),
        sidebar = tagList(
          #timeline_process_ui(ns('Variablefiltering_timeline')),
          uiOutput(ns("Variablefiltering_chooseKeepRemove_ui")),
            uiOutput(ns("Variablefiltering_cname_ui")),
            uiOutput(ns("Variablefiltering_operator_ui")),
            uiOutput(ns("Variablefiltering_value_ui")),
            uiOutput(ns("Variablefiltering_addFilter_btn_ui"))
        ),
        content = tagList(
          uiOutput(ns("Variablefiltering_DT_UI"))
        )
      )
      
    })
    
    
    
    output$Variablefiltering_chooseKeepRemove_ui <- renderUI({
      
      widget <- radioButtons(ns("Variablefiltering_keep_vs_remove"),
        "Type of filter operation",
        choices = rv.widgets$Variablefiltering_keep_vs_remove,
        selected = rv.widgets$Variablefiltering_keep_vs_remove
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Variablefiltering"])
    })
    
    
    
    output$Variablefiltering_cname_ui <- renderUI({
      req(rv.custom$dataIn1)
      .choices <- c("None", colnames(SummarizedExperiment::rowData(rv.custom$dataIn1[[length(rv.custom$dataIn1)]])))
      
      widget <- selectInput(ns("Variablefiltering_cname"),
        "Column name",
        choices = stats::setNames(.choices, nm = .choices),
        selected = rv.widgets$Variablefiltering_cname,
        width = "200px"
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Variablefiltering"])
    })
    
    
    output$Variablefiltering_operator_ui <- renderUI({
      req(rv.custom$dataIn1)
      req(rv.widgets$Variablefiltering_cname %in% colnames(SummarizedExperiment::rowData(rv.custom$dataIn1[[length(rv.custom$dataIn1)]])))
      
      
      if (is.numeric(SummarizedExperiment::rowData(rv.custom$dataIn1[[length(rv.custom$dataIn1)]])[, rv.widgets$Variablefiltering_cname])) {
        .operator <- DaparToolshed::SymFilteringOperators()
      } else {
        .operator <- c("==", "!=", "startsWith", "endsWith", "contains")
      }
      
      .operator = c("None" = "None", .operator)
      
      widget <- selectInput(ns("Variablefiltering_operator"),
        "operator",
        choices = stats::setNames(nm = .operator),
        selected = rv.widgets$Variablefiltering_operator,
        width = "100px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Variablefiltering"])
    })
    
    
    output$Variablefiltering_value_ui <- renderUI({
      
      widget <- textInput(ns("Variablefiltering_value"),
        "value",
        placeholder = 'Enter value...',
        width = "100px"
        #value = rv.widgets$value
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Variablefiltering"])
    })
    
    
    
    
    observeEvent(c(rv.widgets$Variablefiltering_value, rv.widgets$Variablefiltering_cname), {
      req(rv.custom$dataIn1)
      req(rv.widgets$Variablefiltering_value != 'Enter value...')
      req(rv.widgets$Variablefiltering_cname != "None")
      
      if (is.na(Extract_Value(rv.widgets$Variablefiltering_value))) {
        showFeedbackWarning(
          inputId = "Variablefiltering_value",
          text = "wrong type of value"
        )  
      } else {
        hideFeedback("Variablefiltering_value")
      }
      
    })
    
    
    
    Extract_Value <- function(value){
      val <- NULL
      val <- tryCatch({
        
        
        if (is.numeric(SummarizedExperiment::rowData(rv.custom$dataIn1[[length(rv.custom$dataIn1)]])[, rv.widgets$Variablefiltering_cname]) ) {
          as.numeric(value)
        } else if (!is.numeric(SummarizedExperiment::rowData(rv.custom$dataIn1[[length(rv.custom$dataIn1)]])[, rv.widgets$Variablefiltering_cname])){
          as.character(value)
        }
      },
        warning = function(w){NA},
        error = function(e) NA
      )
      return(val)
    }
    
    
    
    Variablefiltering_BuildVariableFilter <- function(
    value = NULL,
      operator = NULL,
      cname = NULL,
      keep_vs_remove = NULL){
      
      req(value != 'Enter value...')
      req(operator != "None")
      req(cname != "None")
      req(Extract_Value(value))
      
      VariableFilter(
        field = cname,
        value = Extract_Value(value),
        condition = operator,
        not = keep_vs_remove == "delete"
      )
    }
    
    
    Variablefiltering_WriteQuery <- function(
    value = NULL,
      operator = NULL,
      cname = NULL,
      keep_vs_remove = NULL){
      
      
      value <- Extract_Value(value)
      query <- paste0(
        keep_vs_remove, " values for which ",
        cname, " ", operator, " ", value)
      query
      
    }
    
    
    
    output$Variablefiltering_addFilter_btn_ui <- renderUI({
      widget <- actionButton(ns("Variablefiltering_addFilter_btn"), "Add filter",
        class = "btn-info")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Variablefiltering"])
    })
    
    
    output$Variablefiltering_DT_UI <- renderUI({
      MagellanNTK::format_DT_server("Variablefiltering_dt", 
        dataIn = reactive({rv.custom$Variablefiltering_variable_Filter_SummaryDT}))
      
      MagellanNTK::format_DT_ui(ns("Variablefiltering_dt"))
    })
    
    
    
    observeEvent(input$Variablefiltering_addFilter_btn,
      ignoreInit = FALSE, ignoreNULL = TRUE, {
        
        
        rv.widgets$Variablefiltering_value
        rv.widgets$Variablefiltering_operator
        rv.widgets$Variablefiltering_cname
        
        
        rv.custom$Variablefiltering_ll.var <- list(
          Variablefiltering_BuildVariableFilter(
            value = rv.widgets$Variablefiltering_value,
            operator = rv.widgets$Variablefiltering_operator,
            cname = rv.widgets$Variablefiltering_cname,
            keep_vs_remove = rv.widgets$Variablefiltering_keep_vs_remove)
        )
        
        rv.custom$Variablefiltering_ll.query <- list(
          Variablefiltering_WriteQuery(
            value = rv.widgets$Variablefiltering_value,
            operator = rv.widgets$Variablefiltering_operator,
            cname = rv.widgets$Variablefiltering_cname,
            keep_vs_remove = rv.widgets$Variablefiltering_keep_vs_remove)
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
        
        
        
        ###########################################
        req(length(rv.custom$Variablefiltering_funFilter$ll.var) > 0)
        req(rv.custom$dataIn1)
        
        tmp <- filterFeaturesOneSE(
          object = rv.custom$dataIn1,
          i = length(rv.custom$dataIn1),
          name = paste0("variableFiltered", MagellanNTK::Timestamp()),
          filters = rv.custom$Variablefiltering_funFilter$ll.var
        )
        indices <- rv.custom$Variablefiltering_funFilter$ll.indices
        
        # Add infos
        
        nBefore <- nrow(tmp[[length(tmp) - 1]])
        nAfter <- nrow(tmp[[length(tmp)]])
        
        
        .html <- rv.custom$Variablefiltering_funFilter$ll.query
        .nbDeleted <- nBefore - nAfter
        .nbBefore <- nrow(SummarizedExperiment::assay(rv.custom$dataIn1[[length(rv.custom$dataIn1)]]))
        .nbAfter <- nrow(SummarizedExperiment::assay(tmp[[length(tmp)]]))
        
        rv.custom$Variablefiltering_variable_Filter_SummaryDT <- rbind(
          rv.custom$Variablefiltering_variable_Filter_SummaryDT , 
          c(.html, .nbDeleted, .nbBefore, .nbAfter))
        
        # Keeps only the last filtered SE
        len_start <- length(dataIn())
        len_end <- length(tmp)
        len_diff <- len_end - len_start
        
        req(len_diff > 0)
        
        if (len_diff == 2)
          rv.custom$dataIn2 <- removeAssay(tmp, length(tmp)-1)
        else 
          rv.custom$dataIn2 <- tmp
        
        # Rename the new dataset with the name of the process
        names(rv.custom$dataIn2)[length(rv.custom$dataIn2)] <- 'Variable_Filtering'
        
        
        query <- rv.custom$Variablefiltering_funFilter$ll.query
        i <- length(rv.custom$dataIn2)
        .history <- DaparToolshed::paramshistory(rv.custom$dataIn2[[i]])[['Variable_Filtering']]
        .history[[paste0('query_', length(.history))]] <- query
        DaparToolshed::paramshistory(rv.custom$dataIn2[[i]])[['Variable_Filtering']] <- .history
        
        
        
      })
    
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Variablefiltering', btnEvents()))
      

      if ( is.null(rv.custom$dataIn2) || !("Variable_Filtering" %in% names(rv.custom$dataIn2)))
        info(btnVentsMasg)
      else {
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status["Variablefiltering"] <- stepStatus$VALIDATED
      }
    })
    
    # <<< END ------------- Code for step 2 UI---------------
    
    
    # >>> START ------------- Code for step 'Save' UI---------------
    output$Save <- renderUI({
      MagellanNTK::process_layout(
        ns = NS(id),
        sidebar = tagList(
         # timeline_process_ui(ns('Save_timeline'))
        ),
        content = uiOutput(ns('dl_ui'))
      )
      
    })
    
    output$dl_ui <- renderUI({
      req(rv$steps.status['Save'] == stepStatus$VALIDATED)
      req(config@mode == 'process')
      
      MagellanNTK::download_dataset_ui(ns('createQuickLink'))
    })
    

    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Save', btnEvents()))
      
      if (isTRUE(all.equal(assays(rv.custom$dataIn2),assays(dataIn()))))
        info(btnVentsMasg)
      else {
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
      }
    })
    # <<< END ------------- Code for step 3 UI---------------
    
    
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  }
  )
}
