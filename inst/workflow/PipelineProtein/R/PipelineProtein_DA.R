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
#' In this example, `PipelineProtein_DA_UI()` and `PipelineProtein_DA_server()` define
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
#' library(MagellanNTK)
#' library(plotly)
#' library(DaparToolshed)
#' library(Prostar2)
#' library(omXplore)
#' library(SummarizedExperiment)
#' data(Exp1_R25_prot, package = "DaparToolshedData")
#' obj <- Exp1_R25_prot
#' # Simulate imputation of missing values
#' obj <- NAIsZero(obj, 1)
#' obj <- NAIsZero(obj, 2)
#' qData <- as.matrix(SummarizedExperiment::assay(obj[[2]]))
#' sTab <- colData(obj)
#' limma <- limmaCompleteTest(qData, sTab)
#' df <- cbind(limma$logFC, limma$P_Value)
#' new.dataset <- obj[[length(obj)]]
#' DaparToolshed::HypothesisTest(new.dataset) <- as.data.frame(df)
#' obj <- Prostar2::addDatasets(obj, new.dataset, 'HypothesisTest')
#' path <- system.file('workflow/PipelineProtein', package = 'Prostar2')
#' shiny::runApp(proc_workflowApp("PipelineProtein_DA", path, dataIn = obj))
#' }
#' 
#' 
#' @author Samuel Wieczorek
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
NULL

#' @rdname PipelineProtein
#' @export
#' 
PipelineProtein_DA_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelineProtein_DA',
    mode = 'process',
    steps = c("Pairwise comparison", "P-value calibration", "FDR"),
    mandatory = c(TRUE, TRUE, TRUE)
  )
}


#' @rdname PipelineProtein
#' 
#' @export
#'
PipelineProtein_DA_ui <- function(id){
  ns <- NS(id)
}



#' @rdname PipelineProtein
#' 
#' @importFrom stats setNames rnorm
#' @import DaparToolshed
#' @importFrom shinyjs info useShinyjs
#' 
#' @export
#' 
PipelineProtein_DA_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  btnEvents = reactive({NULL})
){
  
  requireNamespace('DaparToolshed')
  pkgs_require('magrittr')
  
  pkgs_require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))

  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list(
    Pairwisecomparison_Comparison = "None",
    Pairwisecomparison_tooltipInfo = NULL,
    Pvaluecalibration_numericValCalibration = 1,
    Pvaluecalibration_calibrationMethod = "Benjamini-Hochberg",
    Pvaluecalibration_nBinsHistpval = 80,
    FDR_tooltipInfo = NULL,
    FDR_viewAdjPval = FALSE,
    FDR_showtable = FALSE
  )
  
  rv.custom.default.values <- list(
    result_open_dataset = reactive({NULL}),
    
    tmp.dataIn = NULL,
    resAnaDiff = NULL,
    res_AllPairwiseComparisons = NULL,
    Pairwisecomparison_tooltipInfo = NULL,
    Pairwisecomparison_pushPval_SummaryDT = data.frame(
      comparison = "-",
      query = "-",
      nbPushed = "0",
      TotalPushed = '0',
      TotalNonPushed = '0',
      stringsAsFactors = FALSE
    ),
    thpval = 0,
    thlogfc = 0,
    nbTotalAnaDiff = NULL,
    nbSelectedAnaDiff = NULL,
    nbSelectedTotal_FDR = NULL,
    nbSelected_FDR = NULL,
    conditions = list(cond1 = NULL, cond2 = NULL),
    calibrationRes = NULL,
    errMsgcalibrationPlot = NULL,
    errMsgcalibrationPlotALL = NULL,
    pi0 = NULL,
    filename = NULL,
    AnaDiff_indices = reactive({NULL}),
    dataToAnalyze = NULL,
    Condition1 = NULL,
    Condition2 = NULL,
    history = MagellanNTK::InitializeHistory(),
    step1_query = '-',
    FDR_tooltipInfo = NULL
  )
  
  grey <- "#FFFFFF"
  orangeProstar <- "#E97D5E"
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pkgs_require('grDevices')
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(
      str2expression(
        MagellanNTK::Get_Workflow_Core_Code(
          mode = 'process',
          name = id,
          w.names = names(widgets.default.values),
          rv.custom.names = names(rv.custom.default.values)
        )
      )
    )
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
        sidebar = tagList(),
        content = tagList(
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
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Description', btnEvents()))
      req(dataIn())
      req(inherits(dataIn(), 'QFeatures'))
      shiny::withProgress(message = paste0("Reseting process", id), {
        shiny::incProgress(0.5)
        
        # Find the assay containing the hypothesis tests comparisons
        .ind <- unlist(lapply(seq(length(dataIn())), function(x){
          if(!is.null(DaparToolshed::HypothesisTest(dataIn()[[x]])))
            x
        }))
        #rv$dataIn <- dataIn()[[.ind]]
        rv$dataIn <- dataIn()
        
        # Adds an assay to work on
        rv$dataIn <- QFeatures::addAssay(
          rv$dataIn, 
          rv$dataIn[[length(rv$dataIn)]], 
          'DA')
        
        rv.custom$res_AllPairwiseComparisons <- DaparToolshed::HypothesisTest(rv$dataIn[[length(rv$dataIn)]])
        rv.widgets$Pairwisecomparison_tooltipInfo <- DaparToolshed::idcol(rv$dataIn[[length(rv$dataIn)]])
        
        .se <- rv$dataIn[[length(rv$dataIn)]]
        .history <- DaparToolshed::paramshistory(.se)
        
        .ind_logFC <- which(.history[, 'Parameter'] == 'thlogFC')
        # Get logfc threshold from Hypothesis test dataset
        .thlogfc <- as.numeric(.history[.ind_logFC, 'Value'])
        if(!is.null(.thlogfc))
          rv.custom$thlogfc <- .thlogfc
        
        rv.custom$Pairwisecomparison_pushPval_SummaryDT <- data.frame(
          comparison = "-",
          query = "-",
          nbPushed = "0",
          TotalPushed = '0',
          TotalNonPushed = nrow(rv$dataIn[[length(rv$dataIn)]]),
          stringsAsFactors = FALSE
        )
        #DaparToolshed::paramshistory(.se) <- NULL
        
        compname <- Get_Pairwisecomparison_Names()
        rv.custom$pushed <- setNames(rep(list(NULL), length(compname)), compname)
        
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status['Description'] <- MagellanNTK::stepStatus$VALIDATED
      })
    })
    
    
    ### func -----
    Get_Dataset_to_Analyze <- reactive({
      req(rv.widgets$Pairwisecomparison_Comparison != 'None')
      req(rv$dataIn)
      datasetToAnalyze <- NULL
      .split <- strsplit(
        as.character(rv.widgets$Pairwisecomparison_Comparison), "_vs_"
      )
      rv.custom$Condition1 <- .split[[1]][1]
      rv.custom$Condition2 <- .split[[1]][2]
      
      rv.custom$filename <- paste0("anaDiff_", rv.custom$Condition1,
        "_vs_", rv.custom$Condition2, ".xlsx")
      
      
      if (length(grep("all-", rv.widgets$Pairwisecomparison_Comparison)) == 1) {
        .conds <- DaparToolshed::design_qf(rv$dataIn)$Condition
        condition1 <- strsplit(as.character(rv.widgets$Pairwisecomparison_Comparison), "_vs_")[[1]][1]
        ind_virtual_cond2 <- which(.conds != condition1)
        datasetToAnalyze <- rv$dataIn[[length(rv$dataIn)]]
      } else {
        ind <- c(
          which(DaparToolshed::design_qf(rv$dataIn)$Condition == rv.custom$Condition1),
          which(DaparToolshed::design_qf(rv$dataIn)$Condition == rv.custom$Condition2)
        )
        
        
        # Reduce the size of the variable to be used in volcanoplot
        # One need the quantitative
        datasetToAnalyze <- rv$dataIn[[length(rv$dataIn)]][, ind]
      }
      
      .logfc <- paste0(rv.widgets$Pairwisecomparison_Comparison, '_logFC')
      .pval <- paste0(rv.widgets$Pairwisecomparison_Comparison, '_pval')
      
      rv.custom$resAnaDiff <- list(
        logFC = (rv.custom$res_AllPairwiseComparisons)[, .logfc],
        P_Value = (rv.custom$res_AllPairwiseComparisons)[, .pval],
        condition1 = rv.custom$Condition1,
        condition2 = rv.custom$Condition2
      )
      
      datasetToAnalyze
    })
    
    Get_Dataset_to_Analyze_pushPVAL <- reactive({
      rv$dataIn[[length(rv$dataIn)]]
    })
    
    GetComparisons <- reactive({
      req(rv.widgets$Pairwisecomparison_Comparison != 'None')
      req(rv.custom$Condition1)
      req(rv.custom$Condition2)
      c(rv.custom$Condition1, rv.custom$Condition2)
    })
    
    Get_Pairwisecomparison_Names <- reactive({
      req(rv.custom$res_AllPairwiseComparisons)
      
      .names <- colnames(rv.custom$res_AllPairwiseComparisons)
      .names <- gsub('_logFC', '', .names, fixed = TRUE)
      .names <- gsub('_pval', '', .names, fixed = TRUE)
      
      .names <- unique(.names)
      .names
    })
    
    GetCalibrationMethod <- reactive({
      req(rv.widgets$Pvaluecalibration_numericValCalibration)
      req(rv.widgets$Pvaluecalibration_calibrationMethod != 'None')
      .calibMethod <- NULL
      if (rv.widgets$Pvaluecalibration_calibrationMethod == "Benjamini-Hochberg") {
        .calibMethod <- 1
      } else if (rv.widgets$Pvaluecalibration_calibrationMethod == "numeric value") {
        .calibMethod <- as.numeric(rv.widgets$Pvaluecalibration_numericValCalibration)
      } else {
        .calibMethod <- rv.widgets$Pvaluecalibration_calibrationMethod
      }
      .calibMethod
    })
    
    
    ###########################################################################-
    #
    #--------------------------PAIRWISE COMPARISON------------------------------
    #
    ###########################################################################-
    output$Pairwisecomparison <- renderUI({
      shinyjs::useShinyjs()
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          tags$div(id = ns('div_Pairwisecomparison_Comparison_UI'),
            uiOutput(ns('Pairwisecomparison_Comparison_UI')),
            uiOutput(ns("Pairwisecomparison_pushpval_UI"))
          )
        ),
        content = tagList(
          div(style = "display: flex; margin-top: 10px; gap: 25px;",
            uiOutput(ns("Pairwisecomparison_volcano_UI")),
            uiOutput(ns("Pairwisecomparison_tooltipInfo_UI"))),
          br(),
          uiOutput(ns("Pairwisecomparison_pushPval_DT_UI")),
          br()
        )
      )
    })
    
    #### _sidebar -----
    output$Pairwisecomparison_Comparison_UI <- renderUI({
      req(rv.custom$res_AllPairwiseComparisons)
      
      widget <- selectInput(ns("Pairwisecomparison_Comparison"), "Select a comparison",
        choices = c('None', Get_Pairwisecomparison_Names()),
        selected = rv.widgets$Pairwisecomparison_Comparison,
        width = "200px")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pairwisecomparison"])
    })
    
    output$Pairwisecomparison_pushpval_UI <- renderUI({
      req(rv.widgets$Pairwisecomparison_Comparison != "None")
      
      widget <- tagList(
        p(style = "font-weight: bold;margin-bottom: 5px;",
          "Push p-value"),
        Prostar2::mod_qMetacell_FunctionFilter_Generator_ui(ns("AnaDiff_query"))
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pairwisecomparison"])
    })
    
    #### _content -----
    output$Pairwisecomparison_volcano_UI <- renderUI({
      widget <- div(id = ns('div_Pairwisecomparison_volcano'),
                    mod_volcanoplot_ui(ns("Pairwisecomparison_volcano"))
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pairwisecomparison"])
    })
    
    Prostar2::mod_volcanoplot_server(
      id = "Pairwisecomparison_volcano",
      dataIn = reactive({Get_Dataset_to_Analyze()}),
      comparison = reactive({c(rv.custom$Condition1, rv.custom$Condition2)}),
      group = reactive({DaparToolshed::design_qf(rv$dataIn)$Condition}),
      thlogfc = reactive({rv.custom$thlogfc}),
      tooltip = reactive({rv.custom$Pairwisecomparison_tooltipInfo}),
      remoteReset = reactive({remoteReset()})
    )
    
    output$Pairwisecomparison_tooltipInfo_UI <- renderUI({
      req(rv$dataIn)
      req(rv.widgets$Pairwisecomparison_Comparison != "None")
      req(rv.widgets$Pairwisecomparison_tooltipInfo)
      
      widget <- tagList(div(style = "margin-top: 25px;", ""),
        selectInput(ns("Pairwisecomparison_tooltipInfo"),
          "Tooltip",
          choices = colnames(SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])),
          selected = rv.widgets$Pairwisecomparison_tooltipInfo,
          multiple = TRUE,
          selectize = FALSE,
          width = "300px", 
          size = 10
        ),
        actionButton(ns("Pairwisecomparison_validTooltipInfo"),  
                     "Validate tooltip choice", 
                     class = "btn-info")
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pairwisecomparison"])
    })
    
    observeEvent(input$Pairwisecomparison_validTooltipInfo, {
      rv.custom$Pairwisecomparison_tooltipInfo <- rv.widgets$Pairwisecomparison_tooltipInfo
    })
    
    observe({
      req(rv$steps.enabled["Pairwisecomparison"])
      req(rv$dataIn)
      .split <- strsplit(
        as.character(rv.widgets$Pairwisecomparison_Comparison), "_vs_"
      )
      tmpCond1 <- .split[[1]][1]
      tmpCond2 <- .split[[1]][2]
      compcond <- c(tmpCond1, tmpCond2)
      idxcond <- which(DaparToolshed::design_qf(rv$dataIn)$Condition %in% compcond)
      
      datapush <- rv$dataIn[[length(rv$dataIn)]][, idxcond]
      SummarizedExperiment::rowData(datapush)$qMetacell <- SummarizedExperiment::rowData(datapush)$qMetacell[, idxcond]
      
      rv.custom$AnaDiff_indices <- Prostar2::mod_qMetacell_FunctionFilter_Generator_server(
        id = "AnaDiff_query",
        dataIn = reactive({datapush}),
        conds = reactive({DaparToolshed::design_qf(rv$dataIn)$Condition[idxcond]}),
        keep_vs_remove = reactive({
          stats::setNames(c('Push p-value', 'Keep original p-value'), 
            nm = c("delete", "keep"))}),
        remoteReset = reactive({remoteReset()}),
        is.enabled = reactive({rv$steps.enabled["Pairwisecomparison"]})
      )
    })
    
    observeEvent(req(length(rv.custom$AnaDiff_indices()$value$ll.fun) > 0),{
      .ind <- unlist(rv.custom$AnaDiff_indices()$value$ll.indices)
      .cmd <- rv.custom$AnaDiff_indices()$value$ll.widgets.value[[1]]$keep_vs_remove
      
      if (length(.ind) > 1 && length(.ind) < nrow(Get_Dataset_to_Analyze())) {
        
        if (.cmd == 'delete')
          indices_to_push <- .ind
        else if (.cmd == 'keep')
          indices_to_push <- seq_len(nrow(Get_Dataset_to_Analyze()))[-(.ind)]
        
        .pval <- paste0(rv.widgets$Pairwisecomparison_Comparison, '_pval')
        
        DaparToolshed::HypothesisTest(rv$dataIn[[length(rv$dataIn)]])[indices_to_push, .pval] <- 1.00000000001
        rv.custom$res_AllPairwiseComparisons <- DaparToolshed::HypothesisTest(rv$dataIn[[length(rv$dataIn)]])
        #rv.custom$history <- MagellanNTK::Add2History(rv.custom$history, 'DA', 'Pairwisecomparison', 'Number of pushed values to 1', length(indices_to_push))
        
        comppushed <- unlist(rv.custom$pushed[rv.widgets$Pairwisecomparison_Comparison])
        comppushed <- unique(c(comppushed, indices_to_push))
        rv.custom$pushed[rv.widgets$Pairwisecomparison_Comparison] <- list(comppushed)
        rv.custom$resAnaDiff$pushed <- length(comppushed)
        #rv.custom$step1_query <- rv.custom$AnaDiff_indices()$value$ll.query
        
        comparison <- rv.widgets$Pairwisecomparison_Comparison
        query <- rv.custom$AnaDiff_indices()$value$ll.query
        pushed <- length(indices_to_push)
        totalpushed <- rv.custom$resAnaDiff$pushed
        totalnonpushed <- nrow(rv$dataIn[[length(rv$dataIn)]]) - totalpushed
        rv.custom$Pairwisecomparison_pushPval_SummaryDT <- rbind(
          rv.custom$Pairwisecomparison_pushPval_SummaryDT,
          c(comparison, query, pushed, totalpushed, totalnonpushed))
      }
    })
    
    observeEvent({rv.widgets$Pairwisecomparison_Comparison
                 rv.custom$Pairwisecomparison_pushPval_SummaryDT},{
      req(rv.widgets$Pairwisecomparison_Comparison != "None")
      req(rv.custom$Pairwisecomparison_pushPval_SummaryDT)
      
      dt <- rv.custom$Pairwisecomparison_pushPval_SummaryDT
      dt <- rbind(rv.custom$Pairwisecomparison_pushPval_SummaryDT[1, ],
                  dt[which(dt$comparison == rv.widgets$Pairwisecomparison_Comparison), ])
      dt <- dt[, -1]
      rv.custom$Pairwisecomparison_pushPval_SummaryDT_comp <- dt
    }, ignoreInit = TRUE)
     
    MagellanNTK::format_DT_server("dt", 
                                  dataIn = reactive({rv.custom$Pairwisecomparison_pushPval_SummaryDT_comp}))
    
    output$Pairwisecomparison_pushPval_DT_UI <- renderUI({
      req(rv.widgets$Pairwisecomparison_Comparison != "None")
      req(rv.custom$Pairwisecomparison_pushPval_SummaryDT_comp)
      
      MagellanNTK::format_DT_ui(ns("dt"))
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Pairwisecomparison', btnEvents()))
      shiny::withProgress(message = paste0("Reseting process", id), {
        shiny::incProgress(0.5)
        
        if ( rv.widgets$Pairwisecomparison_Comparison == widgets.default.values$Pairwisecomparison_Comparison 
          || is.null(rv$dataIn))
          shinyjs::info(btnVentsMasg)
        else {
          rv.custom$resAnaDiff$pushed <- length(rv.custom$pushed[rv.widgets$Pairwisecomparison_Comparison])
          query_list <- unlist(rv.custom$Pairwisecomparison_pushPval_SummaryDT_comp[, "query"])
          if (length(query_list) > 1){
            rv.custom$step1_query <- paste(query_list[-1], sep = " ; ")
          } else {
            rv.custom$step1_query <- "-"
          }
          
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DA', 'Pairwisecomparison', 'Comparison', rv.widgets$Pairwisecomparison_Comparison)
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DA', 'Pairwisecomparison', 'Push pval query', rv.custom$step1_query)
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DA', 'Pairwisecomparison', 'Nb pushed pval', rv.custom$resAnaDiff$pushed)
          
          #.comparisons2Txt <- Get_Pairwisecomparison_Names()
          
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["Pairwisecomparison"] <- MagellanNTK::stepStatus$VALIDATED
        }
      })
    })
    
    
    ###########################################################################-
    #
    #--------------------------P-VALUE CALIBRATION------------------------------
    #
    ###########################################################################-
    output$Pvaluecalibration <- renderUI({
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns('Pvaluecalibration_calibrationMethod_UI')),
          uiOutput(ns("Pvaluecalibration_numericValCalibration_UI")),
          div(style = "margin: 10px 0px 15px 20px; font-size: 20px; font-weight: 900;",
              paste0("pi0 = ", round(as.numeric(rv.custom$pi0), digits = 2))),
          div(style = "white-space: nowrap;",
            uiOutput(ns("Pvaluecalibration_nBins_UI")))
        ),
        content = tagList(
          fluidRow(
            tags$style(HTML(".cp-container img {margin: 0 !important;}")),
            div(class = "cp-container", style = "display: flex; margin-top: 10px; margin-bottom: 125px;",
                imageOutput(ns("calibrationPlotAll")),
                imageOutput(ns("calibrationPlot"))
            ),
            plotly::plotlyOutput(ns("histPValue"))
          )
        )
      )
    })
    
    #### _sidebar -----
    output$Pvaluecalibration_calibrationMethod_UI <- renderUI({
      calibMethod_Choices <- c(
        "Benjamini-Hochberg",
        "st.boot", "st.spline",
        "langaas", "jiang", "histo",
        "pounds", "abh", "slim",
        "numeric value"
      )
      names(calibMethod_Choices) <- calibMethod_Choices
      
      widget <- selectInput(ns("Pvaluecalibration_calibrationMethod"), "Calibration method",
        choices = c("None" = "None", calibMethod_Choices),
        selected = rv.widgets$Pvaluecalibration_calibrationMethod,
        width = "200px"
      )
      MagellanNTK::toggleWidget(widget,  rv$steps.enabled["Pvaluecalibration"])
    })
    
    output$Pvaluecalibration_numericValCalibration_UI <- renderUI({
      req(rv.widgets$Pvaluecalibration_calibrationMethod == "numeric value")
      
      # Use the current input value if available; otherwise, use the initial reactive value
      widget_value <- if (!is.null(input$Pvaluecalibration_numericValCalibration)) {
        input$Pvaluecalibration_numericValCalibration
      } else {
        rv.widgets$Pvaluecalibration_numericValCalibration
      }
      
      widget <- shinyWidgets::autonumericInput(
        ns("Pvaluecalibration_numericValCalibration"),
        label = "Proportion of TRUE null hypothesis",
        value = widget_value,  # Preserve user input
        width = "150px",
        minimumValue = 0,
        maximumValue = 1,
        decimalCharacter = ".",
        decimalPlaces = 2,
        modifyValueOnWheel = FALSE,
        align = "left"
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pvaluecalibration"])
    })
    
    observeEvent(input$Pvaluecalibration_numericValCalibration, {
      rv.widgets$Pvaluecalibration_numericValCalibration <- input$Pvaluecalibration_numericValCalibration
    }, ignoreInit = TRUE)
    
    # Debounce the reactive value ONLY for the plot (300ms delay)
    debounced_numericVal <- debounce(
      reactive({ rv.widgets$Pvaluecalibration_numericValCalibration }),
      300  # Adjust delay as needed
    )
    
    output$Pvaluecalibration_nBins_UI <- renderUI({
      req(rv.custom$resAnaDiff)
      req(rv.custom$pi0)
      
      widget <- selectInput(
        ns("Pvaluecalibration_nBinsHistpval"), 
        "n bins of p-value histogram",
        choices = c(1, seq(from = 0, to = 100, by = 10)[-1]),
        selected = rv.widgets$Pvaluecalibration_nBinsHistpval, 
        width = "100px")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pvaluecalibration"])
    })
    
    #### _content -----
    calibrationAllData <- reactive({
      req(rv.custom$resAnaDiff)
      req(rv$dataIn)
      req(length(rv.custom$resAnaDiff$logFC) > 0)
      
      m <- DaparToolshed::matchMetacell(
        DaparToolshed::qMetacell(rv$dataIn[[length(rv$dataIn)]]),
        pattern = c("Missing", "Missing POV", "Missing MEC"),
        level = "peptide"
      )
      req(length(which(m)) == 0)
      
      t <- rv.custom$resAnaDiff$P_Value
      toDelete <- which(t > 1)
      if (length(toDelete) > 0) { 
        t <- t[-toDelete]
      }
      
      list(t = t)
    })
    
    output$calibrationPlotAll <- renderImage({
      dat <- calibrationAllData()
      withProgress(message = "", detail = "", value = 0, {
        incProgress(0.5, detail = "Building calibration plots...")
        
        outfile <- tempfile(fileext = ".png")
        # Open the PNG device FIRST
        grDevices::png(outfile, width = 600, height = 500)
        # Run the function while the PNG device is active.
        # wrapperCalibrationPlot() draws into this device.
        ll <- tryCatch(
          catchToList(DaparToolshed::wrapperCalibrationPlot(dat$t,
                                                            "ALL")),
          
          error = function(e) {
            shinyjs::info(paste("Calibration plot all methods: ", conditionMessage(e)))
            NULL
          }
        )
        
        # Close the PNG device
        grDevices::dev.off()
        
        # Store pi0 AFTER the calculation
        if (!is.null(ll) && !is.null(ll$value) && !is.null(ll$value$pi0)) {
          rv.custom$pi0 <- ll$value$pi0
        }
        # Store warnings
        if (!is.null(ll)) {
          rv.custom$errMsgCalibrationPlot <- ll$warnings[grep("Warning:", ll$warnings)]
        }
        
        list(src = outfile, alt = "Calibration plot")
      })
    }, deleteFile = TRUE)
    
    output$errMsgCalibrationPlotAll <- renderUI({
      rv.custom$errMsgCalibrationPlotAll
      req(rv$dataIn)
      req(!is.null(rv.custom$errMsgCalibrationPlotAll))
      
      txt <- NULL
      for (i in seq_along(rv.custom$errMsgCalibrationPlotAll)) {
        txt <- paste(txt, "errMsgCalibrationPlotAll:",
                     rv.custom$errMsgCalibrationPlotAll[i], "<br>",
                     sep = ""
        )
      }
      
      div(id = ns('div_errMsgCalibrationPlotAll'),
          HTML(txt), style = "color:red")
    })
    
    calibrationData <- reactive({
      req(rv.custom$resAnaDiff)
      req(rv$dataIn)
      req(length(rv.custom$resAnaDiff$logFC) > 0)
      
      m <- DaparToolshed::matchMetacell(
        DaparToolshed::qMetacell(rv$dataIn[[length(rv$dataIn)]]),
        pattern = c("Missing", "Missing POV", "Missing MEC"),
        level = "peptide"
      )
      req(length(which(m)) == 0)
      
      t <- rv.custom$resAnaDiff$P_Value
      toDelete <- which(t > 1)
      if (length(toDelete) > 0) t <- t[-toDelete]
      
      method <- rv.widgets$Pvaluecalibration_calibrationMethod
      if (method == "numeric value") {
        calibration_value <- debounced_numericVal()
        req(!is.null(calibration_value))
        req(!is.na(calibration_value))
      } else if (method == "Benjamini-Hochberg") {
        calibration_value <- 1
      } else {
        calibration_value <- method
      }
      
      list(t = t, calibration_value = calibration_value)
    })
    
    output$calibrationPlot <- renderImage({
      dat <- calibrationData()
      withProgress(message = "", detail = "", value = 0, {
          incProgress(0.5, detail = "Building calibration plots...")
          
          outfile <- tempfile(fileext = ".png")
          # Open the PNG device FIRST
          grDevices::png(outfile, width = 600, height = 500)
          # Run the function while the PNG device is active.
          # wrapperCalibrationPlot() draws into this device.
          ll <- tryCatch(
            catchToList(
              DaparToolshed::wrapperCalibrationPlot(dat$t,
                                                    dat$calibration_value)
            ),
            
            error = function(e) {
              shinyjs::info(paste("Calibration plot:", conditionMessage(e)))
              NULL
            }
          )
          
          # Close the PNG device
          grDevices::dev.off()
          
          # Store pi0 AFTER the calculation
          if (!is.null(ll) && !is.null(ll$value) && !is.null(ll$value$pi0)) {
            rv.custom$pi0 <- ll$value$pi0
          }
          # Store warnings
          if (!is.null(ll)) {
            rv.custom$errMsgCalibrationPlot <- ll$warnings[grep("Warning:", ll$warnings)]
          }
          
          list(src = outfile, alt = "Calibration plot")
      })
    }, deleteFile = TRUE)

    output$errMsgCalibrationPlot <- renderUI({
      req(rv.custom$errMsgCalibrationPlot)
      req(rv$dataIn)

      txt <- NULL

      for (i in seq_along(rv.custom$errMsgCalibrationPlot)) {
        txt <- paste(txt, "errMsgCalibrationPlot: ",
          rv.custom$errMsgCalibrationPlot[i], "<br>", sep = "")
      }

      div(id = ns('div_errMsgCalibrationPlot'),
        HTML(txt), style = "color:red")
    })
    
  
    output$histPValue <- plotly::renderPlotly({
      histPValue()
    })
    
    histPValue <- reactive({
      req(rv.custom$resAnaDiff)
      req(rv.custom$pi0)
      req(rv.widgets$Pvaluecalibration_nBinsHistpval)
      req(rv.custom$thlogfc)
      req(!is.na(rv.custom$thlogfc))
      req(length(rv.custom$resAnaDiff$logFC) > 0)
      
      
      m <- DaparToolshed::matchMetacell(DaparToolshed::qMetacell(rv$dataIn[[length(rv$dataIn)]]),
                                        pattern = c("Missing", "Missing POV", "Missing MEC"),
                                        level = DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]])
      )
      req(length(which(m)) == 0)
      
      t <- NULL
      method <- NULL
      # t <- rv.custom$resAnaDiff$P_Value
      # t <- t[which(abs(rv.custom$resAnaDiff$logFC) >= rv.custom$thlogfc)]
      # toDelete <- which(t == 1)
      
      t <- rv.custom$resAnaDiff$P_Value
      toDelete <- which(t > 1)
      
      if (length(toDelete) > 0) {
        t <- t[-toDelete]
      }
      
      DaparToolshed::histPValue_HC(t,
                                   bins = as.numeric(rv.widgets$Pvaluecalibration_nBinsHistpval),
                                   pi0 = rv.custom$pi0)
    })
    
    
    output$Pvaluecalibration_calibrationResults <- renderUI({
      req(rv.custom$calibrationRes)
      rv$dataIn
      
      txt <- paste("Non-DA protein proportion = ",
                   round(100 * rv.custom$calibrationRes$pi0, digits = 2), "%<br>",
                   "DA protein concentration = ",
                   round(100 * rv.custom$calibrationRes$h1.concentration, digits = 2),
                   "%<br>",
                   "Uniformity underestimation = ",
                   rv.custom$calibrationRes$unif.under, "<br><br><hr>",
                   sep = ""
      )
      
      HTML(txt)
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Pvaluecalibration', btnEvents()))
      shiny::withProgress(message = paste0("Runninf P-value calibration", id), {
        shiny::incProgress(0.5)
        
        if (is.null(rv$dataIn))
          shinyjs::info(btnVentsMasg)
        else {
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DA', 'Pvaluecalibration', 'Calibration method', GetCalibrationMethod())
          
          if (!is.null(rv.custom$calibrationRes$pi0))
            rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DA', 'Pvaluecalibration', 'pi0', rv.custom$calibrationRes$pi0)
          
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DA', 'Pvaluecalibration', 'h1.concentration', rv.custom$calibrationRes$h1.concentration)
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DA', 'Pvaluecalibration', 'Uniformity underestimation', rv.custom$calibrationRes$unif.under)
          
          if (!is.null(rv.custom$calibrationRes$pi0))
            rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DA', 'Pvaluecalibration', 'Non-DA protein proportion', round(100 * rv.custom$calibrationRes$pi0, digits = 2))
          
          if (!is.null(rv.custom$calibrationRes$h1.concentration))
            rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DA', 'Pvaluecalibration', 'DA protein concentration', round(100 * rv.custom$calibrationRes$h1.concentration, digits = 2))
          
          rv.widgets$FDR_tooltipInfo <- rv.widgets$Pairwisecomparison_tooltipInfo
          rv.custom$FDR_tooltipInfo <- rv.widgets$Pairwisecomparison_tooltipInfo
          
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["Pvaluecalibration"] <- MagellanNTK::stepStatus$VALIDATED
        }
      })
    })
    

    ###########################################################################-
    #
    #----------------------------------FDR--------------------------------------
    #
    ###########################################################################-
    output$FDR <- renderUI({
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          tags$div(
            uiOutput(ns('FDR_widgets_ui')),
            uiOutput(ns('showFDR_UI')),
            tags$hr(),
            uiOutput(ns('FDR_showHideDT_UI')),
            uiOutput(ns('FDR_viewAdjPval_UI'))
          )
        ),
        content = div(div(
          style = "display: flex; gap: 20px;",
          uiOutput(ns('FDR_volcanoplot_UI')),
          div(uiOutput(ns("FDR_nbSelectedItems_ui")),
              uiOutput(ns("FDR_tooltipInfo_UI")))
        ),
        downloadButton(ns("FDR_download_SelectedItems_UI"),
                       "Selected final results (Excel file)", class = "btn-info"),
        DT::DTOutput(ns("FDR_selectedItems_UI")),
        br()
        )
      )
    })
    
    #### _sidebar -----
    output$FDR_widgets_ui <- renderUI({
      widget <- tags$div(
        mod_set_pval_threshold_ui(ns("Title")),
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDR"])
    })
    
    logpval <- Prostar2::mod_set_pval_threshold_server(id = "Title",
                                                       pval_init = reactive({10^(-rv.custom$thpval)}),
                                                       #fdr = reactive({Get_FDR()}),
                                                       remoteReset = reactive({remoteReset()}),
                                                       is.enabled = reactive({rv$steps.enabled["FDR"]}))
    
    observeEvent(logpval(), {
      req(logpval())
      tmp <- gsub(",", ".", logpval(), fixed = TRUE)
      
      rv.custom$thpval <- as.numeric(tmp)
      
      th <- Get_FDR() * Get_Nb_Significant()
      
      if (th < 1) {
        warntxt <- paste0("With such a dataset size (",
                          Get_Nb_Significant(), " selected discoveries), an FDR of ",
                          round(100 * Get_FDR(), digits = 2),
                          "% should be cautiously interpreted as strictly less than one
        discovery (", round(th, digits = 2), ") is expected to be false"
        )
        MagellanNTK::mod_errorModal_server('warn_FDR',
                                           title = 'Warning',
                                           text = warntxt)
      }
    })
    
    output$showFDR_UI <- renderUI({
      req(Get_FDR())
      txt <- "FDR = NA"
      if (!is.infinite(Get_FDR())) {
        txt <- paste0("FDR = ", round(100 * Get_FDR(), digits = 2), " %")
      }
      div(style = "margin: 15px 0px 15px 20px; font-size: 20px; font-weight: 900;",
          txt)
    })
    
    output$FDR_showHideDT_UI <- renderUI({
      widget <- checkboxInput(
        ns("FDR_showtable"),
        "Show table",
        value = FALSE
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDR"])
    })
    
    output$FDR_viewAdjPval_UI <- renderUI({
      req(!is.null(rv.widgets$FDR_showtable))
      
      widget <- div(style = "margin-left: 10px; margin-top: -20px;",
                    checkboxInput(ns('FDR_viewAdjPval'),
                              span(style = "font-weight: 100 !important;", 
                                   'View adjusted p-value'),
                              value = rv.widgets$FDR_viewAdjPval))
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDR"] && rv.widgets$FDR_showtable)
    })

    
    #### _content -----
    Prostar2::mod_volcanoplot_server(
      id = "FDR_volcano",
      dataIn = reactive({Get_Dataset_to_Analyze()}),
      comparison = reactive({c(rv.custom$Condition1, rv.custom$Condition2)}),
      group = reactive({DaparToolshed::design_qf(rv$dataIn)$Condition}),
      thlogfc = reactive({rv.custom$thlogfc}),
      thpval = reactive({rv.custom$thpval}),
      tooltip = reactive({rv.custom$FDR_tooltipInfo}),
      remoteReset = reactive({remoteReset()}),
      is.enabled = reactive({rv$steps.enabled["FDR"]})
    )
    
    
    output$FDR_volcanoplot_UI <- renderUI({
      widget <- div(id = ns('div_FDR_volcano'),
                    mod_volcanoplot_ui(ns("FDR_volcano"))
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDR"])
    })
    
    output$FDR_nbSelectedItems_ui <- renderUI({
      rv.custom$thpval
      rv$dataIn
      req(Build_pval_table())
      
      m <- DaparToolshed::matchMetacell(DaparToolshed::qMetacell(rv$dataIn[[length(rv$dataIn)]]),
                                        pattern = c("Missing", "Missing POV", "Missing MEC"),
                                        level = "peptide"
      )
      
      p <- Build_pval_table()
      upItemsPVal <- NULL
      upItemsLogFC <- NULL
      
      upItemsLogFC <- which(abs(p$logFC) >= as.numeric(rv.custom$thlogfc))
      upItemsPVal <- which(-log10(p$P_Value) >= as.numeric(rv.custom$thpval))
      
      rv.custom$nbTotalAnaDiff <- nrow(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]]))
      rv.custom$nbSelectedAnaDiff <- NULL
      t <- NULL
      
      if (!is.null(rv.custom$thpval) && !is.null(rv.custom$thlogfc)) {
        t <- intersect(upItemsPVal, upItemsLogFC)
      } else if (!is.null(rv.custom$thpval) && is.null(rv.custom$thlogfc)) {
        t <- upItemsPVal
      } else if (is.null(rv.custom$thpval) && !is.null(rv.custom$thlogfc)) {
        t <- upItemsLogFC
      }
      rv.custom$nbSelectedAnaDiff <- length(t)
      
      ##
      ## Condition: A = C + D
      ##
      A <- rv.custom$nbTotalAnaDiff
      B <- A - rv.custom$resAnaDiff$pushed
      C <- rv.custom$nbSelectedAnaDiff
      D <- (A - C)
      datatype <- DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]])
      
      tagList(
        div(class = "bloc_page",
            p(paste0("Total number of ", datatype, "(s) = ", A)),
            tags$em(p(style = "padding:0 0 0 20px;", 
                      paste0("Total remaining after push p-values = ", B))),
            p(paste0("Number of selected ", datatype, "(s) = ", C)),
            p(paste0("Number of non selected ", datatype, "(s) = ", D))
        ),
        tags$style(HTML(".bloc_page {
                          max-width: 400px;
                          background: #ffffff;
                          border: 1px solid #dddddd;
                          border-radius: 6px;
                          padding: 18px;
                          box-shadow: 0 2px 6px rgba(0,0,0,0.08);
                          margin-top: 20px;
                          }"))
      )
    })
    
    output$FDR_tooltipInfo_UI <- renderUI({
      req(rv$dataIn)
      req(rv.widgets$Pairwisecomparison_Comparison != "None")
      req(rv.widgets$FDR_tooltipInfo)
      
      widget <- tagList(div(style = "margin-top: 25px;", ""),
                        selectInput(ns("FDR_tooltipInfo"),
                                    "Tooltip",
                                    choices = colnames(SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])),
                                    selected = rv.widgets$FDR_tooltipInfo,
                                    multiple = TRUE,
                                    selectize = FALSE,
                                    width = "300px", 
                                    size = 10
                        ),
                        actionButton(ns("FDR_validTooltipInfo"),  
                                     "Validate tooltip choice", 
                                     class = "btn-info")
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDR"])
    })
    
    observeEvent(input$FDR_validTooltipInfo, {
      rv.custom$FDR_tooltipInfo <- rv.widgets$FDR_tooltipInfo
    })
    

    
    
    output$FDR_selectedItems_UI <- DT::renderDT({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$FDR_showtable)
      df <- Build_pval_table()
      
      if (rv.widgets$FDR_viewAdjPval){
        df <- df[order(df$isDifferential, decreasing = TRUE), ]
        df <- df[order(df$Adjusted_PValue, decreasing = FALSE), ]
      }
      
      if (rv.widgets$FDR_viewAdjPval){
        .coldefs <- list(list(width = "200px", targets = "_all"))
      } else {
        name <- paste0(c('Log_PValue (', 'Adjusted_PValue ('),
          as.character(rv.widgets$Pairwisecomparison_Comparison), ")")
        .coldefs <- list(
          list(width = "200px", targets = "_all"),
          list(targets = (match(name, colnames(df)) - 1), visible = FALSE))
      }
      
      DT::datatable(df,
        escape = FALSE,
        rownames = FALSE,
        selection = 'none',
        options = list(initComplete = MagellanNTK::initComplete(),
          dom = "frtip",
          pageLength = 100,
          scrollY = 500,
          scroller = TRUE,
          server = FALSE,
          columnDefs = .coldefs,
          ordering = !rv.widgets$FDR_viewAdjPval
        )
      ) |>
        DT::formatStyle(
          paste0("isDifferential (",
            as.character(rv.widgets$Pairwisecomparison_Comparison), ")"),
          target = "row",
          backgroundColor = DT::styleEqual(c(0, 1), c("white", orangeProstar))
        )
      
    })
    
    BuildPairwiseComp_wb <- reactive({
      DA_Style <- openxlsx::createStyle(fgFill = orangeProstar)
      hs1 <- openxlsx::createStyle(fgFill = "#DCE6F1",
        halign = "CENTER",
        textDecoration = "italic",
        border = "Bottom")
      
      wb <- openxlsx::createWorkbook() # Create wb in R
      openxlsx::addWorksheet(wb, sheetName = "DA result") # create sheet
      openxlsx::writeData(wb,
        sheet = 1,
        as.character(rv.widgets$Pairwisecomparison_Comparison),
        colNames = TRUE,
        headerStyle = hs1
      )
      openxlsx::writeData(wb,
        sheet = 1,
        startRow = 3,
        Build_pval_table(),
      )
      
      .txt <- paste0("isDifferential (",
        as.character(rv.widgets$Pairwisecomparison_Comparison),
        ")")
      
      ll.DA.row <- which(Build_pval_table()[, .txt] == 1)
      ll.DA.col <- rep(which(colnames(Build_pval_table()) == .txt),
        length(ll.DA.row) )
      
      openxlsx::addStyle(wb,
        sheet = 1, 
        cols = ll.DA.col,
        rows = 3 + ll.DA.row, 
        style = DA_Style
      )
      
      wb
    })
    
    output$FDR_download_SelectedItems_UI <- downloadHandler(
      filename = function() {rv.custom$filename},
      content = function(fname) {
        wb <- BuildPairwiseComp_wb()
        openxlsx::saveWorkbook(wb, file = fname, overwrite = TRUE)
      }
    )
    
    Get_FDR <- reactive({
      req(rv.custom$thpval)
      req(rv.custom$thlogfc)
      req(Build_pval_table())
      
      adj.pval <- Build_pval_table()$Adjusted_PValue
      logpval <- Build_pval_table()$Log_PValue
      .logfc <- Build_pval_table()$logFC


      upitems_logpval <- which(logpval >= rv.custom$thpval)
      upItems_logfcinf <- which(abs(.logfc) < rv.custom$thlogfc)
      upitems_logpval <- setdiff(upitems_logpval, upItems_logfcinf)

     
      if (length(adj.pval[upitems_logpval]) > 0){
        fdr <- max(adj.pval[upitems_logpval], na.rm = TRUE)
      } else {
        fdr <- 1
      }
      
      rv.custom$FDR <- as.numeric(fdr)
      as.numeric(rv.custom$FDR)
    })
    
    Get_Nb_Significant <- reactive({
      nb <- length(
        which(
          Build_pval_table()[paste0(
            "isDifferential (",
            as.character(rv.widgets$Pairwisecomparison_Comparison), ")"
          )] == 1
        )
      )
      rv$widgets$anaDiff$NbSelected <- nb
      nb
    })
    
    Build_pval_table <- reactive({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.custom$thlogfc)
      req(rv.custom$thpval)
      req(rv$dataIn)
      req(GetCalibrationMethod())
      req(GetComparisons())
      
      rv.widgets$Pairwisecomparison_Comparison
      ht <- DaparToolshed::HypothesisTest(rv$dataIn[[length(rv$dataIn)]])
      .logfc <- ht[, paste0(rv.widgets$Pairwisecomparison_Comparison, '_logFC')]
      .pval <- ht[, paste0(rv.widgets$Pairwisecomparison_Comparison, '_pval')]
      
      .digits <- 3
      
      pval_table <- data.frame(
        id = rownames(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])),
        logFC = round(.logfc, digits = .digits),
        P_Value = .pval,
        Log_PValue = -log10(.pval),
        Adjusted_PValue = rep(NA, length(.logfc)),
        isDifferential = rep(0, length(.logfc))
      )
      
      # Determine significant proteins
      signifItems <- intersect(which(pval_table$Log_PValue >= rv.custom$thpval),
        which(abs(pval_table$logFC) >= rv.custom$thlogfc)
      )
      pval_table[signifItems,'isDifferential'] <- 1
      
       upItems_pval <- which(-log10(.pval) >= rv.custom$thpval)
       #push to 1 proteins with logFC under threshold
       pval_pushfc <- .pval
       upItems_logfcinf <- which(abs(.logfc) < rv.custom$thlogfc)
       upItems_pushedpval <- which(.pval > 1)
       upItems_logfcinf <- setdiff(upItems_logfcinf, upItems_pushedpval)
       if (length(upItems_logfcinf) != 0){
         pval_pushfc[upItems_logfcinf] <- 1
       }  
       if (length(upItems_pushedpval) != 0){
         pval_pushfc <- pval_pushfc[-upItems_pushedpval]
       }
       rv.custom$adjusted_pvalues <- DaparToolshed::diffAnaComputeAdjustedPValues(
         pval_pushfc,
         GetCalibrationMethod())
       if (length(upItems_pushedpval) != 0){
         pval_table[-upItems_pushedpval, 'Adjusted_PValue'] <- rv.custom$adjusted_pvalues
       } else {
         pval_table[, 'Adjusted_PValue'] <- rv.custom$adjusted_pvalues
       }
       
      # Set only significant values
      pval_table$logFC <- signif(pval_table$logFC, digits = 4)
      pval_table$P_Value <- signif(pval_table$P_Value, digits = 4)
      pval_table$Adjusted_PValue <- signif(pval_table$Adjusted_PValue, digits = 4)
      pval_table$Log_PValue <- signif(pval_table$Log_PValue, digits = 4)
      
      tmp <- as.data.frame(
        SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])[, rv.custom$FDR_tooltipInfo]
      )
      names(tmp) <- rv.custom$FDR_tooltipInfo
      pval_table <- cbind(pval_table, tmp)
      
      colnames(pval_table)[2:6] <- paste0(colnames(pval_table)[2:6], " (", as.character(rv.widgets$Pairwisecomparison_Comparison), ")")
      
      pval_table
    })
    
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('FDR', btnEvents()))
      shiny::withProgress(message = paste0("COmputing FDR", id), {
        shiny::incProgress(0.5)
        if (is.null(rv$dataIn) || is.null(rv.custom$thpval))
          shinyjs::info(btnVentsMasg)
        else {
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DA', 'FDR', 'th pval', rv.custom$thpval)
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DA', 'FDR', '% FDR', round(100 * Get_FDR(), digits = 2))
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DA', 'FDR', 'Nb significant', Get_Nb_Significant())
          
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["FDR"] <- MagellanNTK::stepStatus$VALIDATED
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
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      
      req(grepl('Save', btnEvents()))
      
      shiny::withProgress(message = paste0("Reseting process", id), {
        shiny::incProgress(0.5)
        
        if (isTRUE(all.equal(SummarizedExperiment::assays(rv$dataIn), SummarizedExperiment::assays(dataIn()))))
          shinyjs::info(btnVentsMasg)
        else {
          
          # Do some stuff
          last.se <- length(rv$dataIn)
          DaparToolshed::paramshistory(rv$dataIn[[last.se]]) <- rbind(DaparToolshed::paramshistory(rv$dataIn[[last.se]]), rv.custom$history)
          
          # Add the result of pairwise comparison to the coldata
          DaparToolshed::DifferentialAnalysis(rv$dataIn[[last.se]]) <- Build_pval_table()
          
          # DO NOT MODIFY THE THREE FOLLOWING LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- rv$dataIn
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
