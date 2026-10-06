#' @title PipelineProtein DA module
#'
#' @description
#' This module contains the DA step of the protein pipeline.
#' 
#' @param id A `character(1)` which is the 'id' of the module.
#' @param dataIn An instance of the class `MultiAssayExperiment`
#' @param steps.enabled A vector of boolean which has the same length of the steps
#' of the pipeline. This information is used to enable/disable the widgets. It is not
#' a communication variable between the caller and this module, thus there is no
#' corresponding output variable
#' @param remoteReset It is a remote command to reset the module. An `integer()` that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#' @param steps.status A vector of `character()` which indicates the status of each step
#' which can be either 'validated', 'undone' or 'skipped'. Enabled or disabled in the UI.
#' @param current.pos A `integer(1)` which acts as a remote command to make
#'  a step active in the timeline. Default is 1.
#' @param path A `character()` which is the path to the directory which 
#' contains the files and directories of the pipeline.
#' 
#' @return An instance of the class `MultiAssayExperiment`
#' 
#' @examples
#' if (interactive()) {
#'   Prostar2("PipelineProtein_DA")
#' }
#' 
#' @name PipelineProtein_DA
#' 
#' @importFrom stats setNames rnorm
#' @importFrom shinyjs useShinyjs
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' 
NULL


#' @rdname PipelineProtein_DA
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


#' @rdname PipelineProtein_DA
#' @export
#'
PipelineProtein_DA_ui <- function(id){
  ns <- NS(id)
}


#' @rdname PipelineProtein_DA
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
  pkgs_require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors', 'magrittr', 'grDevices'))
  requireNamespace('DaparToolshed')
  
  # Default values for widgets
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
  
  # Default values for reactive values
  rv.custom.default.values <- list(
    history = MagellanNTK::InitializeHistory(),

    resAnaDiff = NULL,
    res_AllPairwiseComparisons = NULL,
    comparisonNames = NULL,
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
    containsNA = NULL,
    nbTotalAnaDiff = NULL,
    calibrationRes = NULL,
    errMsgcalibrationPlot = NULL,
    errMsgCalibrationPlotAll = NULL,
    GetCalibrationMethod = NULL,
    pi0 = NULL,
    filename = NULL,
    AnaDiff_indices = reactive({NULL}),
    Condition1 = NULL,
    Condition2 = NULL,
    step1_query = '-',
    FDR_tooltipInfo = NULL
  )
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Code hosted by MagellanNTK to create the process
    # DO NOT MODIFY THESE LINES
    core.code <- MagellanNTK::Get_Workflow_Core_Code(
      mode = "process",
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
        sidebar = tagList(),
        content = tagList(
          if (file.exists(file))
            includeMarkdown(file)
          else
            p('No Description available')
        )
      )
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Description', btnEvents()))
      req(dataIn())
      req(inherits(dataIn(), 'QFeatures'))
      
      shiny::withProgress(message = paste0("Preparing data", id), {
        shiny::incProgress(0.5)
        
        # Find the assay containing the hypothesis tests comparisons
        .ind <- unlist(lapply(seq(length(dataIn())), function(x){
          if(!is.null(DaparToolshed::HypothesisTest(dataIn()[[x]])))
            x
        }))
        
        # Copy the input dataset to use it during this step
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
        rv.custom$thlogfc <- as.numeric(.history[.ind_logFC, 'Value'])
        
        rv.custom$Pairwisecomparison_pushPval_SummaryDT <- data.frame(
          comparison = "-",
          query = "-",
          nbPushed = "0",
          TotalPushed = '0',
          TotalNonPushed = nrow(rv$dataIn[[length(rv$dataIn)]]),
          stringsAsFactors = FALSE
        )
        
        rv.custom$comparisonNames <- Get_Pairwisecomparison_Names(rv.custom$res_AllPairwiseComparisons)
        rv.custom$pushed <- setNames(rep(list(NULL), length(rv.custom$comparisonNames)), rv.custom$comparisonNames)
        
        # DO NOT MODIFY THE NEXT THREE LINES
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status['Description'] <- MagellanNTK::stepStatus$VALIDATED
        shiny::incProgress(1)
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
        choices = c('None', rv.custom$comparisonNames),
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
      print("obs>0")
      
      if (length(.ind) > 1 && length(.ind) <= nrow(Get_Dataset_to_Analyze())) {
        print("in if obs >0")
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
    })
     
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
      req(rv$dataIn)
      
      if (rv.widgets$Pairwisecomparison_Comparison == "None" || is.null(rv$dataIn)) {
        shinyjs::info(btnVentsMasg)
        
      } else {
        shiny::withProgress(message = paste0("Preparing contrast", id), {
          shiny::incProgress(0.5)
          
          rv.custom$resAnaDiff$pushed <- length(unlist(rv.custom$pushed[rv.widgets$Pairwisecomparison_Comparison]))
          query_list <- unlist(rv.custom$Pairwisecomparison_pushPval_SummaryDT_comp[, "query"])
          if (length(query_list) > 1){
            rv.custom$step1_query <- paste(query_list[-1], sep = " ; ")
          } else {
            rv.custom$step1_query <- "-"
          }
          
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DA', 'Pairwisecomparison', 'Comparison', rv.widgets$Pairwisecomparison_Comparison)
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DA', 'Pairwisecomparison', 'Push pval query', rv.custom$step1_query)
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DA', 'Pairwisecomparison', 'Nb pushed pval', rv.custom$resAnaDiff$pushed)
          
          rv.custom$containsNA <- checkNA(rv$dataIn)
          
          # DO NOT MODIFY THE NEXT THREE LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["Pairwisecomparison"] <- MagellanNTK::stepStatus$VALIDATED
          shiny::incProgress(1)
        })
      }
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
      widget <- selectInput(ns("Pvaluecalibration_calibrationMethod"), "Calibration method",
        choices = c("None" = "None", GetCalibMethod()),
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
        value = widget_value, 
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
    })
    
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
        choices = c(1, seq(from = 10, to = 100, by = 10)),
        selected = rv.widgets$Pvaluecalibration_nBinsHistpval, 
        width = "100px")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pvaluecalibration"])
    })
    
    #### _content -----
    calibrationAllData <- reactive({
      req(rv.custom$resAnaDiff)
      req(rv$dataIn)
      req(length(rv.custom$resAnaDiff$logFC) > 0)
      req(!rv.custom$containsNA)
      
      getPValueNonPushed(rv.custom$resAnaDiff$P_Value)
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
          catchToList(DaparToolshed::wrapperCalibrationPlot(dat,
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
      req(rv$dataIn)
      req(rv.custom$resAnaDiff)
      req(length(rv.custom$resAnaDiff$logFC) > 0)
      req(!rv.custom$containsNA)
      
      pval <- getPValueNonPushed(rv.custom$resAnaDiff$P_Value)
      
      calibration_value <- get_calibration_method(rv.widgets$Pvaluecalibration_calibrationMethod,
        debounced_numericVal())
      
      list(pval = pval, calibration_value = calibration_value)
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
              DaparToolshed::wrapperCalibrationPlot(dat$pval,
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
      req(!rv.custom$containsNA)
      req(rv.custom$resAnaDiff)
      req(rv.custom$pi0)
      req(rv.widgets$Pvaluecalibration_nBinsHistpval)
      
      DaparToolshed::histPValue_HC(getPValueNonPushed(rv.custom$resAnaDiff$P_Value),
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
      req(rv$dataIn)
        
      if (is.null(rv$dataIn))
        shinyjs::info(btnVentsMasg)
      
      else {
        shiny::withProgress(message = paste0("Running P-value calibration", id), {
          shiny::incProgress(0.5)
          
          rv.custom$GetCalibrationMethod <- get_calibration_method(
            calibration_method = rv.widgets$Pvaluecalibration_calibrationMethod,
            numeric_value = rv.widgets$Pvaluecalibration_numericValCalibration
          )
          
          result <- pvalCalibrationProt(history = rv.custom$history,
                                        calibmet = rv.custom$GetCalibrationMethod,
                                        pi0 = rv.custom$calibrationRes$pi0,
                                        h1concent = rv.custom$calibrationRes$h1.concentration,
                                        unifunder = rv.custom$calibrationRes$unif.under)
          rv.custom$history <- result$history
          
          
          rv.widgets$FDR_tooltipInfo <- rv.widgets$Pairwisecomparison_tooltipInfo
          rv.custom$FDR_tooltipInfo <- rv.widgets$Pairwisecomparison_tooltipInfo
          
          # DO NOT MODIFY THE NEXT THREE LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["Pvaluecalibration"] <- MagellanNTK::stepStatus$VALIDATED
          shiny::incProgress(1)
        })
      }
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
          uiOutput(ns('FDR_widgets_ui')),
          uiOutput(ns('showFDR_UI')),
          tags$hr(),
          uiOutput(ns('FDR_showHideDT_UI')),
          uiOutput(ns('FDR_viewAdjPval_UI'))
        ),
        content = tagList(div(style = "display: flex; gap: 20px;",
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
                                                       remoteReset = reactive({remoteReset()}),
                                                       is.enabled = reactive({rv$steps.enabled["FDR"]}))
    
    observeEvent(logpval(), {
      req(logpval())
      
      rv.custom$thpval <- as.numeric(gsub(",", ".", logpval(), fixed = TRUE))
      
      txt <- fdr_warning_text(
        Get_FDR(),
        Get_Nb_Significant()
      )
      
      if (!is.null(txt)) {
        MagellanNTK::mod_errorModal_server(
          "warn_FDR",
          title = "Warning",
          text = txt
        )
      }
    })
    
    output$showFDR_UI <- renderUI({
      fdr <- Get_FDR()
      req(fdr)
      txt <- "FDR = NA"
      if (!is.infinite(fdr)) {
        txt <- paste0("FDR = ", round(100 * fdr, digits = 2), " %")
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
      req(rv.custom$thpval)
      req(rv$dataIn)
      req(Pval_table())

      nbSelectedAnaDiff <- count_selected_prot(pval_table = Pval_table(), 
                                                          thpval = rv.custom$thpval, 
                                                          thlogfc = rv.custom$thlogfc)
      nbTotalAnaDiff <- nrow(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]]))
      
      ##
      ## Condition: A = C + D
      ##
      A <- nbTotalAnaDiff
      B <- A - rv.custom$resAnaDiff$pushed
      C <- nbSelectedAnaDiff
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
      df <- Pval_table()
      
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
          backgroundColor = DT::styleEqual(c(0, 1), c("white", "#E97D5E"))
        )
      
    })
    
    output$FDR_download_SelectedItems_UI <- downloadHandler(
      filename = function() {rv.custom$filename},
      content = function(fname) {
        wb <- build_pairwise_comparison_workbook(
          pval_table = Pval_table(),
          comparison = rv.widgets$Pairwisecomparison_Comparison
        )
        openxlsx::saveWorkbook(wb, file = fname, overwrite = TRUE)
      }
    )
    
    Get_FDR <- reactive({
      req(rv.custom$thpval)
      req(rv.custom$thlogfc)
      req(Pval_table())
      
      compute_fdr(
        pval_table = Pval_table(),
        thpval = rv.custom$thpval,
        thlogfc = rv.custom$thlogfc
      )
    })
    
    Get_Nb_Significant <- reactive({
      count_significant(
        Pval_table(),
        rv.widgets$Pairwisecomparison_Comparison
      )
    })
    
    Pval_table <- reactive({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.custom$thlogfc)
      req(rv.custom$thpval)
      req(rv.custom$GetCalibrationMethod)
      
      build_pval_table(
        data = rv$dataIn[[length(rv$dataIn)]],
        comparison = rv.widgets$Pairwisecomparison_Comparison,
        thlogfc = rv.custom$thlogfc,
        thpval = rv.custom$thpval,
        calibration_method = rv.custom$GetCalibrationMethod,
        tooltip_info = rv.custom$FDR_tooltipInfo
      )
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('FDR', btnEvents()))
      req(rv$dataIn)
      
      if (is.null(rv$dataIn) || is.null(rv.custom$thpval))
        shinyjs::info(btnVentsMasg)
      
      else {
        shiny::withProgress(message = paste0("Computing FDR", id), {
          shiny::incProgress(0.5)
          # Add informations to history
          result <- fdrProt(history = rv.custom$history,
                               thpval = rv.custom$thpval,
                               FDR = Get_FDR(),
                               nbSignif = Get_Nb_Significant())
          rv.custom$history <- result$history
          
          # DO NOT MODIFY THE NEXT THREE LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["FDR"] <- MagellanNTK::stepStatus$VALIDATED
          shiny::incProgress(1)
        })
      }
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
    # Save text (before saving)
    output$save_txt <- renderUI({
      req(rv$steps.status['Save'] != MagellanNTK::stepStatus$VALIDATED)
      req(config@mode == 'process')
      
      save_txt_ui()
    })
    
    # Download (ui) (after saving)
    output$dl_ui <- renderUI({
      req(rv$steps.status['Save'] == MagellanNTK::stepStatus$VALIDATED)
      req(config@mode == 'process')
      
      Prostar2::download_dataset_ui(ns(paste0(id, '_createQuickLink')))
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Save', btnEvents()))

      if (isTRUE(all.equal(SummarizedExperiment::assays(rv$dataIn), SummarizedExperiment::assays(dataIn()))))
        shinyjs::info(btnVentsMasg)
        
      else {
        shiny::withProgress(message = paste0("Saving process", id), {
          shiny::incProgress(0.5)
          # Do some stuff
          rv$dataIn <- prepareQFsave(data = rv$dataIn, 
                                     history = rv.custom$history,
                                     namePipeline = 'PipelineProtein')
          
          # Add the result of pairwise comparison to the coldata
          DaparToolshed::DifferentialAnalysis(rv$dataIn[[length(rv$dataIn)]]) <- Pval_table()
          
          # DO NOT MODIFY THE NEXT THREE LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- rv$dataIn
          rv$steps.status['Save'] <- MagellanNTK::stepStatus$VALIDATED
          
          # Download (server)
          Prostar2::download_dataset_server(paste0(id, '_createQuickLink'), dataIn = reactive({dataOut$value}))
          shiny::incProgress(1)
        })
      }
    })

    ####### _END_ -----
    
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  })
}
