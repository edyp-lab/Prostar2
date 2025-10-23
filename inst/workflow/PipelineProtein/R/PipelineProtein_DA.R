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
#' library(highcharter)
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
#' shiny::runApp(workflowApp("PipelineProtein_DA", path, dataIn = obj))
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
    mandatory = c(FALSE, FALSE, FALSE)
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
  pkgs.require('magrittr')
  
  pkgs.require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))
  
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list(
    Pairwisecomparison_Comparison = "None",
    Pairwisecomparison_tooltipInfo = NULL,
    Pvaluecalibration_numericValCalibration = "None",
    Pvaluecalibration_calibrationMethod = "Benjamini-Hochberg",
    Pvaluecalibration_nBinsHistpval = 80,
    FDR_viewAdjPval = FALSE
  )
  
  
  rv.custom.default.values <- list(
    tmp.dataIn = NULL,
    resAnaDiff = NULL,
    res_AllPairwiseComparisons = NULL,
    Pairwisecomparison_tooltipInfo = NULL,
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
    history = list(),
    step1_query = '-'
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
    
    pkgs.require('grDevices')
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
    add.resourcePath()
    
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
        paste0(id, '.Rmd')))
      
      MagellanNTK::process_layout(
        ns = NS(id),
        sidebar = tagList(),
        #timeline_process_ui(ns('Description_timeline')),
        content = tagList(
          if (file.exists(file))
            includeMarkdown(file)
          else
            p('No Description available')
        )
      )
    })
    
    output$datasetDescription_UI <- renderUI({
      # Insert your own code to visualize some information
      # about your dataset. It will appear once the 'Start' button
      # has been clicked
      
    })
    
    # output$Description_btn_validate_UI <- renderUI({
    #   widget <- actionButton(ns("Description_btn_validate"),
    #     "Start",
    #     class = "btn-success")
    #   MagellanNTK::toggleWidget(widget, rv$steps.enabled['Description'])
    # })
    
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Description', btnEvents()))
      
      req(inherits(dataIn(), 'QFeatures'))
      
      # Find the assay containing the hypothesis tests comparisons
      .ind <- unlist(lapply(seq(length(dataIn())), function(x){
        if(!is.null(DaparToolshed::HypothesisTest(dataIn()[[x]])))
          x
      }))
      #rv$dataIn <- dataIn()[[.ind]]
      rv$dataIn <- dataIn()
      
      # Adds an assay to work on
      rv$dataIn <- addAssay(
        rv$dataIn, 
        rv$dataIn[[length(rv$dataIn)]], 
        'DA')
      
      rv.custom$res_AllPairwiseComparisons <- DaparToolshed::HypothesisTest(rv$dataIn[[length(rv$dataIn)]])
      rv.widgets$Pairwisecomparison_tooltipInfo <- idcol(rv$dataIn[[length(rv$dataIn)]])

      
      # Get logfc threshold from Hypothesis test dataset
      .se <- rv$dataIn[[length(rv$dataIn)]]
      .thlogfc <- paramshistory(.se)[['HypothesisTest_thlogFC']]
      if(!is.null(.thlogfc))
        rv.custom$thlogfc <- .thlogfc
      
      paramshistory(.se) <- NULL
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- stepStatus$VALIDATED
    })
    
    
    
    
    
    Get_Dataset_to_Analyze <- reactive({
      req(rv.widgets$Pairwisecomparison_Comparison != 'None')
      datasetToAnalyze <- NULL
      print('In Get_Dataset_to_Analyze()......')
      .split <- strsplit(
        as.character(rv.widgets$Pairwisecomparison_Comparison), "_vs_"
      )
      rv.custom$Condition1 <- .split[[1]][1]
      rv.custom$Condition2 <- .split[[1]][2]
      
      rv.custom$filename <- paste0("anaDiff_", rv.custom$Condition1,
        "_vs_", rv.custom$Condition2, ".xlsx")
      
      
      if (length(grep("all-", rv.widgets$Pairwisecomparison_Comparison)) == 1) {
        .conds <- DaparToolshed::design.qf(rv$dataIn)$Condition
        condition1 <- strsplit(as.character(rv.widgets$Pairwisecomparison_Comparison), "_vs_")[[1]][1]
        ind_virtual_cond2 <- which(.conds != condition1)
        datasetToAnalyze <- rv$dataIn[[length(rv$dataIn)]]
        #colData(datasetToAnalyze)$Condition[ind_virtual_cond2] <- "virtual_cond_2"
      } else {
        ind <- c(
          which(DaparToolshed::design.qf(rv$dataIn)$Condition == rv.custom$Condition1),
          which(DaparToolshed::design.qf(rv$dataIn)$Condition == rv.custom$Condition2)
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
    
    
    GetComparisons <- reactive({
      req(rv.widgets$Pairwisecomparison_Comparison != 'None')
      req(rv.custom$Condition1)
      req(rv.custom$Condition2)
      c(rv.custom$Condition1, rv.custom$Condition2)
    })
    
    
    
    # >>>
    # >>> START ------------- Code for step 1 UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI ------------------------------------
    output$Pairwisecomparison <- renderUI({
      shinyjs::useShinyjs()
      
      .style <- "display:inline-block; vertical-align: top; padding-right: 60px"
      
      
      MagellanNTK::process_layout(
        ns = NS(id),
        sidebar = tagList(
          #timeline_process_ui(ns('Pairwisecomparison_timeline')),
          tags$div(
            uiOutput(ns('Pairwisecomparison_Comparison_UI')),
            uiOutput(ns("pushpval_UI"))
          )
        ),
        content = div(
          uiOutput(ns("Pairwisecomparison_tooltipInfo_UI")),
          uiOutput(ns("Pairwisecomparison_volcano_UI"))
        )
      )
      


    })
    
    
    Get_Pairwisecomparison_Names <- reactive({
      req(rv.custom$res_AllPairwiseComparisons)
      
      .names <- colnames(rv.custom$res_AllPairwiseComparisons)
      .names <- gsub('_logFC', '', .names, fixed = TRUE)
      .names <- gsub('_pval', '', .names, fixed = TRUE)
      
      .names <- unique(.names)
      .names
    })
    
    
    output$Pairwisecomparison_Comparison_UI <- renderUI({
      req(rv.custom$res_AllPairwiseComparisons)
      
      widget <- selectInput(ns("Pairwisecomparison_Comparison"), "Select a comparison",
        choices = c('None', Get_Pairwisecomparison_Names()),
        selected = rv.widgets$Pairwisecomparison_Comparison,
        width = "300px")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pairwisecomparison"])
    })
    

    
    Prostar2::mod_volcanoplot_server(
      id = "Pairwisecomparison_volcano",
      dataIn = reactive({Get_Dataset_to_Analyze()}),
      comparison = reactive({c(rv.custom$Condition1, rv.custom$Condition2)}),
      group = reactive({DaparToolshed::design.qf(rv$dataIn)$Condition}),
      thlogfc = reactive({rv.custom$thlogfc}),
      tooltip = reactive({rv.custom$Pairwisecomparison_tooltipInfo}),
      remoteReset = reactive({remoteReset()})
    )
    #})
    
    output$Pairwisecomparison_volcano_UI <- renderUI({
      widget <- div(
        mod_volcanoplot_ui(ns("Pairwisecomparison_volcano"))
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pairwisecomparison"])
    })
    
    
    output$Pairwisecomparison_tooltipInfo_UI <- renderUI({
      req(rv$dataIn)
      
      widget <- fluidRow(
        column(width = 6,
          MagellanNTK::mod_popover_for_help_ui(ns("modulePopover_volcanoTooltip")),
        selectInput(ns("Pairwisecomparison_tooltipInfo"),
          label = NULL,
          choices = colnames(SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])),
          selected = rv.widgets$Pairwisecomparison_tooltipInfo,
          multiple = TRUE,
          selectize = FALSE,
          width = "300px", 
          size = 5
        )),
        column(width = 6,
          actionButton(ns("Pairwisecomparison_validTooltipInfo"),  "Validate tooltip choice", 
          class = "btn-info")
        )
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pairwisecomparison"])
    })
    
    
    
    observeEvent(input$Pairwisecomparison_validTooltipInfo, {
      rv.custom$Pairwisecomparison_tooltipInfo <- rv.widgets$Pairwisecomparison_tooltipInfo
    })
    
    MagellanNTK::mod_popover_for_help_server("modulePopover_volcanoTooltip",
      title = "Tooltip",
      content = "Infos to be displayed in the tooltip of volcanoplot"
    )
    
    
    MagellanNTK::mod_popover_for_help_server("modulePopover_pushPVal",
      title = h3("Push p-value"),
      content = "This functionality is useful in case of multiple pairwise comparisons 
              (more than 2 conditions): At the filtering step, a given analyte X
              (either peptide or protein) may have been kept because it contains
              very few missing values in a given condition (say Cond. A), even
              though it contains (too) many of them in all other conditions
              (say Cond B and C only contains 'MEC' type missing values).
              Thanks to the imputation step, these missing values are no
              longer an issue for the differential analysis, at least from
              the computational viewpoint. However, statistically speaking,
              when performing B vs C, the test will rely on too many imputed
              missing values to derive a meaningful p-value: It may be wiser
              to consider analyte X as non-differentially abundant, regardless
              the test result (and thus, to push its p-value to 1). This is just
              the role of the P-value push parameter. It makes it possible to
              introduce a new filtering step that only applies to each pairwise
              comparison, and which assigns a p-value of 1 to analytes that, for
              the considered comparison are assumed meaningless due to too many
              missing values (before imputation)."
    )
    
    
    output$pushpval_UI <- renderUI({
      req(rv.widgets$Pairwisecomparison_Comparison != "None")
      
      widget <- tagList(
        MagellanNTK::mod_popover_for_help_ui(ns("modulePopover_pushPVal")),
        div(
          mod_qMetacell_FunctionFilter_Generator_ui(ns("AnaDiff_query"))
        )
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pairwisecomparison"])
      
      ##############################
      
    })
    
    
    
    
    GetFiltersScope <- function(){
      c("Whole Line" = "WholeLine",
        "Whole matrix" = "WholeMatrix",
        "For every condition" = "AllCond",
        "At least one condition" = "AtLeastOneCond"
      )
    }
    
    
    
    
    observe({
      req(rv$dataIn)
      req(Get_Dataset_to_Analyze())
      rv.custom$AnaDiff_indices <- Prostar2::mod_qMetacell_FunctionFilter_Generator_server(
        id = "AnaDiff_query",
        dataIn = reactive({Get_Dataset_to_Analyze()}),
        conds = reactive({DaparToolshed::design.qf(rv$dataIn)$Condition}),
        keep_vs_remove = reactive({
          stats::setNames(c('Push p-value', 'Keep original p-value'), 
            nm = c("delete", "keep"))}),
        remoteReset = reactive({0}),
        is.enabled = reactive({rv$steps.enabled["Pairwisecomparison"]})
      )
    })
    
    
    observeEvent(req(rv.custom$AnaDiff_indices()$trigger),{
      .ind <- rv.custom$AnaDiff_indices()$value$ll.indices
      .cmd <- rv.custom$AnaDiff_indices()$value$ll.widgets.value[[1]]$keep_vs_remove
      
      
      if (length(.ind) > 1 && length(.ind) < nrow(Get_Dataset_to_Analyze())) {
        
        if (.cmd == 'delete')
          indices_to_push <- .ind
        else if (.cmd == 'keep')
          indices_to_push <- seq_len(nrow(Get_Dataset_to_Analyze()))[-(.ind)]
        
        
        .pval <- paste0(rv.widgets$Pairwisecomparison_Comparison, '_pval')
        
        DaparToolshed::HypothesisTest(rv$dataIn[[length(rv$dataIn)]])[indices_to_push, .pval] <- 1
        rv.custom$res_AllPairwiseComparisons <- DaparToolshed::HypothesisTest(rv$dataIn[[length(rv$dataIn)]])
        rv.custom$history[['Number of pushed values to 1']] <- length(indices_to_push)
        
        n <- length(rv.custom$resAnaDiff$P_Value)
        rv.custom$pushed <- seq(n)[indices_to_push]
        rv.custom$resAnaDiff$pushed <- length(indices_to_push)
        rv.custom$step1_query <- rv.custom$AnaDiff_indices()$value$ll.query

      }
    })
    
    
    
    MagellanNTK::mod_popover_for_help_server("modulePopover_keepLines",
      title = "n values",
      content = "Keep the lines which have at least n intensity values."
    )
    
    not_a_numeric <- function(input) {
      if (is.na(as.numeric(input))) {
        "Please input a number"
      } else {
        NULL
      }
    }
    
    
    # output$Pairwisecomparison_btn_validate_ui <- renderUI({
    #   widget <- actionButton(ns("Pairwisecomparison_btn_validate"),
    #     "Validate step",
    #     class = "btn-success"
    #   )
    #   MagellanNTK::toggleWidget(widget,  rv$steps.enabled["Pairwisecomparison"])
    # })
    # >>> END: Definition of the widgets
    
    
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      
      req(grepl('Pairwisecomparison', btnEvents()))
      
     
      if ( rv.widgets$Pairwisecomparison_Comparison == widgets.default.values$Pairwisecomparison_Comparison 
        || is.null(rv$dataIn))
        info(btnVentsMasg)
      else {
      #UpdateCompList()
     
      rv.custom$history[['Push pval query']] <- rv.custom$step1_query
      rv.custom$history[['Comparison']] <- GetComparisons()
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status["Pairwisecomparison"] <- stepStatus$VALIDATED
      }
    })
    
    
    # <<< END ------------- Code for step 1 UI---------------
    
    
    # >>> START ------------- Code for step 2 UI---------------
    output$Pvaluecalibration <- renderUI({

      MagellanNTK::process_layout(
        ns = NS(id),
        sidebar = tagList(
         # timeline_process_ui(ns('Pvaluecalibration_timeline')),
          uiOutput(ns('Pvaluecalibration_calibrationMethod_UI')),
          uiOutput(ns("Pvaluecalibration_numericValCalibration_UI")),
          uiOutput(ns("Pvaluecalibration_nBins_UI"))
        ),
        content = div(
          p(tags$strong(
            paste0("value of pi0: ", round(as.numeric(rv.custom$pi0), digits = 2))
          )),
          fluidRow(
            column(width = 6, fluidRow(style = "height:800px;",
              imageOutput(ns("calibrationPlotAll"), height = "800px")
            )),
            column(width = 6, fluidRow(style = "height:400px;",
              imageOutput(ns("calibrationPlot"), height = "400px")
            ),
              fluidRow(style = "height:400px;", 
                highchartOutput(ns("histPValue")))
            )
          )
        )
      )
      
      
    })
    
    
    
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
      widget <- numericInput(ns("Pvaluecalibration_numericValCalibration"),
        "Proportion of TRUE null hypothesis",
        value = rv.widgets$Pvaluecalibration_numericValCalibration,
        min = 0,
        max = 1,
        step = 0.05
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pvaluecalibration"] &&
          rv.widgets$Pvaluecalibration_calibrationMethod == "numeric value")
    })
    
    
    output$Pvaluecalibration_nBins_UI <- renderUI({
      req(rv.custom$resAnaDiff)
      req(rv.custom$pi0)
      
      widget <- selectInput(
        ns("Pvaluecalibration_nBinsHistpval"), 
        "n bins of p-value histogram",
        choices = c(1, seq(from = 0, to = 100, by = 10)[-1]),
        selected = rv.widgets$Pvaluecalibration_nBinsHistpval, 
        width = "80px")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pvaluecalibration"])
    })
    
    
    histPValue <- reactive({
      #browser()
      req(rv.custom$resAnaDiff)
      req(rv.custom$pi0)
      req(rv.widgets$Pvaluecalibration_nBinsHistpval)
      req(rv.custom$thlogfc)
      req(!is.na(rv.custom$thlogfc))
      req(length(rv.custom$resAnaDiff$logFC) > 0)
      
      
      m <- DaparToolshed::match.metacell(DaparToolshed::qMetacell(rv$dataIn[[length(rv$dataIn)]]),
        pattern = c("Missing", "Missing POV", "Missing MEC"),
        level = DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]])
      )
      req(length(which(m)) == 0)
      
      t <- NULL
      method <- NULL
      t <- rv.custom$resAnaDiff$P_Value
      t <- t[which(abs(rv.custom$resAnaDiff$logFC) >= rv.custom$thlogfc)]
      toDelete <- which(t == 1)
      if (length(toDelete) > 0) {
        t <- t[-toDelete]
      }
      
      histPValue_HC(t,
        bins = as.numeric(rv.widgets$Pvaluecalibration_nBinsHistpval),
        pi0 = rv.custom$pi0
      )
      
    })
    
    output$histPValue <- highcharter::renderHighchart({
      histPValue()
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
    
    
    
    
    calibrationPlot <- reactive({
      #req(rv.widgets$Pvaluecalibration_calibrationMethod != "None")
      
      req(rv.custom$resAnaDiff)
      req(rv$dataIn)
      req(length(rv.custom$resAnaDiff$logFC) > 0)
      
      m <- DaparToolshed::match.metacell(DaparToolshed::qMetacell(rv$dataIn[[length(rv$dataIn)]]),
        pattern = c("Missing", "Missing POV", "Missing MEC"),
        level = "peptide")
      req(length(which(m)) == 0)
      
      t <- NULL
      method <- NULL
      t <- rv.custom$resAnaDiff$P_Value
      t <- t[which(abs(rv.custom$resAnaDiff$logFC) >= rv.custom$thlogfc)]
      toDelete <- which(t == 1)
      if (length(toDelete) > 0) {
        t <- t[-toDelete]
      }
      
      
      l <- NULL
      ll <- NULL
      result <- tryCatch(
        {
          if ((rv.widgets$Pvaluecalibration_calibrationMethod == "numeric value") &&
              !is.null(rv.widgets$Pvaluecalibration_numericValCalibration)) {
            
            ll <- catchToList(
              wrapperCalibrationPlot(
                t,
                rv.widgets$Pvaluecalibration_numericValCalibration
              )
            )
            .warns <- ll$warnings[grep("Warning:", ll$warnings)]
            rv.custom$errMsgCalibrationPlot <- .warns
          } else if (rv.widgets$Pvaluecalibration_calibrationMethod == "Benjamini-Hochberg") {
            ll <- catchToList(wrapperCalibrationPlot(t, 1))
            .warns <- ll$warnings[grep("Warning:", ll$warnings)]
            rv.custom$errMsgCalibrationPlot <- .warns
          } else {
            ll <- catchToList(
              wrapperCalibrationPlot(t, rv.widgets$Pvaluecalibration_calibrationMethod)
            )
            .warns <- ll$warnings[grep("Warning:", ll$warnings)]
            rv.custom$errMsgCalibrationPlot <- .warns
          }
          rv.custom$pi0 <- ll$value$pi0
          
        },
        warning = function(w) {
          shinyjs::info(paste("Calibration plot", ":",
            conditionMessage(w),
            sep = " "
          ))
        },
        error = function(e) {
          shinyjs::info(paste("Calibration plot", ":",
            conditionMessage(e),
            sep = " "
          ))
        },
        finally = {
          # cleanup-code
        }
      )
    })
    
    output$calibrationPlot <- renderImage(
      {
        outfile <- tempfile(fileext = ".png")
        
        # Generate a png
        grDevices::png(outfile, width = 600, height = 500)
        calibrationPlot()
        grDevices::dev.off()
        
        # Return a list
        list(
          src = outfile,
          alt = "This is alternate text"
        )
      },
      deleteFile = TRUE
    )
    
    
    
    
    output$errMsgCalibrationPlot <- renderUI({
      req(rv.custom$errMsgCalibrationPlot)
      req(rv$dataIn)
      
      txt <- NULL
      
      for (i in 1:length(rv.custom$errMsgCalibrationPlot)) {
        txt <- paste(txt, "errMsgCalibrationPlot: ",
          rv.custom$errMsgCalibrationPlot[i], "<br>",
          sep = ""
        )
      }
      
      div(HTML(txt), style = "color:red")
    })
    
    
    output$errMsgCalibrationPlotAll <- renderUI({
      rv.custom$errMsgCalibrationPlotAll
      req(rv$dataIn)
      req(!is.null(rv.custom$errMsgCalibrationPlotAll))
      
      txt <- NULL
      for (i in 1:length(rv.custom$errMsgCalibrationPlotAll)) {
        txt <- paste(txt, "errMsgCalibrationPlotAll:",
          rv.custom$errMsgCalibrationPlotAll[i], "<br>",
          sep = ""
        )
      }
      
      div(HTML(txt), style = "color:red")
    })
    
    
    
    calibrationPlotAll <- reactive({
      
      rv.custom$resAnaDiff
      req(rv$dataIn)
      req(!is.na(rv.custom$thlogfc))
      req(length(rv.custom$resAnaDiff$logFC) > 0) 
      
      m <- DaparToolshed::match.metacell(DaparToolshed::qMetacell(rv$dataIn[[length(rv$dataIn)]]),
        pattern = c("Missing", "Missing POV", "Missing MEC"),
        level = "peptide")
      req(length(which(m)) == 0)
      
      cond <- c(rv.custom$resAnaDiff$condition1, rv.custom$resAnaDiff$condition2)
      
      t <- NULL
      method <- NULL
      t <- rv.custom$resAnaDiff$P_Value
      t <- t[which(abs(rv.custom$resAnaDiff$logFC) >= rv.custom$thlogfc)]
      toDelete <- which(t == 1)
      if (length(toDelete) > 0) {
        t <- t[-toDelete]
      }
      
      l <- NULL
      result <- tryCatch(
        {
          l <- catchToList(wrapperCalibrationPlot(t, "ALL"))
          .warns <- l$warnings[grep("Warning:", l$warnings)]
          rv.custom$errMsgCalibrationPlotAll <- .warns
        },
        warning = function(w) {
          shinyjs::info(paste("Calibration Plot All methods", ":",
            conditionMessage(w),
            sep = " "
          ))
        },
        error = function(e) {
          shinyjs::info(paste("Calibration Plot All methods", ":",
            conditionMessage(e),
            sep = " "
          ))
        },
        finally = {
          # cleanup-code
        }
      )
    })
    
    
    output$calibrationPlotAll <- renderImage(
      {
        outfile <- tempfile(fileext = ".png")
        
        # Generate a png
        grDevices::png(outfile, width = 600, height = 500)
        calibrationPlotAll()
        grDevices::dev.off()
        
        # Return a list
        list(
          src = outfile,
          alt = "This is alternate text"
        )
      },
      deleteFile = TRUE
    )
    
    
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
    
    
    
    
    # output$Pvaluecalibration_btn_validate_ui <- renderUI({
    #   widget <- actionButton(ns("Pvaluecalibration_btn_validate"),
    #     "Validate step",
    #     class = "btn-success"
    #   )
    #   MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pvaluecalibration"])
    # })
    # >>> END: Definition of the widgets
    
    
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      

      req(grepl('Pvaluecalibration', btnEvents()))
      
      if (is.null(rv$dataIn))
        info(btnVentsMasg)
      else {
        
      rv.custom$history[['Calibration method']] <- GetCalibrationMethod()
      rv.custom$history[['pi0']] <- rv.custom$calibrationRes$pi0
      # rv.custom$history[['h1.concentration']] <- rv.custom$calibrationRes$h1.concentration
      # rv.custom$history[['Uniformity underestimation']] <- rv.custom$calibrationRes$unif.under
      # rv.custom$history[['Non-DA protein proportion']] <- round(100 * rv.custom$calibrationRes$pi0, digits = 2)
      # rv.custom$history[['DA protein concentration']] <- round(100 * rv.custom$calibrationRes$h1.concentration, digits = 2)
      # 
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status["Pvaluecalibration"] <- stepStatus$VALIDATED
      }
    })
    
    # <<< END ------------- Code for step 2 UI---------------
    
    
    # >>> START ------------- Code for step 2 UI---------------
    output$FDR <- renderUI({
      
      MagellanNTK::process_layout(
        ns = NS(id),
        sidebar = tagList(
          #timeline_process_ui(ns('FDR_timeline')),
          tags$div(
            uiOutput(ns('widgets_ui'))
          )
        ),
        content = div(
          uiOutput(ns("FDR_nbSelectedItems_ui")),
          fluidRow(
            column(width = 4,
              withProgress(message = "", detail = "", value = 1, {
            uiOutput(ns('FDR_volcanoplot_UI'))
          })),
          column(width = 8,
            downloadButton(ns("FDR_download_SelectedItems_UI"), 
            "Selected final results (Excel file)", class = "btn-info"),
            checkboxInput(ns('FDR_viewAdjPval'), 
              'View adjusted p-value', 
              value = rv.widgets$FDR_viewAdjPval),
          DT::DTOutput(ns("FDR_selectedItems_UI"))
          )
          )
        )
      )
    })
    
    
    output$widgets_ui <- renderUI({
      widget <- tags$div(
        mod_set_pval_threshold_ui(ns("Title"))
        )

      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDR"])
      
    })
    
    #-------------------------------------------------------------------
    #
    Prostar2::mod_volcanoplot_server(
      id = "FDR_volcano",
      dataIn = reactive({Get_Dataset_to_Analyze()}),
      comparison = reactive({GetComparisons()}),
      group = reactive({DaparToolshed::design.qf(rv$dataIn)$Condition}),
      thlogfc = reactive({rv.custom$thlogfc}),
      thpval = reactive({rv.custom$thpval}),
      tooltip = reactive({rv.custom$Pairwisecomparison_tooltipInfo}),
      remoteReset = reactive({0}),
      is.enabled = reactive({rv$steps.enabled["FDR"]})
    )
    
    output$FDR_volcanoplot_UI <- renderUI({
      widget <- div(
        mod_volcanoplot_ui(ns("FDR_volcano"))
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDR"])
    })
    
    
    output$FDR_nbSelectedItems_ui <- renderUI({
      rv.custom$thpval
      rv$dataIn
      req(Build_pval_table())
      
      
      m <- DaparToolshed::match.metacell(DaparToolshed::qMetacell(rv$dataIn[[length(rv$dataIn)]]),
        pattern = c("Missing", "Missing POV", "Missing MEC"),
        level = "peptide"
      )
      #req(length(which(m)) > 0)
      
      p <- Build_pval_table()
      upItemsPVal <- NULL
      upItemsLogFC <- NULL
      
      
      upItemsLogFC <- which(abs(p$logFC) >= as.numeric(rv.custom$thlogfc))
      upItemsPVal <- which(-log10(p$P_Value) >= as.numeric(
        rv.custom$thpval
      ))
      
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
      B <- A - length(rv.custom$pushed)
      C <- rv.custom$nbSelectedAnaDiff
      D <- ( A - C)
      # 
      # txt <- paste("Total number of ", rv$typeOfDataset, "(s) = ", A , "<br>",
      #   "\t <em>Total remaining after push p-values = ", B , "</em><br>",
      #     paste("Number of selected ", rv$typeOfDataset, "(s) = ", C, sep=''),
      #     paste("Number of non selected ", rv$typeOfDataset, "(s) = ", D, sep = ''),
      #     sep = ""
      # )
      
      rv.custom$history[['Total number']] <- A
      rv.custom$history[['Total remaining after push p-values']] <- B
      rv.custom$history[['Number of selected']] <- C
      rv.custom$history[['Number of non selected']] <- D
     
      
      
      div(id="bloc_page",
        style = "background-color: lightgrey; width: 300px",
        p(paste("Total number of ", DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]]), "(s) = ", A, sep = '' )),
        tags$em(p(style = "padding:0 0 0 20px;", 
          paste("Total remaining after push p-values = ", B, sep=''))),
        p(paste("Number of selected ", DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]]), "(s) = ", C, sep = '')),
        p(paste("Number of non selected ", DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]]), "(s) = ", D, sep = ''))
      )
      #HTML(txt)
    })
    
    
    
    #################################################################
    ###### Set code for widgets managment
    ################################################################
    
    logpval <- Prostar2::mod_set_pval_threshold_server(id = "Title",
      pval_init = reactive({10^(-rv.custom$thpval)}),
      fdr = reactive({Get_FDR()}),
      remoteReset = reactive({0}),
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
    
    
    
    output$FDR_selectedItems_UI <- DT::renderDT({
      req(rv$steps.status["Pvaluecalibration"] == stepStatus$VALIDATED)
      df <- Build_pval_table()
      
      if (rv.widgets$FDR_viewAdjPval){
        df <- df[order(df$Adjusted_PValue, decreasing=FALSE), ]
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
        options = list(initComplete = initComplete(),
          dom = "frtip",
          pageLength = 100,
          scrollY = 500,
          scroller = TRUE,
          server = FALSE,
          columnDefs = .coldefs,
          ordering = !rv.widgets$FDR_viewAdjPval
        )
      ) %>%
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
      req(Build_pval_table())
      
      adj.pval <- Build_pval_table()$Adjusted_PValue
      logpval <- Build_pval_table()$Log_PValue
      upitems_logpval <- which(logpval >= rv.custom$thpval)
      
      fdr <- max(adj.pval[upitems_logpval], na.rm = TRUE)
      rv.custom$FDR <- as.numeric(fdr)
      as.numeric(fdr)
    })
    
    # observeEvent(input$validate_pval,{
    #   
    # })
    
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
      req(rv$steps.status["Pvaluecalibration"] == stepStatus$VALIDATED)
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
      upItems_logFC <- which(abs(.logfc) >= rv.custom$thlogfc)
      rv.custom$adjusted_pvalues <- diffAnaComputeAdjustedPValues(
        .pval[upItems_logFC],
        GetCalibrationMethod())
      pval_table[upItems_logFC, 'Adjusted_PValue'] <- rv.custom$adjusted_pvalues
      
      
      
      # Set only significant values
      pval_table$logFC <- signif(pval_table$logFC, digits = 4)
      pval_table$P_Value <- signif(pval_table$P_Value, digits = 4)
      pval_table$Adjusted_PValue <- signif(pval_table$Adjusted_PValue, digits = 4)
      pval_table$Log_PValue <- signif(pval_table$Log_PValue, digits = 4)
      
      
      
      tmp <- as.data.frame(
        SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])[, rv.custom$Pairwisecomparison_tooltipInfo]
      )
      names(tmp) <- rv.custom$Pairwisecomparison_tooltipInfo
      pval_table <- cbind(pval_table, tmp)
      
      colnames(pval_table)[2:6] <- paste0(colnames(pval_table)[2:6], " (", as.character(rv.widgets$Pairwisecomparison_Comparison), ")")
      
      pval_table
    })
    
    
    
    isContainedIn <- function(strA, strB) {
      return(all(strA %in% strB))
    }
    
    
    # output$FDR_btn_validate_UI <- renderUI({
    #   widget <- actionButton(ns("FDR_btn_validate"),
    #     "Validate step",
    #     class = "btn-success"
    #   )
    #   MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDR"])
    # })
    # >>> END: Definition of the widgets
    
    
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('FDR', btnEvents()))
      
      if (is.null(rv$dataIn) || is.null(rv.custom$thpval))
        info(btnVentsMasg)
      else {
        
      rv.custom$history[['th pval']] <- rv.custom$thpval
      rv.custom$history[['% FDR']] <- round(100 * Get_FDR(), digits = 2)
      rv.custom$history[['Nb significant']] <- Get_Nb_Significant()


      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status["FDR"] <- stepStatus$VALIDATED
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
        content = uiOutput(ns('dl_UI'))
      )
    })
    
    output$dl_UI <- renderUI({
      req(rv$steps.status['Save'] == stepStatus$VALIDATED)
      req(config@mode == 'process')
      
      MagellanNTK::download_dataset_ui(ns('createQuickLink'))
    })

    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {

      req(grepl('Save', btnEvents()))
      
      if (isTRUE(all.equal(assays(rv$dataIn),assays(dataIn()))))
        info(btnVentsMasg)
      else {
        
      # Do some stuff
      
      last.se <- length(rv$dataIn)
      paramshistory(rv$dataIn[[last.se]]) <- NULL
      paramshistory(rv$dataIn[[last.se]]) <- rv.custom$history
      
     
      # Add the result of pairwise comparison to the coldata
     DaparToolshed::DifferentialAnalysis(rv$dataIn[[last.se]]) <- Build_pval_table()
     

      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Save'] <- stepStatus$VALIDATED
      
      
      Prostar2::download_dataset_server('createQuickLink', 
        dataIn = reactive({rv$dataIn}))
      
      }
    })
    # <<< END ------------- Code for step 3 UI---------------
    
    
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  }
  )
}