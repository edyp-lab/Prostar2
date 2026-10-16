#' @title PipelinePeptide DifferentialAnalysis module
#'
#' @description
#' This module contains the differential analysis step of the peptide pipeline.
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
#'   Prostar2("PipelinePeptide_DifferentialAnalysis")
#' }
#' 
#' @name PipelinePeptide_DifferentialAnalysis
#' 
#' @importFrom stats setNames rnorm
#' @importFrom shinyjs useShinyjs
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' 
NULL


#' @rdname PipelinePeptide_DifferentialAnalysis
#' @export
#' 
PipelinePeptide_DifferentialAnalysis_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelinePeptide_DifferentialAnalysis',
    mode = 'process',
    steps = c("Scenario", "Hypothesis test", "Fine tuning", "P-value calibration", "FDR control"),
    mandatory = c(TRUE, TRUE, TRUE, TRUE, TRUE)
  )
}


#' @rdname PipelinePeptide_DifferentialAnalysis
#' @export
#'
PipelinePeptide_DifferentialAnalysis_ui <- function(id){
  ns <- NS(id)
}


#' @rdname PipelinePeptide_DifferentialAnalysis
#' @export
#' 
PipelinePeptide_DifferentialAnalysis_server <- function(id,
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
    Scenario_choice = "Contrast",
    
    Hypothesistest_test = "Limma",
    Hypothesistest_ttestOptions = "Student",
    Hypothesistest_thlogFC = 0,
    Hypothesistest_contrastchoice = NULL,
    Hypothesistest_uniquechoice = "None",
    
    Finetuning_cluster_protprofile = "Mean",
    Finetuning_cluster_method = "kmeans",
    Finetuning_cluster_nbclust = 2,
    Finetuning_aggreg_method = "MinPval",
    
    Pvaluecalibration_calibrationMethod = "Benjamini-Hochberg",
    Pvaluecalibration_numericValCalibration = "None",
    Pvaluecalibration_nBinsHistpval = 80,
    
    FDRcontrol_viewAdjPval = FALSE,
    FDRcontrol_volcanocontrast = NULL,
    FDRcontrol_Pairwisecomparison_tooltipInfo = NULL,
    FDRcontrol_cluster_plot_type = "Fixed", 
    FDRcontrol_aggreg_plot_type = "Fixed",
    FDRcontrol_aggreg_plot_rep = "Associated FC",
    FDRcontrol_cluster_plot_clust = NULL
  )
  
  # Default values for reactive values
  rv.custom.default.values <- list(
    history = MagellanNTK::InitializeHistory(),
    
    logFC_onevsone = NULL,
    logFC_onevsall = NULL,
    logFC_stacked = NULL,
    enable_Limma = NULL,
    containsNA = FALSE,
    logFC = NULL,
    listNamesComparaison = NULL,
    n = NULL,
    swap.history = NULL,
    res_pval_FC = NULL,
    res_pval_FC_complete = NULL,
    res_pval_FC_stacked = NULL,
    contrast_list = NULL,
    comparison = NULL,
    names_condition = NULL,
    Scenario_constratnames = NULL,
    res_pval_tmp = NULL,
    push_pval_data = NULL,
    pushPval_SummaryDT = data.frame(
      query = "-",
      nbPushed = "0",
      TotalPushed = "0",
      TotalNonPushed = "-",
      stringsAsFactors = FALSE),
    pushed = NULL,
    rowdatacolname = NULL,
    centered_means = NULL,
    colidx_pvalagg = NULL,
    AnaDiff_indices = reactive({NULL}),
    errMsgcalibrationPlotALL = NULL,
    errMsgCalibrationPlot = NULL,
    pi0 = NULL,
    adjusted_pvalues = NULL,
    FDRcontrol_thpval = 0,
    FDR = NULL,
    FDRcontrol_Pairwisecomparison_tooltipInfo = NULL
  )
  
  ### -------------------------------------------------------------###
  ###                                                              ###
  ### -------------------- MODULE SERVER --------------------------###
  ###                                                              ###
  ### -------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Code hosted by MagellanNTK to create the process
    # DO NOT MODIFY THESE LINES
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
      
      shiny::withProgress(message = paste0("Reseting process", id), {
        shiny::incProgress(0.5)
        
        # Copy the input dataset to use it during this step
        rv$dataIn <- dataIn()
        
        # Check if contains NA
        rv.custom$containsNA <- checkNA(rv$dataIn)
        
        # DO NOT MODIFY THE NEXT THREE LINES
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status['Description'] <- MagellanNTK::stepStatus$VALIDATED
        shiny::incProgress(1)
      })
    })

    ###########################################################################-
    #
    #--------------------------------SCENARIO-----------------------------------
    #
    ###########################################################################-
    output$Scenario <- renderUI({
      shinyjs::useShinyjs()
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns('Scenario_choice_UI'))
        ),
        content = tagList(
          uiOutput(ns('Scenario_warningNA_UI')),
          uiOutput(ns('Scenario_desc_UI'))
        )
      )
    })
    
    #### _sidebar -----
    # Widget - scenario choice
    output$Scenario_choice_UI <- renderUI({
      req(rv$dataIn)
      
      widget <- selectInput(ns("Scenario_choice"), "Scenario",
                            choices = c("Contrast", "Cluster", "Aggregation"),
                            selected = rv.widgets$Scenario_choice,
                            width = "200px")
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Scenario"])
    })
    
    #### _content -----
    # Txt warning if missing values in quantitative data
    output$Scenario_warningNA_UI <- renderUI({
      req(rv$dataIn)
      
      if (rv.custom$containsNA) {
        tags$b(style = "color: red;",
               "Your dataset contains missing values. Please filter or impute them before proceeding.")
      }
    })
    
    # Description of each scenario
    output$Scenario_desc_UI <- renderUI({
      scenarioInfos()
    })
    
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Scenario', btnEvents()))
      req(rv$dataIn)
      
      if (!(rv.widgets$Scenario_choice %in% c("Contrast", "Cluster", "Aggregation"))) {
        shinyjs::info(btnVentsMasg)
        
      } else if (rv.custom$containsNA){
        warntxt <- "Your dataset contains missing values. Please filter or impute them before proceeding."
        MagellanNTK::mod_errorModal_server('warn_NA',
                                           title = 'Warning',
                                           text = warntxt)
        
      } else {
        shiny::withProgress(message = paste0("Initializing scenario", id), {
          shiny::incProgress(0.5)
          
          if (rv.widgets$Scenario_choice == "Contrast"){
            # Get logFC
            rv.custom$logFC_onevsone <- as.data.frame(getlogFC(rv$dataIn, type = "OnevsOne"))
            rv.custom$logFC_onevsall <- as.data.frame(getlogFC(rv$dataIn, type = "OnevsAll"))
            rv.custom$logFC_stacked <- data.frame(cond1_vs_cond2_logFC = unlist(rv.custom$logFC_onevsone, use.names = FALSE))
          }
          rv.custom$logFC <- as.data.frame(rv.custom$logFC_stacked)
            
          # Check if limma can be used
          rv.custom$enable_Limma <- checkLimma(rv$dataIn)
          
          # Update history
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, "DifferentialAnalysis", "Scenario", "Scenario", rv.widgets$Scenario_choice)
          
          # DO NOT MODIFY THE NEXT THREE LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["Scenario"] <- MagellanNTK::stepStatus$VALIDATED
          shiny::incProgress(1)
        })
      }
    })
    
    
    ###########################################################################-
    #
    #---------------------------HYPOTHESIS TEST---------------------------------
    #
    ###########################################################################-
    output$Hypothesistest <- renderUI({
      shinyjs::useShinyjs()
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns('Hypothesistest_contrastchoice_ui')),
          uiOutput(ns('Hypothesistest_uniquechoice_ui')),
          uiOutput(ns('Hypothesistest_test_ui')),
          uiOutput(ns("Hypothesistest_ttestOptions_ui")),
          uiOutput(ns('Hypothesistest_thlogFC_ui')),
        ),
        content = tagList(
          uiOutput(ns('Hypothesistest_warning_conditions_ui')),
          uiOutput(ns('Hypothesistest_plots_ui'))
        )
      )
    })
    
    ### Common to all (Contrast/Cluster/Aggregation) -----
    # Widget - test choice
    output$Hypothesistest_test_ui <- renderUI({
      req(rv$dataIn)
      req(rv.custom$enable_Limma)
      
      methods <- c("None" = "None", "t-tests" = "ttests", "ANOVA" = "ANOVA")
      if (rv.custom$enable_Limma) {
        methods <- c(methods, "Limma" = "Limma")
      }
      
      widget <- selectInput(ns("Hypothesistest_test"), "Statistical test",
                            choices = methods,
                            selected = rv.widgets$Hypothesistest_test,
                            width = "200px")
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Hypothesistest'])
    })
    
    # Widget - parameters for t-test
    output$Hypothesistest_ttestOptions_ui <- renderUI({
      req(rv.widgets$Hypothesistest_method == "ttests")
      
      widget <- radioButtons(ns("Hypothesistest_ttestOptions"),
                             "t-tests options",
                             choices = c("Student", "Welch"),
                             selected = rv.widgets$Hypothesistest_ttestOptions,
                             width = "200px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Hypothesistest"])
    })
    
    
    # Txt if limma cannot be applied
    output$Hypothesistest_warning_conditions_ui <- renderUI({
      req(!rv.custom$enable_Limma)
      tagList(
        tags$p("Info: Limma has been disabled because the design of your dataset:"),
        tags$ul(
          tags$li(p("is of level 1 and contains more than 26 conditions,")),
          tags$li("is of level 2 or 3 and contains more than 9 conditions.")
        ),
        tags$p("Prostar does not currently handle these cases.")
      )
    })
    
    
    ### Contrast - Fold Change -----
    #### _sidebar -----
    # Widget - logFC value
    output$Hypothesistest_thlogFC_ui <- renderUI({
      req(rv$steps.status["Scenario"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Contrast")
      
      widget <- shinyWidgets::autonumericInput(
        ns("Hypothesistest_thlogFC"),
        label = "log(FC) threshold",
        value = isolate(rv.widgets$Hypothesistest_thlogFC),
        width = "200px",
        minimumValue = 0,
        decimalCharacter = ".",
        decimalPlaces = 2,
        modifyValueOnWheel = TRUE,
        align = "left"
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Hypothesistest'])
    })
    
    # Widget - contrast type
    output$Hypothesistest_contrastchoice_ui <- renderUI({
      req(rv$steps.status["Scenario"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Contrast")
      
      widget <- selectInput(ns("Hypothesistest_contrastchoice"), "Contrast type",
                            choices = c("Stacked" = "Stacked", "One vs One" = "Unique_OvO", "One vs All" = "Unique_OvA"),
                            selected = rv.widgets$Hypothesistest_contrastchoice,
                            width = "200px")
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Hypothesistest'])
    })
    
    # Widget - unique contrast choice
    output$Hypothesistest_uniquechoice_ui <- renderUI({
      req(rv$steps.status["Scenario"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Hypothesistest_contrastchoice %in% c("Unique_OvO", "Unique_OvA"))

      choices <- c("None", rv.custom$listNamesComparaison)
      
      widget <- selectInput(ns("Hypothesistest_uniquechoice"), "Contrast",
                            choices = choices,
                            selected = rv.widgets$Hypothesistest_uniquechoice,
                            width = "200px")
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Hypothesistest'])
    })
    
    #### _content -----
    # All content for contrast scenario
    output$Hypothesistest_plots_ui <- renderUI({
      req(rv$steps.status["Scenario"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv$dataIn)
      req(rv.widgets$Scenario_choice == "Contrast")
      
      tagList(
        plotly::plotlyOutput(ns("Hypothesistest_Plot")),
        uiOutput(ns("Hypothesistest_swapConds_ui"))
      )
    })
    
    # Plot - logFC
    output$Hypothesistest_Plot <- plotly::renderPlotly({
      req(rv$steps.status["Scenario"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Contrast")
      req(rv.widgets$Hypothesistest_thlogFC)
      req(rv.custom$logFC)
      logFC_th <- Extract_Value(rv.widgets$Hypothesistest_thlogFC, "numeric")
      req(!is.na(logFC_th))
      
      withProgress(message = "Computing plot...", detail = "", value = 0.5, {
        pal <- DaparToolshed::ExtendPalette(ncol(rv.custom$logFC), "Paired")
        
        DaparToolshed::hc_logFC_DensityPlot(
          df_logFC = rv.custom$logFC,
          th_logFC = logFC_th,
          pal = pal
        )
      })
    })
    
    # Change which logFC to show on the plot
    observeEvent(rv.widgets$Hypothesistest_contrastchoice, {
      req(rv$dataIn)
      
      # Get logFC
      rv.custom$logFC <- switch(rv.widgets$Hypothesistest_contrastchoice,
                   Unique_OvO = as.data.frame(rv.custom$logFC_onevsone),
                   Unique_OvA = as.data.frame(rv.custom$logFC_onevsall),
                   Stacked = as.data.frame(rv.custom$logFC_stacked)
      )
      
      # Get comparison names
      comp <- colnames(rv.custom$logFC)
      rv.custom$listNamesComparaison <- unlist(strsplit(comp, split = "_logFC"))
      # Get number of comparison
      rv.custom$n <- ncol(rv.custom$logFC)
      rv.custom$swap.history <- rep(FALSE, rv.custom$n)
    })
    
    # Widget - swap conditions
    output$Hypothesistest_swapConds_ui <- renderUI({
      req(rv.widgets$Hypothesistest_contrastchoice)
      req(rv.custom$listNamesComparaison)
      
      n <- length(rv.custom$listNamesComparaison)
      
      widget <- lapply(seq_len(n), function(i) {
        conds <- strsplit(rv.custom$listNamesComparaison[i], "_vs_", fixed = TRUE)[[1]]
        
        div(
          style = "margin-bottom: -15px;",
          div(
            style = "display: inline-block; margin-right: 10px;",
            checkboxInput(
              inputId = ns(paste0("compswap_", i)),
              label = NULL,
              value = rv.custom$swap.history[i],
              width = "100%"
            )
          ),
          div(
            style = "display: inline-block;",
            paste0(gsub("[()]", "", conds[1]), "   VS   ", gsub("[()]", "", conds[2]))
          )
        )
      })
      
      widget <- tagList(
        h3("Swap conditions"),
        widget
      )
      
      MagellanNTK::toggleWidget(
        widget,
        rv$steps.enabled["Hypothesistest"]
      )
    })
    
    # When changes in swap conditions checkbox
    observeEvent(lapply(
      seq_along(rv.custom$listNamesComparaison),
      function(i) {
        input[[paste0("compswap_", i)]]
      }
    ), ignoreInit = TRUE, {
      n <- length(rv.custom$listNamesComparaison)
      
      swap <- vapply(
        seq_len(n),
        function(i) {
          isTRUE(input[[paste0("compswap_", i)]])
        },
        logical(1)
      )
      ind.swap <- which(
        swap != rv.custom$swap.history
      )
      
      req(length(ind.swap) > 0)
      
      # Save checkbox state
      rv.custom$swap.history <- swap
      
      # Apply swaps
      for (i in ind.swap) {
        result <- swapConditions(
          logFC = rv.custom$logFC,
          i = i
        )
        
        # Swap comparison name
        colnames(rv.custom$logFC)[i] <- result$name
        
        # Swap logFC values
        rv.custom$logFC[, i] <- result$values
      }
    })
    
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Hypothesistest', btnEvents()))
      
      if (is.null(rv$dataIn)  || !(rv.widgets$Hypothesistest_test %in% c("ANOVA", "Limma")) 
          || (rv.widgets$Hypothesistest_contrastchoice %in% c("Unique_OvO", "Unique_OvA") & rv.widgets$Hypothesistest_uniquechoice == "None")) {
        shinyjs::info(btnVentsMasg)
        
      } else {
        # Perform hypothesis test sub-step
        switch(rv.widgets$Scenario_choice,
               Contrast = {
                 result <- hypothesisTestPeptContrast(data = rv$dataIn,
                                                      history = rv.custom$history,
                                                      method = rv.widgets$Hypothesistest_test,
                                                      ttest_type = rv.widgets$Hypothesistest_ttestOptions,
                                                      contrastchoice = rv.widgets$Hypothesistest_contrastchoice,
                                                      thlogFC = rv.widgets$Hypothesistest_thlogFC,
                                                      uniquechoice = rv.widgets$Hypothesistest_uniquechoice,
                                                      swap = rv.custom$swap.history)
                 # Update values
                 rv.custom$res_pval_FC_stacked <- result$AllPairwiseComp_stacked
                 rv.custom$res_pval_tmp <- result$res_pval_tmp
                 rv.custom$rowdatacolname <- result$rowdatacolname
                 rv.custom$push_pval_data <- result$push_pval_data
                 rv.custom$cond <- result$cond
                 rv.custom$contrast_list <- result$contrast_list
                 rv.custom$comparison <- result$comparison
                 rv.custom$names_condition <- result$names_condition
                 rv.custom$pushPval_SummaryDT <- result$SummaryDT
                 rv.widgets$FDRcontrol_Pairwisecomparison_tooltipInfo <- result$tooltipInfo 
                 rv.custom$FDRcontrol_Pairwisecomparison_tooltipInfo <- result$tooltipInfo 
               },
               
               Cluster = {
                 result <- hypothesisTestPeptCluster(data = rv$dataIn,
                                                     history = rv.custom$history,
                                                     method = rv.widgets$Hypothesistest_test,
                                                     ttest_type = rv.widgets$Hypothesistest_ttestOptions)
                 # Update values
                 rv.custom$res_clusters <- result$clusters
               },
               
               Aggregation = {
                 result <- hypothesisTestPeptAggregation(data = rv$dataIn,
                                                         history = rv.custom$history,
                                                         method = rv.widgets$Hypothesistest_test,
                                                         ttest_type = rv.widgets$Hypothesistest_ttestOptions)
                 # Update values
                 rv.custom$res_pval_FC_complete <- result$AllPairwiseComp
               })
        
        # Update values
        rv.custom$res_pval_FC <- result$AllPairwiseComp
        rv.custom$history <- result$history
        
        # DO NOT MODIFY THE NEXT THREE LINES
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status["Hypothesistest"] <- MagellanNTK::stepStatus$VALIDATED
      }
    })
    
    
    ###########################################################################-
    #
    #-----------------------------FINE TUNING-----------------------------------
    #
    ###########################################################################-
    output$Finetuning <- renderUI({
      shinyjs::useShinyjs()
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns('Finetuning_pushpval_param_ui')),
          uiOutput(ns('Finetuning_cluster_param_ui')),
          uiOutput(ns('Finetuning_cluster_param_kmeans_ui')),
          uiOutput(ns('Finetuning_aggregation_param_ui'))
        ),
        content = tagList(
          uiOutput(ns('Finetuning_pushpval_ui')),
          uiOutput(ns('Finetuning_cluster_ui')),
          uiOutput(ns('Finetuning_aggregation_ui'))
        )
      )
    })
    
    ### Contrast - Push p-val -----
    #### _sidebar -----
    # Widget - filter creation for push p-values (ui)
    output$Finetuning_pushpval_param_ui <- renderUI({
      req(rv$steps.status["Hypothesistest"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Contrast")
      
      widget <- tagList(
        h3("Push p-value"),
        Prostar2::mod_qMetacell_FunctionFilter_Generator_ui(ns("AnaDiff_query"))
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Finetuning"])
    })

    # Widget - filter creation for push p-values (server)
    observe({
      req(rv$steps.status["Hypothesistest"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Contrast")
      req(rv.custom$push_pval_data)
      req(rv.custom$res_pval_tmp)
      
      ## A MODIFIER QUAND RAJOUT DE ALL COMME CONTRAST
      # if (length(grep("all-", rv.widgets$Pairwisecomparison_Comparison)) == 1) {
      #   conds <- DaparToolshed::design_qf(rv$dataIn)$Condition
      # } else {
      #   split <- unlist(strsplit(rv.widgets$Pairwisecomparison_Comparison, "_vs_"))
      #   idxcond <- DaparToolshed::design_qf(rv$dataIn)$Condition %in% split
      #   conds <- DaparToolshed::design_qf(rv$dataIn)$Condition[idxcond]
      # }
      
      rv.custom$AnaDiff_indices <- Prostar2::mod_qMetacell_FunctionFilter_Generator_server(
        id = "AnaDiff_query",
        dataIn = reactive({rv.custom$push_pval_data}),
        conds = reactive({rv.custom$cond}),
        keep_vs_remove = reactive({stats::setNames(c('Push p-value', 'Keep original p-value'), nm = c("delete", "keep"))}),
        val_vs_percent = reactive({stats::setNames(nm = c("Count", "Percentage"))}),
        operator = reactive({stats::setNames(nm = DaparToolshed::SymFilteringOperators())}),
        remoteReset = reactive({remoteReset()}),
        is.enabled = reactive({rv$steps.enabled["Finetuning"]})
      )
    })

    observeEvent(req(length(rv.custom$AnaDiff_indices()$value$ll.fun) > 0),{
      req(rv.custom$res_pval_tmp)
      
      ind <- unlist(rv.custom$AnaDiff_indices()$value$ll.indices)
      browser()
      if (length(ind) > 0 && length(ind) <= nrow(rv.custom$res_pval_tmp)) {
        result <- pushPvalues(
          data = rv.custom$push_pval_data,
          ind = ind,
          command = rv.custom$AnaDiff_indices()$value$ll.widgets.value[[1]]$keep_vs_remove,
          query = rv.custom$AnaDiff_indices()$value$ll.query,
          comparison = rv.custom$comparison,
          dt = rv.custom$pushPval_SummaryDT,
          pushed = rv.custom$pushed
        )
        
        rv.custom$push_pval_data <- result$data
        #rv.custom$res_AllPairwiseComparisons <- DaparToolshed::HypothesisTest(result$data)
        rv.custom$pushPval_SummaryDT <- result$dt[, -1]
        rv.custom$pushed <- result$pushed
      } 
    })
    
    #### _content -----
    # Pushed p-values DT (ui)
    output$Finetuning_pushpval_ui <- renderUI({
      req(rv$steps.status["Hypothesistest"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Contrast")
      
      req(rv.custom$pushPval_SummaryDT)
      MagellanNTK::format_DT_ui(ns("dt"))
    })
    
    # Pushed p-values DT (server)
    MagellanNTK::format_DT_server("dt",
                                  dataIn = reactive({rv.custom$pushPval_SummaryDT}))
    
    
    ### Cluster - Clustering -----
    #### _sidebar -----
    output$Finetuning_cluster_param_ui <- renderUI({
      req(rv$steps.status["Hypothesistest"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Cluster")
      
      widget1 <- selectInput(ns("Finetuning_cluster_protprofile"), "Protein profile method",
                            choices = c("Mean", "Median"),
                            selected = rv.widgets$Finetuning_cluster_protprofile,
                            width = "200px")
      widget2 <- selectInput(ns("Finetuning_cluster_method"), "Clustering method", ###NB_ PAS BESOIN SI NCOND = 2
                             choices = c("kmeans", "affinityProp", "affinityPropReduced"),
                             selected = rv.widgets$Finetuning_cluster_method,
                             width = "200px")
      
      tagList(
        MagellanNTK::toggleWidget(widget1, rv$steps.enabled['Finetuning']),
        MagellanNTK::toggleWidget(widget2, rv$steps.enabled['Finetuning'])
      )
    })
    
    output$Finetuning_cluster_param_kmeans_ui <- renderUI({
      req(rv$steps.status["Hypothesistest"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Cluster")
      req(rv.widgets$Finetuning_cluster_method == "kmeans")
      
      widget <- numericInput(ns("Finetuning_cluster_nbclust"), "Number of clusters",
                             value = rv.widgets$Finetuning_cluster_nbclust,
                             min = 1,
                             step = 1,
                             width = "200px")
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Finetuning'])
    })
    
    #### _content -----
    output$Finetuning_cluster_ui <- renderUI({
      req(rv$steps.status["Hypothesistest"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Cluster")
      
      plotOutput(ns("Finetuning_cluster_plot"))
    })
    
    output$Finetuning_cluster_plot <- renderPlot({
      req(rv$steps.status["Hypothesistest"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Cluster")
      req(rv.widgets$Finetuning_cluster_protprofile)
      
      conds <- DaparToolshed::design_qf(rv$dataIn)$Condition
      prot_prof <- switch(rv.widgets$Finetuning_cluster_protprofile,
                          Mean = sapply(unique(conds), function(c) 
                            rowMeans(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])[, conds == c, drop = FALSE], 
                                     na.rm = TRUE)),
                          Median = sapply(unique(conds), function(c) 
                            matrixStats::rowMedians(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])[, conds == c, drop = FALSE], 
                                       na.rm = TRUE)))
      
      datapca <- prot_prof
      pca <- prcomp(datapca,  scale = TRUE)
      scores <- pca$x[, seq_len(2)]
      
      clusterspca <- as.factor(rv.custom$res_clusters)
      cols <- as.numeric(clusterspca)
      
      plot(scores, col = cols, pch = 19,
           xlab = "PC1", ylab = "PC2", main = "Clusters"
      )
      legend("topright", legend = levels(clusterspca),
             col = seq_along(levels(clusterspca)), pch = 19
      )
    })
    
    ### Aggregation - Aggregation -----
    #### _sidebar -----
    output$Finetuning_aggregation_param_ui <- renderUI({
      req(rv$steps.status["Hypothesistest"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Aggregation")
      
      widget1 <- selectInput(ns("Finetuning_aggreg_method"), "p-value agggregation method",
                             choices = c("MinPval", "MaxPval"),
                             selected = rv.widgets$Finetuning_aggreg_method,
                             width = "200px")
      
      MagellanNTK::toggleWidget(widget1, rv$steps.enabled['Finetuning'])
    })
    
    #### _content -----
    output$Finetuning_aggregation_ui <- renderUI({
      req(rv$steps.status["Hypothesistest"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Aggregation")
      
      tagList(plotOutput(ns('Finetuning_aggregation_plot')))
    })
    
    output$Finetuning_aggregation_plot <- renderPlot({
      req(rv$steps.status["Hypothesistest"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Aggregation")
      req(rv.widgets$Finetuning_aggreg_method)
      
      nb_contr <- ncol(rv.custom$res_pval_FC_complete$P_Value)
      min_pvals <- apply(rv.custom$res_pval_FC_complete$P_Value, 1, min, na.rm = TRUE)
      max_pvals <- apply(rv.custom$res_pval_FC_complete$P_Value, 1, max, na.rm = TRUE)
      
      pval_agg <- switch(rv.widgets$Finetuning_aggreg_method,
             MinPval = {
               best.pval <- apply(rv.custom$res_pval_FC_complete$P_Value, 1, min)
               1-(1-best.pval)^nb_contr},
             MaxPval = {
               worst.pval <- apply(rv.custom$res_pval_FC_complete$P_Value, 1, max)
               worst.pval^nb_contr}
      )
      
      ord <- order(pval_agg)
      pval_agg_sorted <- pval_agg[ord]
      min_sorted <- min_pvals[ord]
      max_sorted <- max_pvals[ord]
      
      plot(pval_agg_sorted,
           ylim = range(min_sorted, max_sorted),
           pch = 16,
           xaxt="n",
           xlab = "",
           ylab = "p-value",
           main = "Ordered aggregated p-value with interval")
      
      arrows(x0 = seq_along(pval_agg_sorted),
             y0 = min_sorted,
             x1 = seq_along(pval_agg_sorted),
             y1 = max_sorted,
             angle = 90,
             code = 3,
             length = 0.05,
             col = "blue")
    })
    
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Finetuning', btnEvents()))
      
      if (is.null(rv$dataIn)) {
        shinyjs::info(btnVentsMasg)
        
      } else {
        if (rv.widgets$Scenario_choice == "Contrast"){
          if (!('Finetuning' %in% rv.custom$history$Step)){
            rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DifferentialAnalysis', 'Finetuning', 'query', '-')
          }
          rv.custom$res_pval_FC$P_Value <- rv.custom$res_pval_tmp
          
          
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DifferentialAnalysis', 'Finetuning', 'query', query)    
          
        } else if (rv.widgets$Scenario_choice == "Cluster"){
          rv.custom$comparison <- "omnibus_cluster"
          rv.widgets$Finetuning_cluster_nbclust <- round(rv.widgets$Finetuning_cluster_nbclus, 0)
          
          conds <- DaparToolshed::design_qf(rv$dataIn)$Condition
          prot_prof <- switch(rv.widgets$Finetuning_cluster_protprofile,
                 Mean = sapply(unique(conds), function(c) 
                   rowMeans(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])[, conds == c, drop = FALSE], 
                            na.rm = TRUE)),
                 Median = sapply(unique(conds), function(c) 
                   matrixStats::rowMedians(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])[, conds == c, drop = FALSE], 
                            na.rm = TRUE))
          )
          
          if (length(unique(conds)) == 2) {
            means <- rowMeans(prot_prof) 
            rv.custom$centered_means <- prot_prof - means 
            if (rv.widgets$Finetuning_cluster_nbclust == 1){
              rv.custom$res_clusters <- as.integer(rep_len(1, nrow(prot_prof)))
            }else{
              difference <- prot_prof[, 1] - prot_prof[, 2]
              rv.custom$res_clusters <- as.integer(ifelse(difference > 0, 1, 2))
            }
          } else if (length(unique(conds)) > 2) {
            standards <- prot_prof
            for (i in seq_len(nrow(standards))) {
              standards[i, ] <- (standards[i, ] - mean(standards[i, ], na.rm = TRUE))/sd(standards[i, 
              ], na.rm = TRUE)
            }
            rv.custom$centered_means <- standards
            
            rv.custom$res_clusters <- switch(rv.widgets$Finetuning_cluster_method,
                   affinityProp = {model <- apcluster::apcluster(apcluster::negDistMat(r = 2), 
                                                                 standards)
                                   clust_num <- 0
                                   cluster <- rep(0, nrow(standards))
                                   for (clust in model@clusters){
                                     clust_num <- clust_num + 1
                                     cluster[clust] <- clust_num
                                   }
                                   cluster
                   },
                   affinityPropReduced = {model <- apcluster::apcluster(apcluster::negDistMat(r = 2), 
                                                                        standards, q = 0)
                                          clust_num <- 0
                                          cluster <- rep(0, nrow(standards))
                                          for (clust in model@clusters){
                                            clust_num <- clust_num + 1
                                            cluster[clust] <- clust_num
                                          }
                                          cluster
                   },
                   kmeans = {
                     if (is.null(rv.widgets$Finetuning_cluster_nbclust)) {
                     gap_cluster <- cluster::clusGap(as.matrix(standards), 
                                                       FUNcluster = stats::kmeans, 
                                                       nstart = 20, 
                                                       K.max = 10, 
                                                       d.power = 2, 
                                                       B = 500)
                     best_k <- cluster::maxSE(gap_cluster$Tab[, "gap"],
                                              gap_cluster$Tab[, "SE.sim"],
                                              method = "Tibs2001SEmax"
                     )
                     cluster <- stats::kmeans(standards,
                                              centers = best_k,
                                              nstart = 25)
                     cluster$cluster
                   } else if (rv.widgets$Finetuning_cluster_nbclust > 1) {
                     best_k <- rv.widgets$Finetuning_cluster_nbclust
                     cluster <- stats::kmeans(standards,
                                              centers = best_k, 
                                              nstart = 25)
                     cluster$cluster
                   } else { # corresponds to the case k = 1 so no need for clustering
                     as.integer(rep_len(1, nrow(standards)))
                   }
                  })
          }
          nbclust <- length(unique(rv.custom$res_clusters))
          
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DifferentialAnalysis', 'Finetuning', 'Clustering_Method', rv.widgets$Finetuning_cluster_method)
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DifferentialAnalysis', 'Finetuning', 'Number_of_Clusters', nbclust)
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DifferentialAnalysis', 'Finetuning', 'Protein_Profile_Method', rv.widgets$Finetuning_cluster_protprofile)
          
        } else if (rv.widgets$Scenario_choice == "Aggregation") {
          rv.custom$comparison <- "aggregated"
          nb_contr <- ncol(rv.custom$res_pval_FC$P_Value)
          
          rv.custom$res_pval_FC$P_Value <- switch(rv.widgets$Finetuning_aggreg_method,
                    MinPval = {
                      best.pval <- apply(rv.custom$res_pval_FC$P_Value, 1, min)
                      rv.custom$colidx_pvalagg <- apply(rv.custom$res_pval_FC$P_Value, 1, which.min)
                      #best.pval_colname <- colnames(rv.custom$res_pval_FC$P_Value)[rv.custom$colidx_pvalagg]
                      
                      sidak.pval <- 1-(1-best.pval)^nb_contr
                      sidak.pval},
                    MaxPval = {
                      worst.pval <- apply(rv.custom$res_pval_FC$P_Value, 1, max)
                      rv.custom$colidx_pvalagg <- apply(rv.custom$res_pval_FC$P_Value, 1, which.max)
                      #worst.pval_colname <- colnames(rv.custom$res_pval_FC$P_Value)[rv.custom$colidx_pvalagg]
                      
                      sidak.pval <- worst.pval^nb_contr
                      sidak.pval}
                 )
          rv.custom$res_pval_FC$logFC <- rep(NA, length(rv.custom$res_pval_FC$P_Value))
          
          rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DifferentialAnalysis', 'Finetuning', 'Aggregation_Method', rv.widgets$Finetuning_aggreg_method)
        }
        
        # DO NOT MODIFY THE NEXT THREE LINES
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status["Finetuning"] <- MagellanNTK::stepStatus$VALIDATED
      }
    })
    
    
    ###########################################################################-
    #
    #--------------------------PVALUE CALIBRATION-------------------------------
    #
    ###########################################################################-
    output$Pvaluecalibration <- renderUI({
      shinyjs::useShinyjs()
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns('Pvaluecalibration_calibrationMethod_UI')),
          uiOutput(ns("Pvaluecalibration_numericValCalibration_UI")),
          uiOutput(ns("Pvaluecalibration_nBins_UI"))
        ),
        content = tagList(
          plotly::plotlyOutput(ns("histPValue")),
          imageOutput(ns("calibrationPlotAll"), height = "800px"),
          imageOutput(ns("calibrationPlot"), height = "400px")
          # p(tags$strong(
          #   paste0("value of pi0: ", round(as.numeric(rv.custom$pi0), digits = 2))
          # ))
        )
      )
    })
    
    ### Common to all (Contrast/Cluster/Aggregation) -----
    ### _sidebar -----
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
                             "pi0",
                             value = rv.widgets$Pvaluecalibration_numericValCalibration,
                             min = 0,
                             max = 1,
                             step = 0.05
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pvaluecalibration"] &&
                                  rv.widgets$Pvaluecalibration_calibrationMethod == "numeric value")
    })
    
    output$Pvaluecalibration_nBins_UI <- renderUI({
      req(rv$steps.status["Finetuning"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.custom$res_pval_FC)
      req(rv.custom$pi0)
      
      widget <- numericInput(
        ns("Pvaluecalibration_nBinsHistpval"), 
        "n bins histogram",
        value = rv.widgets$Pvaluecalibration_nBinsHistpval,
        min = 1,
        max = 100,
        step = 10,
        width = "200px")
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pvaluecalibration"])
    })
    

    ### _content -----
    ###GRAPH HISTO
    histPValue <- reactive({
      req(rv.custom$res_pval_FC)
      req(rv.custom$pi0)
      req(rv.widgets$Pvaluecalibration_nBinsHistpval)
      req(rv.widgets$Hypothesistest_thlogFC)
      req(!is.na(rv.widgets$Hypothesistest_thlogFC))
      req(length(rv.custom$res_pval_FC$logFC) > 0)
      
      if (rv.widgets$Scenario_choice == "Contrast" & rv.widgets$Hypothesistest_contrastchoice == "Stacked"){
        t <- unlist(rv.custom$res_pval_FC_stacked$P_Value)
        t <- t[which(abs(rv.custom$res_pval_FC_stacked$logFC) >= rv.widgets$Hypothesistest_thlogFC)]
      }else{
        t <- unlist(rv.custom$res_pval_FC$P_Value)
        if (rv.widgets$Scenario_choice == "Contrast"){
          t <- t[which(abs(rv.custom$res_pval_FC$logFC) >= rv.widgets$Hypothesistest_thlogFC)]
        }
      }
      toDelete <- which(t > 1)
      if (length(toDelete) > 0) {
        t <- t[-toDelete]
      }

      DaparToolshed::histPValue_HC(t,
                                   bins = as.numeric(round(rv.widgets$Pvaluecalibration_nBinsHistpval, 0)),
                                   pi0 = rv.custom$pi0)
    })

    output$histPValue <- plotly::renderPlotly({
      histPValue()
    })


    ###GRAPH ALL
    calibrationPlotAll <- reactive({
      rv.custom$res_pval_FC
      req(rv$steps.status["Finetuning"] == MagellanNTK::stepStatus$VALIDATED)
      req(!is.na(rv.widgets$Hypothesistest_thlogFC))
      req(length(rv.custom$res_pval_FC$logFC) > 0)

      if (rv.widgets$Scenario_choice == "Contrast" & rv.widgets$Hypothesistest_contrastchoice == "Stacked"){
        t <- unlist(rv.custom$res_pval_FC_stacked$P_Value)
        t <- t[which(abs(rv.custom$res_pval_FC_stacked$logFC) >= rv.widgets$Hypothesistest_thlogFC)]
      }else{
        t <- unlist(rv.custom$res_pval_FC$P_Value)
        if (rv.widgets$Scenario_choice == "Contrast"){
          t <- t[which(abs(rv.custom$res_pval_FC$logFC) >= rv.widgets$Hypothesistest_thlogFC)]
        }
      }
      toDelete <- which(t > 1)
      if (length(toDelete) > 0) {
        t <- t[-toDelete]
      }

      l <- NULL
      result <- tryCatch(
        {
          l <- catchToList(DaparToolshed::wrapperCalibrationPlot(t, "ALL"))
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
        png(outfile, width = 600, height = 500)
        calibrationPlotAll()
        dev.off()

        # Return a list
        list(
          src = outfile,
          alt = "This is alternate text"
        )
      },
      deleteFile = TRUE
    )


    output$errMsgCalibrationPlotAll <- renderUI({
      rv.custom$errMsgCalibrationPlotAll
      req(rv$dataIn)
      req(!is.null(rv.custom$errMsgCalibrationPlotAll))

      txt <- NULL
      for (i in seq_along(rv.custom$errMsgCalibrationPlotAll)) {
        txt <- paste(txt, "errMsgCalibrationPlotAll:",
                     rv.custom$errMsgCalibrationPlotAll[i], "<br>",
                     sep = "")
      }

      div(HTML(txt), style = "color:red")
    })

    ###GRAPH METHOD CONTENT
    calibrationPlot <- reactive({
      req(rv.custom$res_pval_FC)
      req(rv$dataIn)
      req(length(rv.custom$res_pval_FC$logFC) > 0)
      if (rv.widgets$Scenario_choice == "Contrast" & rv.widgets$Hypothesistest_contrastchoice == "Stacked"){
        t <- unlist(rv.custom$res_pval_FC_stacked$P_Value)
        t <- t[which(abs(rv.custom$res_pval_FC_stacked$logFC) >= rv.widgets$Hypothesistest_thlogFC)]
      }else{
        t <- unlist(rv.custom$res_pval_FC$P_Value)
        if (rv.widgets$Scenario_choice == "Contrast"){
          t <- t[which(abs(rv.custom$res_pval_FC$logFC) >= rv.widgets$Hypothesistest_thlogFC)]
        }
      }
      toDelete <- which(t > 1)
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
              DaparToolshed::wrapperCalibrationPlot(
                t,
                rv.widgets$Pvaluecalibration_numericValCalibration
              )
            )
            .warns <- ll$warnings[grep("Warning:", ll$warnings)]
            rv.custom$errMsgCalibrationPlot <- .warns
          } else if (rv.widgets$Pvaluecalibration_calibrationMethod == "Benjamini-Hochberg") {
            ll <- catchToList(DaparToolshed::wrapperCalibrationPlot(t, 1))
            .warns <- ll$warnings[grep("Warning:", ll$warnings)]
            rv.custom$errMsgCalibrationPlot <- .warns
          } else {
            ll <- catchToList(
              DaparToolshed::wrapperCalibrationPlot(t, rv.widgets$Pvaluecalibration_calibrationMethod)
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

    output$calibrationPlot <- renderImage({
        outfile <- tempfile(fileext = ".png")

        # Generate a png
        png(outfile, width = 600, height = 500)
        calibrationPlot()
        dev.off()

        # Return a list
        list(
          src = outfile,
          alt = "This is alternate text"
        )
      }, deleteFile = TRUE
    )

    output$errMsgCalibrationPlot <- renderUI({
      req(rv.custom$errMsgCalibrationPlot)
      req(rv$dataIn)

      txt <- NULL
      for (i in seq_along(rv.custom$errMsgCalibrationPlot)) {
        txt <- paste(txt, "errMsgCalibrationPlot: ",
                     rv.custom$errMsgCalibrationPlot[i], "<br>",
                     sep = "")
      }

      div(HTML(txt), style = "color:red")
    })


    ###UTILS
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
    
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Pvaluecalibration', btnEvents()))
      
      if (is.null(rv$dataIn)) {
        shinyjs::info(btnVentsMasg)
        
      } else {
        rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DifferentialAnalysis', 'Pvaluecalibration', 'Calibration_Method', rv.widgets$Pvaluecalibration_calibrationMethod)
        rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DifferentialAnalysis', 'Pvaluecalibration', 'pi0', rv.custom$pi0)
        
        # DO NOT MODIFY THE NEXT THREE LINES
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status["Pvaluecalibration"] <- MagellanNTK::stepStatus$VALIDATED
      }
    })
    
    
    ###########################################################################-
    #
    #-----------------------------FDR CONTROL-----------------------------------
    #
    ###########################################################################-
    output$FDRcontrol <- renderUI({
      shinyjs::useShinyjs()
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns('FDRcontrol_widgets_ui')),
          uiOutput(ns('FDRcontrol_widgets_contrast_ui'))
        ),
        content = tagList(
          uiOutput(ns('FDRcontrol_plot_contrast_ui')),
          uiOutput(ns('FDRcontrol_plot_cluster_ui')),
          uiOutput(ns('FDRcontrol_plot_aggreg_ui')),
          downloadButton(ns("FDRcontrol_download_SelectedItems_UI"),
                         "Selected final results (Excel file)", class = "btn-info"),
          tabsetPanel(
            id = ns("hidden_tabs"),
            type = "hidden",
            tabPanelBody("panelNULL", NULL),
            tabPanelBody("panel1", DT::DTOutput(ns("FDR_selectedItems_UI")))
          )
        )
      )
    })
    
    ### Contrast - Volcano plot -----
    # output$FDRcontrol_widgets_contrast_ui <- renderUI({
    #   req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
    #   req(rv.widgets$Hypothesistest_contrastchoice == "Stacked")
    #   
    #   widget <- selectInput(ns("FDRcontrol_volcanocontrast"), "Select contrast",
    #                         choices = rv.custom$Scenario_constratnames,
    #                         selected = rv.widgets$FDRcontrol_volcanocontrast,
    #                         width = "200px")
    #   
    #   MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDRcontrol"])
    # })
    
    output$FDRcontrol_plot_contrast_ui <- renderUI({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Contrast")
      
      widget <- div(
        style = "display: flex; gap: 20px;",
        mod_volcanoplot_ui(ns("FDR_volcano")),
        div(uiOutput(ns("FDR_nbSelectedItems_ui")),
            br(),
            uiOutput(ns("FDRcontrol_Pairwisecomparison_tooltipInfo_UI")))
        )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDRcontrol"])
    })
    
    Prostar2::mod_volcanoplot_server(
      id = "FDR_volcano",
      dataIn = reactive({rv.custom$push_pval_data}), 
      comparison = reactive({rv.custom$names_condition}),
      group = reactive({rv.custom$cond}),
      thlogfc = reactive({rv.widgets$Hypothesistest_thlogFC}),
      thpval = reactive({rv.custom$FDRcontrol_thpval}),
      tooltip = reactive({rv.custom$FDRcontrol_Pairwisecomparison_tooltipInfo}), 
      remoteReset = reactive({remoteReset()}),
      is.enabled = reactive({rv$steps.enabled["FDRcontrol"]})
    )
    
    output$FDRcontrol_Pairwisecomparison_tooltipInfo_UI <- renderUI({
      req(rv$dataIn)
      req(rv.widgets$FDRcontrol_Pairwisecomparison_tooltipInfo)
      
      widget <- tagList(
        selectInput(ns("FDRcontrol_Pairwisecomparison_tooltipInfo"),
                    label = "Tooltip",
                    choices = rv.custom$rowdatacolname,
                    selected = rv.widgets$FDRcontrol_Pairwisecomparison_tooltipInfo,
                    multiple = TRUE,
                    selectize = FALSE,
                    width = "300px", 
                    size = 5),
        actionButton(ns("Pairwisecomparison_validTooltipInfo"),  "Validate tooltip choice", 
                     class = "btn-info")
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDRcontrol"])
    })
    
    observeEvent(input$Pairwisecomparison_validTooltipInfo, {
      rv.custom$FDRcontrol_Pairwisecomparison_tooltipInfo <- rv.widgets$FDRcontrol_Pairwisecomparison_tooltipInfo
    })
    
    
    ### Cluster - Plot -----
    output$FDRcontrol_plot_cluster_ui <- renderUI({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Cluster")
      req(Build_pval_table()$isDifferential)
      
      tagList(
        fluidRow(
          column(width = 8,
            if(rv.widgets$FDRcontrol_cluster_plot_type == "Fixed"){
              plotOutput(ns('FDRcontrol_plot_cluster_plot_mult'))
            } else {
              plotly::plotlyOutput(ns("FDRcontrol_plot_cluster_plot_unique"))
            }),
          column(width = 4,
            uiOutput(ns('FDRcontrol_plot_cluster_param')))
        )
      )
    })
    
    output$FDRcontrol_plot_cluster_param <- renderUI({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Cluster")
      
      widget1 <- radioButtons(ns("FDRcontrol_cluster_plot_type"), 
                              "Type of graph",
                              choices = c("Fixed", "Interactive"),
                              selected = rv.widgets$FDRcontrol_cluster_plot_type)
      widget2 <- selectInput(ns("FDRcontrol_cluster_plot_clust"), 
                             "Cluster to plot",
                             choices = unique(rv.custom$res_clusters),
                             selected = rv.widgets$FDRcontrol_cluster_plot_clust,
                             width = "200px")
      
      tagList(
          uiOutput(ns("FDR_nbSelectedItems_ui")),
          MagellanNTK::toggleWidget(widget1, rv$steps.enabled['FDRcontrol']),
          if (rv.widgets$FDRcontrol_cluster_plot_type == "Interactive"){
            MagellanNTK::toggleWidget(widget2, rv$steps.enabled['FDRcontrol'])}
      )
    })
    
    output$FDRcontrol_plot_cluster_plot_mult <- renderPlot({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Cluster")
      req(Build_pval_table()$isDifferential)
      
      df <- as.data.frame(rv.custom$centered_means)
      vars <- colnames(rv.custom$centered_means)
      df$flag <- Build_pval_table()$isDifferential
      df$cluster <- rv.custom$res_clusters
      
      clusters <- unique(rv.custom$res_clusters)  
      
      n_clust <- length(clusters)
      n_col <- ceiling(sqrt(n_clust))
      n_row <- ceiling(n_clust / n_col)
      
      par(mfrow = c(n_row, n_col), mar = c(4,4,3,1))
      
      for (cl in clusters) {
        
        df_cluster <- df[df$cluster == cl, ]
        x_pos <- seq_along(vars)
        cols <- ifelse(df_cluster$flag == 1, "orange", "grey")
        
        ylim_range <- range(as.matrix(df_cluster[, vars]), na.rm = TRUE)
        
        plot(x_pos,
             as.numeric(df_cluster[1, vars]),
             type = "n",
             xaxt = "n",
             xlab = "Condition",
             ylab = "Protein profile value",
             main = paste("Cluster", cl),
             ylim = ylim_range)
        
        axis(1, at = x_pos, labels = vars)
        
        for (i in seq_len(nrow(df_cluster))) {
          lines(x_pos,
                as.numeric(df_cluster[i, vars]),
                col = adjustcolor(cols[i], alpha.f = 0.7),
                lwd = 1)
        }
        
        legend("topright",
               legend = c("Differential", "Non differential"),
               col = c("orange", "grey"),
               lty = 1,
               cex = 0.8,
               bty = "n")
      }
      
      # Reset layout
      par(mfrow = c(1,1))
    })
    
    output$FDRcontrol_plot_cluster_plot_unique <- plotly::renderPlotly({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Cluster")
      req(Build_pval_table()$isDifferential)

      df <- as.data.frame(rv.custom$centered_means)
      vars <- colnames(rv.custom$centered_means)
      df$flag <- Build_pval_table()$isDifferential
      df$cluster <- rv.custom$res_clusters

      clusters <- unique(rv.custom$res_clusters)


      df_cluster <- df[df$cluster == rv.widgets$FDRcontrol_cluster_plot_clust, ]

      series_list <- lapply(seq_len(nrow(df_cluster)), function(i) {
        flag_value <- df_cluster$flag[i]
        list(
          name = rownames(df_cluster)[i],
          data = as.numeric(df_cluster[i, vars]),
          color = ifelse(flag_value == 1, "orange", "grey"),
          showInLegend = FALSE
        )
      })

      highcharter::highchart() |>
        highcharter::hc_chart(type = "line") |>
        highcharter::hc_title(text = paste0("Cluster ", rv.widgets$FDRcontrol_cluster_plot_clust)) |>
        highcharter::hc_xAxis(categories = vars, title = list(text = "Condition")) |>
        highcharter::hc_yAxis(title = list(text = "Protein profile value")) |>
        highcharter::hc_add_series_list(series_list) |>
        highcharter::hc_add_series(name = "Differential", data = list(), color = "orange") |>
        highcharter::hc_add_series(name = "Non differential", data = list(), color = "grey")
    })
    
    ### Aggregation - Plot -----
    output$FDRcontrol_plot_aggreg_ui <- renderUI({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Aggregation")
      
      tagList(
        fluidRow(
                 column(width = 8,
                        if(rv.widgets$FDRcontrol_aggreg_plot_type == "Fixed"){
                          plotOutput(ns('FDRcontrol_plot_aggreg_plot_volcano_base'), height = "600px")
                        } else {
                          plotly::plotlyOutput(ns("FDRcontrol_plot_aggreg_plot_volcano_highchart"), height = "600px")
                        }),
                 column(width = 4,
                        uiOutput(ns("FDR_nbSelectedItems_ui")),
                        br(),
                        uiOutput(ns("FDRcontrol_plot_aggreg_param")))
        ))
    })
    
    output$FDRcontrol_plot_aggreg_param <- renderUI({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Aggregation")
      
      widget1 <- radioButtons(ns("FDRcontrol_aggreg_plot_type"), 
                              "Type of graph",
                              choices = c("Fixed", "Interactive"),
                              selected = rv.widgets$FDRcontrol_aggreg_plot_type)
      
      widget2 <- selectInput(ns("FDRcontrol_aggreg_plot_rep"), 
                             "Representation",
                             choices = c("Associated FC", "FC interval", "Ellipse"),
                             selected = rv.widgets$FDRcontrol_aggreg_plot_rep,
                             width = "200px")
      
      tagList(
        MagellanNTK::toggleWidget(widget1, rv$steps.enabled['FDRcontrol']),
        MagellanNTK::toggleWidget(widget2, rv$steps.enabled['FDRcontrol'])
      )
    })
    
    output$FDRcontrol_plot_aggreg_plot_volcano_highchart <- plotly::renderPlotly({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Aggregation")
      req(rv.widgets$FDRcontrol_aggreg_plot_type == "Interactive")
      
      withProgress(message = "", detail = "", value = 0, {
        incProgress(0.5, detail = "Making plot")
        pal <- list(diff = "orange", nondiff = "gray")
        clickFunction <-
          shinyjqui::JS(paste0(
            "function(event) {Shiny.onInputChange('",
            ns("eventPointClicked"),
            "', [this.index]+'_'+ [this.series.name]);}"
          ))
        
        fcunique <- rv.custom$res_pval_FC_complete$logFC[cbind(seq(length(rv.custom$res_pval_FC$logFC)), rv.custom$colidx_pvalagg)]
        df <- data.frame(
          x = fcunique,
          y = -log10(rv.custom$res_pval_FC$P_Value),
          index = seq(length(rv.custom$res_pval_FC$P_Value)),
          xmin = apply(rv.custom$res_pval_FC_complete$logFC, 1, min, na.rm = TRUE),
          xmax = apply(rv.custom$res_pval_FC_complete$logFC, 1, max, na.rm = TRUE),
          ymin = -log10(apply(rv.custom$res_pval_FC_complete$P_Value, 1, max, na.rm = TRUE)),
          ymax = -log10(apply(rv.custom$res_pval_FC_complete$P_Value, 1, min, na.rm = TRUE)),
          tooltip_ProtName = names(rv.custom$res_pval_FC$P_Value)
        )
        df <- cbind(df,
                    g = ifelse(df$y >= rv.custom$FDRcontrol_thpval, "g1", "g2")
        )
        txt_tooltip <- "{point.tooltip_ProtName} <br>"
        
        title <- paste0("Aggregated p-values (", rv.widgets$Finetuning_aggreg_method, ")")
        
        if (rv.widgets$FDRcontrol_aggreg_plot_rep == "Ellipse"){
          create_ellipse <- function(x, y, rx, ry, n = 40) {
            theta <- seq(0, 2 * pi, length.out = n)
            xvals <- x + rx * cos(theta)
            yvals <- y + ry * sin(theta)
            
            # Highcharts format = list(x, y)
            lapply(seq_along(xvals), function(i) {
              list(xvals[i], yvals[i])
            })
          }
          
          # create ellipses
          ellipses_list <- lapply(seq_len(nrow(df)), function(i) {
            rx <- (df$xmax[i] - df$xmin[i]) / 2
            ry <- (df$ymax[i] - df$ymin[i]) / 2
            
            ell <- create_ellipse(df$x[i], df$y[i], rx, ry)
            
            col <- if (df$g[i] == "g1") {
              "rgba(255,165,0,0.20)"
            } else {
              "rgba(128,128,128,0.20)"
            }
            
            list(
              type = "polygon",
              data = ell,
              color = col,
              lineWidth = 1,
              enableMouseTracking = FALSE,
              showInLegend = FALSE
            )
          })
          
          hc <- highcharter::highchart() |>
            highcharter::hc_add_series_list(ellipses_list)
        } else if (rv.widgets$FDRcontrol_aggreg_plot_rep == "FC interval") {
          df_seg <- do.call(rbind, lapply(seq_len(nrow(df)), function(i) {
            data.frame(
              x = c(df$xmin[i], df$xmax[i]),
              y = c(df$y[i], df$y[i]),
              id = i
            )
          }))
          
          hc <- highcharter::highchart() |>
            highcharter::hc_add_series(
              data = df_seg,
              type = "line",
              highcharter::hcaes(x = x, y = y, group = id),
              color = "darkgrey",
              lineWidth = 1,
              enableMouseTracking = FALSE,
              marker = list(enabled = FALSE),
              showInLegend = FALSE
            )
        } else {
          hc <- highcharter::highchart()
        }
        
        hc <- hc |>
          highcharter::hc_add_series(data = df, type = "scatter", highcharter::hcaes(x, y, group = g)) |>
          highcharter::hc_colors(c(pal$diff, pal$nondiff)) |>
          DaparToolshed::my_hc_chart(zoomType = "xy", chartType = "scatter") |>
          highcharter::hc_legend(enabled = FALSE) |>
          highcharter::hc_title(
            text = title,
            margin = 20, align = "center",
            style = list(size = 20, color = "black", useHTML = TRUE)
          ) |>
          highcharter::hc_yAxis(title = list(text = "-log10(pValue)"),
                                min = -0.1,
                                startOnTick = FALSE,
                                plotLines = list(
                                  list(
                                    value = rv.custom$FDRcontrol_thpval,
                                    color = "grey",
                                    width = 3,
                                    dashStyle = "Dash"
                                  )
                                )) |>
          highcharter::hc_xAxis(
            title = list(text = "logFC"),
            plotLines = list(
              list(
                color = "grey",
                width = 1,
                value = 0,
                zIndex = 5
              )
            )
          ) |>
          highcharter::hc_tooltip(headerFormat = "", pointFormat = txt_tooltip) |>
          highcharter::hc_plotOptions(
            scatter = list(
              marker = list(
                radius = 4,
                lineWidth = 1,
                lineColor = "white",
                symbol = "circle"
              )
            ),
            line = list(
              marker = list(enabled = FALSE),
              dashStyle = "Dash"
            ),
            series = list(
              animation = list(duration = 100),
              cursor = "pointer",
              point = list(events = list(
                click = clickFunction
              ))
            )
          ) |>
          DaparToolshed::my_hc_ExportMenu(filename = "volcanoplot")
      })
      hc
    })
    
    output$FDRcontrol_plot_aggreg_plot_volcano_base <- renderPlot({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Aggregation")
      req(rv.widgets$FDRcontrol_aggreg_plot_type == "Fixed")
      
      pal <- list(diff = "orange", nondiff = "gray")
      fcunique <- rv.custom$res_pval_FC_complete$logFC[
        cbind(seq(length(rv.custom$res_pval_FC$logFC)),
              rv.custom$colidx_pvalagg)
      ]
      df <- data.frame(
        x = fcunique,
        y = -log10(rv.custom$res_pval_FC$P_Value),
        xmin = apply(rv.custom$res_pval_FC_complete$logFC, 1, min, na.rm = TRUE),
        xmax = apply(rv.custom$res_pval_FC_complete$logFC, 1, max, na.rm = TRUE),
        ymin = -log10(apply(rv.custom$res_pval_FC_complete$P_Value, 1, max, na.rm = TRUE)),
        ymax = -log10(apply(rv.custom$res_pval_FC_complete$P_Value, 1, min, na.rm = TRUE))
      )
      df$g <- ifelse(df$y >= rv.custom$FDRcontrol_thpval, "g1", "g2")
      cols <- ifelse(df$g == "g1", pal$diff, pal$nondiff)
      
      plot(
        df$x, df$y,
        pch = 16,
        col = cols,
        cex = 1,
        xlab = "logFC",
        ylab = "-log10(pValue)",
        main = paste0("Aggregated p-values (", rv.widgets$Finetuning_aggreg_method, ")")
      )
      
      abline(v = 0, col = "grey", lwd = 1)
      abline(h = rv.custom$FDRcontrol_thpval, col = "grey", lwd = 2, lty = 2)
      
      if (rv.widgets$FDRcontrol_aggreg_plot_rep == "Ellipse") {
        
        draw_ellipse <- function(x, y, rx, ry, n = 40) {
          theta <- seq(0, 2*pi, length.out = n)
          xs <- x + rx * cos(theta)
          ys <- y + ry * sin(theta)
          polygon(xs, ys, border = NA, col = rgb(0, 0, 0, 0.05))
        }
        
        for (i in seq_len(nrow(df))) {
          rx <- (df$xmax[i] - df$xmin[i]) / 2
          ry <- (df$ymax[i] - df$ymin[i]) / 2
          
          if (is.finite(rx) && is.finite(ry) && rx > 0 && ry > 0) {
            
            col <- if (df$g[i] == "g1") {
              rgb(1, 0.65, 0, 0.08)   # orange transparent
            } else {
              rgb(0.5, 0.5, 0.5, 0.08)
            }
            
            theta <- seq(0, 2*pi, length.out = 40)
            xs <- df$x[i] + rx * cos(theta)
            ys <- df$y[i] + ry * sin(theta)
            
            polygon(xs, ys, border = NA, col = col)
          }
        }
        
      } else if (rv.widgets$FDRcontrol_aggreg_plot_rep == "FC interval") {
        
        for (i in seq_len(nrow(df))) {
          segments(
            x0 = df$xmin[i],
            y0 = df$y[i],
            x1 = df$xmax[i],
            y1 = df$y[i],
            col = "darkgrey",
            lwd = 1
          )
        }
      }
      
      points(
        df$x, df$y,
        pch = 21,
        bg = cols,
        col = "white",
        lwd = 1,
        cex = 1.2
      )
    })
    
    ### Common to all (Contrast/Cluster/Aggregation) -----
    ### FDR -----
    #### _sidebar -----
    output$FDRcontrol_widgets_ui <- renderUI({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      
      widget <- tags$div(
        mod_set_pval_threshold_ui(ns("FDRcontrol_signifthreshold")),
        uiOutput(ns('showFDR_UI')),
        br(),
        uiOutput(ns('FDR_showHideDT_UI')),
        checkboxInput(ns('FDRcontrol_viewAdjPval'), 
                      'View adjusted p-value', 
                      value = rv.widgets$FDRcontrol_viewAdjPval)
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDRcontrol"])
    })
    
    output$FDR_showHideDT_UI <- renderUI({
      widget <- actionButton(ns("SELECT_INPUT"), "Hide/Show table")
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDRcontrol"])
    })
    
    output$showFDR_UI <- renderUI({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      #req(rv.widgets$Scenario_choice == "Contrast")
      req(Get_FDR())
      txt <- "FDR = NA"
      if (!is.infinite(Get_FDR())) {
        txt <- paste0("FDR = ", round(100 * Get_FDR(), digits = 2), " %")
      }
      h3(txt)
    })
    
    logpval <- Prostar2::mod_set_pval_threshold_server(id = "FDRcontrol_signifthreshold",
                                                       pval_init = reactive({10^(-rv.custom$FDRcontrol_thpval)}),
                                                       #fdr = reactive({Get_FDR()}),
                                                       remoteReset = reactive({remoteReset()}),
                                                       is.enabled = reactive({rv$steps.enabled["FDRcontrol"]}))
    observeEvent(logpval(), {
      req(logpval())
      tmp <- gsub(",", ".", logpval(), fixed = TRUE)
      
      rv.custom$FDRcontrol_thpval <- as.numeric(tmp)
      
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
    
    BuildPairwiseComp_wb <- reactive({
      DA_Style <- openxlsx::createStyle(fgFill = "#E97D5E")
      hs1 <- openxlsx::createStyle(fgFill = "#DCE6F1",
                                   halign = "CENTER",
                                   textDecoration = "italic",
                                   border = "Bottom")
      
      wb <- openxlsx::createWorkbook() # Create wb in R
      openxlsx::addWorksheet(wb, sheetName = "DA result") # create sheet
      openxlsx::writeData(wb,
                          sheet = 1,
                          as.character(rv.custom$comparison),
                          colNames = TRUE,
                          headerStyle = hs1
      )
      openxlsx::writeData(wb,
                          sheet = 1,
                          startRow = 3,
                          Build_pval_table(),
      )
      
      .txt <- paste0("isDifferential (",
                     as.character(rv.custom$comparison),
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

    Get_FDR <- reactive({
      req(rv.custom$FDRcontrol_thpval)
      req(Build_pval_table())
      
      adj.pval <- Build_pval_table()$Adjusted_PValue
      logpval <- Build_pval_table()$Log_PValue
      .logfc <- Build_pval_table()$logFC
      upitems_logpval <- which(logpval >= rv.custom$FDRcontrol_thpval)
      upItems_logfcinf <- which(abs(.logfc) < rv.widgets$Hypothesistest_thlogFC)
      upitems_logpval <- setdiff(upitems_logpval, upItems_logfcinf)
      
      if (length(adj.pval[upitems_logpval]) > 0){
        fdr <- max(adj.pval[upitems_logpval], na.rm = TRUE)
      } else {
        fdr <- 1
      }
      rv.custom$FDR <- as.numeric(fdr)
      as.numeric(fdr)
    })
    
    Get_Nb_Significant <- reactive({
      nb <- length(
        which(
          Build_pval_table()[paste0(
            "isDifferential (",
            as.character(rv.custom$comparison), ")"
          )] == 1
        )
      )
      nb
    })
    
    Build_pval_table <- reactive({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv$steps.status["Save"] != MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Hypothesistest_thlogFC)
      req(rv.custom$FDRcontrol_thpval)
      req(rv$dataIn)
      req(GetCalibrationMethod())
      
      #rv.widgets$Pairwisecomparison_Comparison
      .logfc <- unlist(rv.custom$res_pval_FC$logFC)
      .pval <- unlist(rv.custom$res_pval_FC$P_Value)
      .digits <- 3
      
      if (rv.widgets$Scenario_choice == "Contrast" & rv.widgets$Hypothesistest_contrastchoice == "Stacked"){
        pval_table <- data.frame(
          id = rep(rownames(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])), ncol(rv.custom$res_pval_FC_complete$P_Value)),
          logFC = round(.logfc, digits = .digits),
          P_Value = .pval,
          Log_PValue = -log10(.pval),
          Adjusted_PValue = rep(NA, length(.logfc)),
          isDifferential = rep(0, length(.logfc)),
          Contrast = rv.custom$contrast_list)
      }else{
        pval_table <- data.frame(
          id = rownames(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])),
          logFC = round(.logfc, digits = .digits),
          P_Value = .pval,
          Log_PValue = -log10(.pval),
          Adjusted_PValue = rep(NA, length(.logfc)),
          isDifferential = rep(0, length(.logfc)))
      }
      
      # Determine significant Peptides
      if (rv.widgets$Scenario_choice == "Contrast"){ 
        signifItems <- intersect(which(pval_table$Log_PValue >= rv.custom$FDRcontrol_thpval),
                                 which(abs(pval_table$logFC) >= rv.widgets$Hypothesistest_thlogFC))
        pval_table[signifItems,'isDifferential'] <- 1
        
        upItems_pval <- which(-log10(.pval) >= rv.custom$FDRcontrol_thpval)
        #push to 1 proteins with logFC under threshold
        pval_pushfc <- .pval
        upItems_logfcinf <- which(abs(.logfc) < rv.widgets$Hypothesistest_thlogFC)
        upItems_pushepval <- which(.pval > 1)
        upItems_logfcinf <- setdiff(upItems_logfcinf, upItems_pushepval)
        if (length(upItems_logfcinf) != 0){
          pval_pushfc[upItems_logfcinf] <- 1
        }  
        if (length(upItems_pushepval) != 0){
          pval_pushfc <- pval_pushfc[-upItems_pushepval]
        }
        rv.custom$adjusted_pvalues <- DaparToolshed::diffAnaComputeAdjustedPValues(
          pval_pushfc,
          GetCalibrationMethod())
        if (length(upItems_pushepval) != 0){
          pval_table[-upItems_pushepval, 'Adjusted_PValue'] <- rv.custom$adjusted_pvalues
        } else {
          pval_table[, 'Adjusted_PValue'] <- rv.custom$adjusted_pvalues
        }
        
      }else{
        signifItems <- which(pval_table$Log_PValue >= rv.custom$FDRcontrol_thpval)
        pval_table[signifItems,'isDifferential'] <- 1
        
        upItems_pval <- which(-log10(.pval) >= rv.custom$FDRcontrol_thpval)
        
        rv.custom$adjusted_pvalues <- DaparToolshed::diffAnaComputeAdjustedPValues(
          .pval,
          GetCalibrationMethod())
        pval_table[, 'Adjusted_PValue'] <- rv.custom$adjusted_pvalues
        }

      # Set only significant values
      pval_table$logFC <- signif(pval_table$logFC, digits = 4)
      pval_table$P_Value <- signif(pval_table$P_Value, digits = 4)
      pval_table$Adjusted_PValue <- signif(pval_table$Adjusted_PValue, digits = 4)
      pval_table$Log_PValue <- signif(pval_table$Log_PValue, digits = 4)
      
      if (rv.widgets$Scenario_choice == "Contrast" & rv.widgets$Hypothesistest_contrastchoice == "Stacked"){
        tmp <- as.data.frame(
          rep(SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])[, rv.custom$FDRcontrol_Pairwisecomparison_tooltipInfo], 
              ncol(rv.custom$res_pval_FC_complete$P_Value))
        )
      }else{
        tmp <- as.data.frame(
          SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])[, rv.custom$FDRcontrol_Pairwisecomparison_tooltipInfo])
      }
      
      names(tmp) <- rv.custom$FDRcontrol_Pairwisecomparison_tooltipInfo
      pval_table <- cbind(pval_table, tmp)
      colnames(pval_table)[2:6] <- paste0(colnames(pval_table)[2:6], " (", as.character(rv.custom$comparison), ")")
      
      pval_table
    })
    
    
    #### _content -----
    output$FDR_nbSelectedItems_ui <- renderUI({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      
      if (rv.widgets$Scenario_choice == "Contrast"){
        datatmp <- rv.custom$push_pval_data
      } else {
        datatmp <- rv$dataIn[[length(rv$dataIn)]]
      }
      
      # p <- Build_pval_table()
      # upItemsPVal <- NULL
      # upItemsLogFC <- NULL
      # 
      # upItemsLogFC <- which(abs(p$logFC) >= as.numeric(rv.widgets$Hypothesistest_thlogFC))
      # upItemsPVal <- which(-log10(p$P_Value) >= as.numeric(rv.custom$FDRcontrol_thpval))
      
      rv.custom$nbTotalAnaDiff <- nrow(SummarizedExperiment::assay(datatmp))
      rv.custom$nbSelectedAnaDiff <- NULL
      # t <- NULL
      
      # if (rv.widgets$Scenario_choice == "Contrast"){
      #   if (!is.null(rv.custom$FDRcontrol_thpval) && !is.null(rv.widgets$Hypothesistest_thlogFC)) {
      #     t <- intersect(upItemsPVal, upItemsLogFC)
      #   } else if (!is.null(rv.custom$FDRcontrol_thpval) && is.null(rv.widgets$Hypothesistest_thlogFC)) {
      #     t <- upItemsPVal
      #   } else if (is.null(rv.custom$FDRcontrol_thpval) && !is.null(rv.widgets$Hypothesistest_thlogFC)) {
      #     t <- upItemsLogFC
      #   }
      #   rv.custom$nbSelectedAnaDiff <- length(t)
      # }else{
      #   isdiff <- p[, grep("isDifferential", names(p))]
      #   rv.custom$nbSelectedAnaDiff <- length(isdiff[isdiff == 1])
      # }
      
      rv.custom$nbSelectedAnaDiff <- Get_Nb_Significant()
      
      ##
      ## Condition: A = C + D
      ##
      A <- rv.custom$nbTotalAnaDiff
      B <- A - length(which(rv.custom$res_pval_FC$P_Value > 1))
      C <- rv.custom$nbSelectedAnaDiff
      D <- (A - C)
      
      tagList(
        div(class = "bloc_page",
            p(paste0("Total number of ", 
                     DaparToolshed::typeDataset(datatmp), "(s) = ", A)),
            tags$em(p(style = "padding:0 0 0 20px;", 
                      paste0("Total remaining after push p-values = ", B))),
            p(paste0("Number of selected ", DaparToolshed::typeDataset(datatmp), "(s) = ", C)),
            p(paste0("Number of non selected ", DaparToolshed::typeDataset(datatmp), "(s) = ", D))
        ),
        tags$style(HTML(".bloc_page {
                          max-width: 400px;
                          background: #ffffff;
                          border: 1px solid #dddddd;
                          border-radius: 6px;
                          padding: 18px;
                          box-shadow: 0 2px 6px rgba(0,0,0,0.08);
                          }"))
      )
    })
    
    output$FDRcontrol_download_SelectedItems_UI <- downloadHandler(
      
      filename = function() {paste0("anaDiff_", rv.custom$comparison, ".xlsx")},
      content = function(fname) {
        wb <- BuildPairwiseComp_wb()
        openxlsx::saveWorkbook(wb, file = fname, overwrite = TRUE)
      }
    )
    
    observeEvent(input$SELECT_INPUT, {
      if (input$SELECT_INPUT %% 2 == 1) 
        updateTabsetPanel(session, "hidden_tabs", 
                          selected = paste0("panel1"))
      else updateTabsetPanel(session, "hidden_tabs", 
                             selected = paste0("panelNULL")
      )
    })
    
    output$FDR_selectedItems_UI <- DT::renderDT({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      df <- Build_pval_table()
      
      if (rv.widgets$FDRcontrol_viewAdjPval){
        df <- df[order(df$Adjusted_PValue, decreasing=FALSE), ]
        .coldefs <- list(list(width = "200px", targets = "_all"))
      } else {
        name <- paste0(c('Log_PValue (', 'Adjusted_PValue ('),
                       as.character(rv.custom$comparison), ")")
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
                                   ordering = !rv.widgets$FDRcontrol_viewAdjPval
                    )
      ) |>
        DT::formatStyle(
          paste0("isDifferential (",
                 as.character(rv.custom$comparison), ")"),
          target = "row",
          backgroundColor = DT::styleEqual(c(0, 1), c("white", "#E97D5E"))
        )
    })
    
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('FDRcontrol', btnEvents()))
      
      if (is.null(rv$dataIn)) {
        shinyjs::info(btnVentsMasg)
        
      } else {
        rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'DifferentialAnalysis', 'FDRcontrol', 'FDR_Threshold', rv.custom$FDRcontrol_thpval)
        
        new.dataset <- rv$dataIn[[length(rv$dataIn)]]
        rv$dataIn <- QFeatures::addAssay(rv$dataIn, new.dataset, 'DifferentialAnalysis')
        
        if (rv.widgets$Scenario_choice == "Contrast"){
          if (rv.widgets$Hypothesistest_contrastchoice == "Stacked"){
            pvaltable <- Build_pval_table()
            id <- pvaltable$id
            names(id) <- rownames(pvaltable)
            listpvaltable <- split(pvaltable,pvaltable$Contrast)
            listpvaltable <- lapply(names(listpvaltable), function(contrastname) {
              tmp <- listpvaltable[[contrastname]]
              colnames(tmp) <- gsub("stacked", contrastname, colnames(tmp))
              tmp$Contrast <- NULL
              tmp$id <- NULL
              tmp
            })
            pvaltable <- do.call(cbind, listpvaltable)
            pvaltable <- cbind(id[seq_len(nrow(pvaltable))], pvaltable)
            
            DaparToolshed::DifferentialAnalysis(rv$dataIn[[length(rv$dataIn)]]) <- pvaltable
            
          } else if (rv.widgets$Hypothesistest_contrastchoice %in% c("Unique_OvO", "Unique_OvA")){
            DaparToolshed::DifferentialAnalysis(rv$dataIn[[length(rv$dataIn)]]) <- Build_pval_table()
          }
        } else if (rv.widgets$Scenario_choice == "Cluster"){
          DaparToolshed::DifferentialAnalysis(rv$dataIn[[length(rv$dataIn)]]) <- Build_pval_table()
          SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])$Clusters <- rv.custom$res_clusters
        } else if (rv.widgets$Scenario_choice == "Aggregation"){
          pval_table <- Build_pval_table()
          DaparToolshed::DifferentialAnalysis(rv$dataIn[[length(rv$dataIn)]]) <- cbind(pval_table, rv.custom$res_pval_FC_complete$logFC, rv.custom$res_pval_FC_complete$P_Value)
        }
        
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status["FDRcontrol"] <- MagellanNTK::stepStatus$VALIDATED
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
          uiOutput(ns('dl_ui'))
        )
      )
    })
    
    output$dl_UI <- renderUI({
      req(rv$steps.status['Save'] == MagellanNTK::stepStatus$VALIDATED)
      req(config@mode == 'process')
      
      Prostar2::download_dataset_ui(ns(paste0(id, '_createQuickLink')))
    })
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Save', btnEvents()))
      # Do some stuff
      
      if (isTRUE(all.equal(SummarizedExperiment::assays(rv$dataIn), SummarizedExperiment::assays(dataIn()))))
        shinyjs::info(btnVentsMasg)
      else {
        shiny::withProgress(message = paste0("Saving process", id), {
          shiny::incProgress(0.5)
          
          rv$dataIn <- prepareQFsave(
            data = rv$dataIn,
            history = rv.custom$history,
            namePipeline = 'PipelinePeptide'
          )

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
