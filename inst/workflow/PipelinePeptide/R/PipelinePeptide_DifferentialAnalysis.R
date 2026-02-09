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
#' In this example, `PipelinePeptide_DifferentialAnalysis_UI()` and `PipelinePeptide_DifferentialAnalysis_server()` define
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
#' if (interactive()){
#' library(MagellanNTK)
#' library(DaparToolshed)
#' data(Exp2_R100_pept, package = "DaparToolshedData")
#' obj <- Exp2_R100_pept
#' # Simulate imputation of missing values
#' obj <- NAIsZero(obj, 1)
#' obj <- NAIsZero(obj, 2)
#' path <- system.file('workflow/PipelinePeptide', package = 'Prostar2')
#' shiny::runApp(workflowApp("PipelinePeptide_DifferentialAnalysis", path, dataIn = obj))
#' }
#' 
#' 
#' @author Manon Gaudin
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
NULL

#' @rdname PipelinePeptide
#' @export
#' 
PipelinePeptide_DifferentialAnalysis_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelinePeptide_DifferentialAnalysis',
    mode = 'process',
    steps = c("Scenario", "Fold-change", "Fine tuning", "P-value calibration", "FDR control"),
    mandatory = c(TRUE, TRUE, TRUE, TRUE, TRUE)
  )
}


#' @rdname PipelinePeptide
#' 
#' @export
#'
PipelinePeptide_DifferentialAnalysis_ui <- function(id){
  ns <- NS(id)
}



#' @rdname PipelinePeptide
#' 
#' @importFrom stats setNames rnorm
#' @importFrom magrittr "%>%"
#' @import DaparToolshed
#' @importFrom shinyjs info useShinyjs
#' 
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
  
  requireNamespace('DaparToolshed')
  pkgs.require('magrittr')
  
  pkgs.require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list(
    Scenario_choice = "Contrast",
    Scenario_method = "ANOVA",
    Foldchange_thlogFC = 0,
    Foldchange_contrastchoice = "",
    Foldchange_uniquechoice = NULL,
    Finetuning_cluster_protprofile = NULL,
    Finetuning_cluster_method = "kmeans",
    Finetuning_cluster_nbclust = 2,
    Finetuning_aggreg_method = "Most significant FC",
    Pvaluecalibration_calibrationMethod = "Benjamini-Hochberg",
    Pvaluecalibration_numericValCalibration = "None",
    Pvaluecalibration_nBinsHistpval = 80,
    FDRcontrol_viewAdjPval = FALSE,
    FDRcontrol_volcanocontrast = NULL
  )
  
  
  rv.custom.default.values <- list(
    result_open_dataset = reactive({NULL}),
    
    history = MagellanNTK::InitializeHistory(),
    res_pval_FC = NULL,
    res_pval_FC_complete = NULL,
    res_pval_FC_stacked = NULL,
    contrast_list = NULL,
    comparison = NULL,
    Scenario_constratnames = NULL,
    res_pval_tmp = NULL,
    push_pval_data = NULL,
    pushPval_SummaryDT = data.frame(
      query = "-",
      nbPushed = "0",
      stringsAsFactors = FALSE),
    centered_means = NULL,
    AnaDiff_indices = reactive({NULL}),
    errMsgcalibrationPlotALL = NULL,
    errMsgCalibrationPlot = NULL,
    pi0 = NULL,
    adjusted_pvalues = NULL,
    FDRcontrol_thpval = 0,
    FDR = NULL
  )
  
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
    core.code <- MagellanNTK::Get_Workflow_Core_Code(
      mode = 'process',
      name = id,
      w.names = names(widgets.default.values),
      rv.custom.names = names(rv.custom.default.values)
    )
    
    eval(str2expression(core.code))
    add.resourcePath()
    
    ###########################################################################-
    #
    #-----------------------------DESCRIPTION-----------------------------------
    #
    ###########################################################################-
    output$Description <- renderUI({
      # file <- normalizePath(file.path(session$userData$workflow.path, 
      #   'md', paste0(id, '.md')))
      
      file <- normalizePath(file.path(
        system.file('workflow', package = 'Prostar2'),
        unlist(strsplit(id, '_'))[1], 
        'md', 
        paste0(id, '.Rmd')))
      
      MagellanNTK::process_layout(session,
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
    
    
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Description', btnEvents()))
      req(inherits(dataIn(), 'QFeatures'))
      
      shiny::withProgress(message = paste0("Reseting process", id), {
        shiny::incProgress(0.5)
        rv$dataIn <- dataIn()
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status['Description'] <- MagellanNTK::stepStatus$VALIDATED
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
          uiOutput(ns('Scenario_warningNA_UI'))
        )
      )
    })
    
    
    output$Scenario_choice_UI <- renderUI({
      req(rv$dataIn)
      
      widget1 <- selectInput(ns("Scenario_choice"), "Select a scenario",
                            choices = c("Contrast", "Cluster", "Aggregation"),
                            selected = rv.widgets$Scenario_choice,
                            width = "300px")
      widget2 <- selectInput(ns("Scenario_method"), "Select a method",
                            choices = c("ANOVA", "Limma"),
                            selected = rv.widgets$Scenario_method,
                            width = "300px")
      tagList(
        MagellanNTK::toggleWidget(widget1, rv$steps.enabled["Scenario"]),
        MagellanNTK::toggleWidget(widget2, rv$steps.enabled["Scenario"])
      )
    })
    
    output$Scenario_warningNA_UI <- renderUI({
      req(rv$dataIn)
      
      m <- DaparToolshed::match.metacell(
        DaparToolshed::qMetacell(rv$dataIn[[length(rv$dataIn)]]),
        pattern = c("Missing", "Missing POV", "Missing MEC"),
        level = DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]])
      )
      NA.count <- length(which(m))
      
      if (NA.count > 0) {
        tags$p("Your dataset contains missing values. You must filter/impute them before proceeding.")
      }
    })
    
    
    # >>> END: Definition of the widgets
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Scenario', btnEvents()))
      req(rv$dataIn)
      
      if (is.null(rv$dataIn) ||
          !(rv.widgets$Scenario_choice %in% c("Contrast", "Cluster", "Aggregation")) ||
          !(rv.widgets$Scenario_method %in% c("ANOVA", "Limma"))) {
        shinyjs::info(btnVentsMasg)
        
      } else {
        if (rv.widgets$Scenario_choice == "Contrast"){
          if (rv.widgets$Scenario_method == "ANOVA"){
            anova.models <- DaparToolshed::applyAnovasOnProteins(rv$dataIn, length(rv$dataIn))
            rv.custom$res_pval_FC <- DaparToolshed::testAnovaModels(anova.models, test = "TukeyNoMTC")
            
          }else if (rv.widgets$Scenario_method == "Limma"){
            rv.custom$res_pval_FC <- DaparToolshed::limmaCompleteTest(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]]), design.qf(rv$dataIn), comp.type="OnevsOne")
          }
          
          rv.custom$res_pval_FC_stacked$logFC <- data.frame(Stacked_logFC = unlist(rv.custom$res_pval_FC$logFC, use.names = FALSE))
          rv.custom$res_pval_FC_stacked$P_Value <- data.frame(Stacked_pval = unlist(rv.custom$res_pval_FC$P_Value, use.names = FALSE))
          rv.custom$Scenario_constratnames <- sub("_logFC$", "", colnames(rv.custom$res_pval_FC$logFC))
          
        }else if (rv.widgets$Scenario_choice == "Cluster"){
          if (rv.widgets$Scenario_method == "ANOVA"){
            anova.models <- DaparToolshed::applyAnovasOnProteins(rv$dataIn, length(rv$dataIn))
            rv.custom$res_pval_FC <- DaparToolshed::testAnovaModels(anova.models, test = "Omnibus")
            
          }else if (rv.widgets$Scenario_method == "Limma"){
            rv.custom$res_pval_FC <- DaparToolshed::limmaCompleteTest(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]]), design.qf(rv$dataIn), comp.type="anova1way")
          }
          
        }else if (rv.widgets$Scenario_choice == "Aggregation"){
          if (rv.widgets$Scenario_method == "ANOVA"){
            anova.models <- DaparToolshed::applyAnovasOnProteins(rv$dataIn, length(rv$dataIn))
            rv.custom$res_pval_FC <- DaparToolshed::testAnovaModels(anova.models, test = "TukeyNoMTC")
            
          }else if (rv.widgets$Scenario_method == "Limma"){
            rv.custom$res_pval_FC <- DaparToolshed::limmaCompleteTest(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]]), design.qf(rv$dataIn), comp.type="OnevsOne")
          }
        }
        
        .history <- rv.custom$history[['Scenario']]
        .history[['Scenario']] <- rv.widgets$Scenario_choice
        .history[['Method']] <- rv.widgets$Scenario_method
        rv.custom$history[['Scenario']] <- .history
        
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status["Scenario"] <- MagellanNTK::stepStatus$VALIDATED
      }
    })
    
    
    ###########################################################################-
    #
    #-----------------------------FOLD CHANGE-----------------------------------
    #
    ###########################################################################-
    output$Foldchange <- renderUI({
      shinyjs::useShinyjs()
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns('Foldchange_thlogFC_ui')),
          uiOutput(ns('Foldchange_contrastchoice_ui')),
          uiOutput(ns('Foldchange_uniquechoice_ui'))
        ),
        content = tagList(
          uiOutput(ns('Foldchange_skiptxt_ui')),
          uiOutput(ns('Foldchange_plots_ui'))
        )
      )
    })
    
    output$Foldchange_skiptxt_ui <- renderUI({
      req(rv$steps.status["Scenario"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice != "Contrast")
        
      tags$p(style = "color: red;",
             tags$b("Please validate and proceed to the next step."), 
             " This step is only available for scenario = Contrast."
      )
    })
    
    output$Foldchange_thlogFC_ui <- renderUI({
      req(rv$steps.status["Scenario"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Contrast")
      
      widget <- numericInput(ns("Foldchange_thlogFC"),
                             "log(FC) threshold",
                             value = rv.widgets$Foldchange_thlogFC,
                             min = 0,
                             step = 0.01,
                             width = "150px"
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Foldchange'])
    })
    
    
    output$Foldchange_contrastchoice_ui <- renderUI({
      req(rv$steps.status["Scenario"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Contrast")
      
      widget <- selectInput(ns("Foldchange_contrastchoice"), "Select contrast type",
                            choices = c("Stacked", "Unique"),
                            selected = rv.widgets$Foldchange_contrastchoice,
                            width = "300px")
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Foldchange'])
    })
    
    output$Foldchange_uniquechoice_ui <- renderUI({
      req(rv$steps.status["Scenario"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Foldchange_contrastchoice == "Unique")
      
      widget <- selectInput(ns("Foldchange_uniquechoice"), "Select contrast",
                            choices = rv.custom$Scenario_constratnames,
                            selected = rv.widgets$Foldchange_uniquechoice,
                            width = "300px")
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Foldchange'])
    })
    
    
    
    output$Foldchange_plots_ui <- renderUI({
      req(rv$steps.status["Scenario"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv$dataIn)
      req(rv.widgets$Scenario_choice == "Contrast")
      
      m <- DaparToolshed::match.metacell(
        DaparToolshed::qMetacell(rv$dataIn[[length(rv$dataIn)]]),
        pattern = c("Missing", "Missing POV", "Missing MEC"),
        level = DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]])
      )
      NA.count <- length(which(m))
      if (NA.count > 0) {
        
        tags$p("Your dataset contains missing values. Before using the
      Hypothesis test, you must filter/impute them.")
      } else {
        tagList(
          uiOutput(ns('Foldchange_warning_conditions_ui')),
          uiOutput(ns("Foldchange_swapConds_ui")),
          highcharter::highchartOutput(ns("Foldchange_Plot")),
          highcharter::highchartOutput(ns("FoldchangeStacked_Plot"))
        )
      }
    })
    
    output$Foldchange_Plot <- highcharter::renderHighchart({
      req(rv$steps.status["Scenario"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Contrast")
      
      withProgress(message = "Computing plot...", detail = "", value = 0.5, {
        
        DaparToolshed::hc_logFC_DensityPlot(
          df_logFC = as.data.frame(rv.custom$res_pval_FC$logFC),
          th_logFC = as.numeric(rv.widgets$Foldchange_thlogFC)
        )
      })
    })
    
    output$FoldchangeStacked_Plot <- highcharter::renderHighchart({
      req(rv$steps.status["Scenario"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Contrast")
      
      withProgress(message = "Computing plot...", detail = "", value = 0.5, {
        
        DaparToolshed::hc_logFC_DensityPlot(
          df_logFC = as.data.frame(rv.custom$res_pval_FC_stacked$logFC),
          th_logFC = as.numeric(rv.widgets$Foldchange_thlogFC)
        )
      })
    })
    
    
    # >>> END: Definition of the widgets
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Foldchange', btnEvents()))
      
      if (is.null(rv$dataIn)) {
        shinyjs::info(btnVentsMasg)
        
      } else {
        if (rv.widgets$Foldchange_contrastchoice == "Stacked"){
          rv.custom$contrast_list <- rep(rv.custom$Scenario_constratnames,  
                                         rep(nrow(rv$dataIn[[length(rv$dataIn)]]),length(colnames(rv.custom$res_pval_FC$logFC))))
          rv.custom$res_pval_FC_complete <- rv.custom$res_pval_FC
          rv.custom$res_pval_FC <- rv.custom$res_pval_FC_stacked
          rv.custom$comparison <- "stacked"
          
          
          all_conditions <- DaparToolshed::design.qf(rv$dataIn)$Condition
          max_sample_cond <- max(unlist(lapply(unique(all_conditions), function(x) length(which(all_conditions == x)))))
          rv.custom$cond <- rep(c("cond1", "cond2"), c(max_sample_cond, max_sample_cond))
          
          assay_contr_list <- list()
          meta_contr_list <- list()
          for (contraste_name in rv.custom$Scenario_constratnames){
            names_condition <- unlist(strsplit(as.character(contraste_name), "_vs_"))
            
            assay_list <- list()
            meta_list <- list()
            for (contrast_cond in names_condition){
              index_cond <- which(all_conditions == contrast_cond)
              assay_cond <- SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])[, index_cond]
              meta_cond <- SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])$qMetacell[, index_cond]
              
              diff_nb_sample_cond <- max_sample_cond - length(index_cond) 
              if (diff_nb_sample_cond != 0){
                add_col <- as.data.frame(matrix(NA, nrow = nrow(assay_cond), ncol = diff_nb_sample_cond)) 
                assay_cond <- cbind(assay_cond, add_col)
                meta_cond <- cbind(meta_cond, add_col)
              }
              
              assay_list[[contrast_cond]] <- assay_cond
              meta_list[[contrast_cond]] <- meta_cond
            }
            assay_cond_contr <- do.call(cbind, assay_list)
            meta_cond_contr <- do.call(cbind, meta_list)
            colnames(assay_cond_contr) <- paste0("c", rep(1:2, each = max_sample_cond), "_", rep(1:max_sample_cond, times = 2)) 
            colnames(meta_cond_contr) <- paste0("metacell_", colnames(assay_cond_contr))
            
            assay_contr_list[[contraste_name]] <- assay_cond_contr
            meta_contr_list[[contraste_name]] <- meta_cond_contr
          } 
          assay_contr <- do.call(rbind, assay_contr_list)
          meta_contr <- do.call(rbind, meta_contr_list)
          
          rv.custom$push_pval_data <- SummarizedExperiment::SummarizedExperiment(assay_contr)
          SummarizedExperiment::rowData(rv.custom$push_pval_data)$qMetacell <- meta_contr
          DaparToolshed::idcol(rv.custom$push_pval_data) <- DaparToolshed::idcol(rv$dataIn[[length(rv$dataIn)]])
          SummarizedExperiment::rowData(rv.custom$push_pval_data)[, DaparToolshed::idcol(rv.custom$push_pval_data)] <- SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])[, DaparToolshed::idcol(rv$dataIn[[length(rv$dataIn)]])]
          DaparToolshed::typeDataset(rv.custom$push_pval_data) <- DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]])
          
          rv.custom$res_pval_tmp <- rv.custom$res_pval_FC$P_Value
          
          .history <- rv.custom$history[['Foldchange']]
          .history[['Contrast_Type']] <- rv.widgets$Foldchange_contrastchoice
          .history[['Foldchange_Threshold']] <- rv.widgets$Foldchange_thlogFC
          rv.custom$history[['Foldchange']] <- .history
          
        } else if (rv.widgets$Foldchange_contrastchoice == "Unique") {
          rv.custom$res_pval_FC$P_Value <- rv.custom$res_pval_FC$P_Value[, paste0(rv.widgets$Foldchange_uniquechoice, "_pval"), FALSE]
          rv.custom$res_pval_FC$logFC <- rv.custom$res_pval_FC$logFC[, paste0(rv.widgets$Foldchange_uniquechoice, "_logFC"), FALSE]
          rv.custom$comparison <- rv.widgets$Foldchange_uniquechoice
          
          names_condition <- unlist(strsplit(as.character(rv.widgets$Foldchange_uniquechoice), "_vs_"))
          col_cond <- which(DaparToolshed::design.qf(rv$dataIn)$Condition %in% names_condition)
          rv.custom$push_pval_data <- rv$dataIn[[length(rv$dataIn)]][, col_cond]
          SummarizedExperiment::rowData(rv.custom$push_pval_data)$qMetacell <- SummarizedExperiment::rowData(rv.custom$push_pval_data)$qMetacell[, col_cond]
          DaparToolshed::idcol(rv.custom$push_pval_data) <- DaparToolshed::idcol(rv$dataIn[[length(rv$dataIn)]])
          DaparToolshed::typeDataset(rv.custom$push_pval_data) <- DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]])
          SummarizedExperiment::rowData(rv.custom$push_pval_data)[, DaparToolshed::idcol(rv.custom$push_pval_data)] <- SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])[, DaparToolshed::idcol(rv$dataIn[[length(rv$dataIn)]])]
          rv.custom$cond <- DaparToolshed::design.qf(rv$dataIn)$Condition[col_cond]
          
          rv.custom$res_pval_tmp <- rv.custom$res_pval_FC$P_Value
          
          .history <- rv.custom$history[['Foldchange']]
          .history[['Contrast_Type']] <- rv.widgets$Foldchange_contrastchoice
          .history[['Contrast_Choice']] <- rv.widgets$Foldchange_uniquechoice
          .history[['Foldchange_Threshold']] <- rv.widgets$Foldchange_thlogFC
          rv.custom$history[['Foldchange']] <- .history
        }
        
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status["Foldchange"] <- MagellanNTK::stepStatus$VALIDATED
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
    
    output$Finetuning_pushpval_ui <- renderUI({
      req(rv$steps.status["Foldchange"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Contrast")

      req(rv.custom$pushPval_SummaryDT)
      MagellanNTK::format_DT_ui(ns("dt"))
    })

    MagellanNTK::format_DT_server("dt",
                                  dataIn = reactive({rv.custom$pushPval_SummaryDT}))
    
    output$Finetuning_pushpval_param_ui <- renderUI({
      req(rv$steps.status["Foldchange"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Contrast")
      
      widget <- tagList(
        MagellanNTK::mod_popover_for_help_ui(ns("modulePopover_pushPVal")),
        Prostar2::mod_qMetacell_FunctionFilter_Generator_ui(ns("AnaDiff_query"))
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Finetuning"])
    })
    
    
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
    
    MagellanNTK::mod_popover_for_help_server("modulePopover_keepLines",
                                             title = "n values",
                                             content = "Keep the lines which have at least n intensity values."
    )
    
    GetFiltersScope <- function(){
      c("Whole Line" = "WholeLine",
        "Whole matrix" = "WholeMatrix",
        "For every condition" = "AllCond",
        "At least one condition" = "AtLeastOneCond")
    }

    observe({
      req(rv$steps.status["Foldchange"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Contrast")
      req(rv.custom$push_pval_data)
      req(rv.custom$res_pval_tmp)
      rv.custom$res_pval_tmp
      
      rv.custom$AnaDiff_indices <- Prostar2::mod_qMetacell_FunctionFilter_Generator_server(
        id = "AnaDiff_query",
        dataIn = reactive({rv.custom$push_pval_data}),
        conds = reactive({rv.custom$cond}),
        keep_vs_remove = reactive({stats::setNames(c('Push p-value', 'Keep original p-value'), nm = c("delete", "keep"))}),
        val_vs_percent = reactive({stats::setNames(nm = c("Count", "Percentage"))}),
        operator = reactive({stats::setNames(nm = DaparToolshed::SymFilteringOperators())}),
        remoteReset = reactive({0}),
        is.enabled = reactive({rv$steps.enabled["Finetuning"]})
      )
    })

    observeEvent(req(length(rv.custom$AnaDiff_indices()$value$ll.fun) > 0),{
      req(rv.custom$res_pval_tmp)
      
      .ind <- unlist(rv.custom$AnaDiff_indices()$value$ll.indices)
      .cmd <- rv.custom$AnaDiff_indices()$value$ll.widgets.value[[1]]$keep_vs_remove

      if (length(.ind) > 0 && length(.ind) <= nrow(rv.custom$res_pval_tmp)) {
        if (.cmd == 'delete'){
          indices_to_push <- .ind
        }else if (.cmd == 'keep'){
          indices_to_push <- seq_len(nrow(rv.custom$res_pval_tmp))[-(.ind)]
        }
        rv.custom$res_pval_tmp[indices_to_push, ] <- 1.00000000001
        nbPushed <- length(indices_to_push)
        
        query <- rv.custom$AnaDiff_indices()$value$ll.query
        
        rv.custom$pushPval_SummaryDT <- rbind(
          rv.custom$pushPval_SummaryDT ,
          c(query, nbPushed))
        
        .history <- rv.custom$history[['Push_pval']]
        .history[[paste0('query_', length(.history)+1)]] <- paste0(query, " ; Number pval pushed to 1 : ", length(indices_to_push))
        rv.custom$history[['Push_pval']] <- .history
      }
    })
    
    

    output$Finetuning_cluster_ui <- renderUI({
      req(rv$steps.status["Foldchange"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Cluster")
      
    })
    
    output$Finetuning_cluster_param_ui <- renderUI({
      req(rv$steps.status["Foldchange"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Cluster")
      
      widget1 <- selectInput(ns("Finetuning_cluster_protprofile"), "Select protein profile method",
                            choices = c("Mean", "Median"),
                            selected = rv.widgets$Finetuning_cluster_protprofile,
                            width = "300px")
      widget2 <- selectInput(ns("Finetuning_cluster_method"), "Select clustering method", ###NB_ PAS BESOIN SI NCOND = 2
                             choices = c("kmeans", "affinityProp", "affinityPropReduced"),
                             selected = rv.widgets$Finetuning_cluster_method,
                             width = "300px")
      
      tagList(
        MagellanNTK::toggleWidget(widget1, rv$steps.enabled['Finetuning']),
        MagellanNTK::toggleWidget(widget2, rv$steps.enabled['Finetuning'])
      )
    })
    
    output$Finetuning_cluster_param_kmeans_ui <- renderUI({
      req(rv$steps.status["Foldchange"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Cluster")
      req(rv.widgets$Finetuning_cluster_method == "kmeans")
      
      widget1 <- selectInput(ns("Finetuning_cluster_nbclust"), "Number of clusters",
                             choices = 1:length(unique(DaparToolshed::design.qf(rv$dataIn)$Condition)),
                             selected = rv.widgets$Finetuning_cluster_nbclust,
                             width = "300px")
      
      MagellanNTK::toggleWidget(widget1, rv$steps.enabled['Finetuning'])
    })
    
    
    
    output$Finetuning_aggregation_ui <- renderUI({
      req(rv$steps.status["Foldchange"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Aggregation")
      
    })
    
    output$Finetuning_aggregation_param_ui <- renderUI({
      req(rv$steps.status["Foldchange"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Aggregation")
      
      widget1 <- selectInput(ns("Finetuning_aggreg_method"), "Select p-value agggregation method",
                             choices = c("Most significant FC", "Worst FC"),
                             selected = rv.widgets$Finetuning_aggreg_method,
                             width = "300px")
      
      MagellanNTK::toggleWidget(widget1, rv$steps.enabled['Finetuning'])
    })
    
    
    # >>> END: Definition of the widgets
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Finetuning', btnEvents()))
      
      if (is.null(rv$dataIn)) {
        shinyjs::info(btnVentsMasg)
        
      } else {
        if (rv.widgets$Scenario_choice == "Contrast"){
          rv.custom$res_pval_FC$P_Value <- rv.custom$res_pval_tmp
          
        } else if (rv.widgets$Scenario_choice == "Cluster"){
          rv.custom$comparison <- "omnibus_cluster"
          
          conds <- DaparToolshed::design.qf(rv$dataIn)$Condition
          prot_prof <- switch(rv.widgets$Finetuning_cluster_protprofile,
                 Mean = sapply(unique(conds), function(c) 
                   rowMeans(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])[, conds == c, drop = FALSE], 
                            na.rm = TRUE)),
                 Median = sapply(unique(conds), function(c) 
                   rowMedians(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])[, conds == c, drop = FALSE], 
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
            for (i in 1:nrow(standards)) {
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
          
          .history <- rv.custom$history[['Clustering']]
          .history[['Clustering_Method']] <- rv.widgets$Finetuning_cluster_method
          .history[['Number_of_Clusters']] <- unique(rv.custom$res_clusters)
          .history[['Protein_Profile_Method']] <- rv.widgets$Finetuning_cluster_protprofile
          rv.custom$history[['Clustering']] <- .history
          
        } else if (rv.widgets$Scenario_choice == "Aggregation") {
          rv.custom$res_pval_FC_complete <- rv.custom$res_pval_FC
          rv.custom$comparison <- "aggregated"
          nb_contr <- ncol(rv.custom$res_pval_FC$P_Value)
          
          if (rv.widgets$Finetuning_aggreg_method == "Most significant FC"){
            best.pval <- apply(rv.custom$res_pval_FC$P_Value, 1, min)
            sidak.pval <- 1-(1-best.pval)^nb_contr
            
          }else if (rv.widgets$Finetuning_aggreg_method == "Worst FC"){
            worst.pval <- apply(rv.custom$res_pval_FC$P_Value, 1, max)
            sidak.pval <- worst.pval^nb_contr
          }
          
          rv.custom$res_pval_FC$P_Value <- sidak.pval
          
          .history <- rv.custom$history[['Aggregation']]
          .history[['Aggregation_Method']] <- rv.widgets$Finetuning_aggreg_method
          rv.custom$history[['Aggregation']] <- .history
        }
        
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
          highcharter::highchartOutput(ns("histPValue")),
          imageOutput(ns("calibrationPlotAll"), height = "800px"),
          imageOutput(ns("calibrationPlot"), height = "400px")
          # p(tags$strong(
          #   paste0("value of pi0: ", round(as.numeric(rv.custom$pi0), digits = 2))
          # ))
        )
      )
    })
    
    ###WIDGET SIDEBAR
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
      req(rv$steps.status["Finetuning"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.custom$res_pval_FC)
      req(rv.custom$pi0)
      
      widget <- selectInput(
        ns("Pvaluecalibration_nBinsHistpval"), 
        "n bins of p-value histogram",
        choices = c(1, seq(from = 0, to = 100, by = 10)[-1]),
        selected = rv.widgets$Pvaluecalibration_nBinsHistpval, 
        width = "80px")
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pvaluecalibration"])
    })
    

    
    ###GRAPH HISTO CONTENT
    histPValue <- reactive({
      req(rv.custom$res_pval_FC)
      req(rv.custom$pi0)
      req(rv.widgets$Pvaluecalibration_nBinsHistpval)
      req(rv.widgets$Foldchange_thlogFC)
      req(!is.na(rv.widgets$Foldchange_thlogFC))
      req(length(rv.custom$res_pval_FC$logFC) > 0)

      if (rv.widgets$Foldchange_contrastchoice == "Stacked"){
        t <- unlist(rv.custom$res_pval_FC_stacked$P_Value)
        t <- t[which(abs(rv.custom$res_pval_FC_stacked$logFC) >= rv.widgets$Foldchange_thlogFC)]
      }else{
        t <- unlist(rv.custom$res_pval_FC$P_Value)
        if (rv.widgets$Scenario_choice == "Contrast"){
          t <- t[which(abs(rv.custom$res_pval_FC$logFC) >= rv.widgets$Foldchange_thlogFC)]
        }
      }
      toDelete <- which(t > 1)
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


    # ###GRAPH ALL CONTENT
    calibrationPlotAll <- reactive({
      rv.custom$res_pval_FC
      req(rv$steps.status["Finetuning"] == MagellanNTK::stepStatus$VALIDATED)
      req(!is.na(rv.widgets$Foldchange_thlogFC))
      req(length(rv.custom$res_pval_FC$logFC) > 0)

      if (rv.widgets$Foldchange_contrastchoice == "Stacked"){
        t <- unlist(rv.custom$res_pval_FC_stacked$P_Value)
        t <- t[which(abs(rv.custom$res_pval_FC_stacked$logFC) >= rv.widgets$Foldchange_thlogFC)]
      }else{
        t <- unlist(rv.custom$res_pval_FC$P_Value)
        if (rv.widgets$Scenario_choice == "Contrast"){
          t <- t[which(abs(rv.custom$res_pval_FC$logFC) >= rv.widgets$Foldchange_thlogFC)]
        }
      }
      toDelete <- which(t > 1)
      if (length(toDelete) > 0) {
        t <- t[-toDelete]
      }

      l <- NULL
      result <- tryCatch(
        {
          l <- MagellanNTK::catchToList(wrapperCalibrationPlot(t, "ALL"))
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
      for (i in 1:length(rv.custom$errMsgCalibrationPlotAll)) {
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
      if (rv.widgets$Foldchange_contrastchoice == "Stacked"){
        t <- unlist(rv.custom$res_pval_FC_stacked$P_Value)
        t <- t[which(abs(rv.custom$res_pval_FC_stacked$logFC) >= rv.widgets$Foldchange_thlogFC)]
      }else{
        t <- unlist(rv.custom$res_pval_FC$P_Value)
        if (rv.widgets$Scenario_choice == "Contrast"){
          t <- t[which(abs(rv.custom$res_pval_FC$logFC) >= rv.widgets$Foldchange_thlogFC)]
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

            ll <- MagellanNTK::catchToList(
              wrapperCalibrationPlot(
                t,
                rv.widgets$Pvaluecalibration_numericValCalibration
              )
            )
            .warns <- ll$warnings[grep("Warning:", ll$warnings)]
            rv.custom$errMsgCalibrationPlot <- .warns
          } else if (rv.widgets$Pvaluecalibration_calibrationMethod == "Benjamini-Hochberg") {
            ll <- MagellanNTK::catchToList(wrapperCalibrationPlot(t, 1))
            .warns <- ll$warnings[grep("Warning:", ll$warnings)]
            rv.custom$errMsgCalibrationPlot <- .warns
          } else {
            ll <- MagellanNTK::catchToList(
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
      for (i in 1:length(rv.custom$errMsgCalibrationPlot)) {
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


    
    # >>> END: Definition of the widgets
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Pvaluecalibration', btnEvents()))
      
      if (is.null(rv$dataIn)) {
        shinyjs::info(btnVentsMasg)
        
      } else {
        .history <- rv.custom$history[['Pvaluecalibration']]
        .history[['Calibration_Method']] <- GetCalibrationMethod()
        .history[['pi0']] <- rv.custom$pi0
        rv.custom$history[['Pvaluecalibration']] <- .history
        
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
          uiOutput(ns('FDRcontrol_plot_aggreg_ui'))
        )
      )
    })
    
    output$FDRcontrol_widgets_ui <- renderUI({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      
      widget <- tags$div(
        mod_set_pval_threshold_ui(ns("FDRcontrol_signifthreshold")),
        checkboxInput(ns('FDRcontrol_viewAdjPval'), 
                      'View adjusted p-value', 
                      value = rv.widgets$FDRcontrol_viewAdjPval)
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDRcontrol"])
    })
    
    output$FDRcontrol_plot_cluster_ui <- renderUI({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$Scenario_choice == "Cluster")
      req(Build_pval_table()$Adjusted_PValue)
      

      visualizeClusters(
        dat = rv.custom$centered_means,
        clust_model = as.factor(rv.custom$res_clusters),
        adjusted_pValues = Build_pval_table()$Adjusted_PValue, 
        FDR_th = rv.custom$FDRcontrol_thpval
      )
    })
    
    # output$FDRcontrol_widgets_contrast_ui <- renderUI({
    #   req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
    #   req(rv.widgets$Foldchange_contrastchoice == "Stacked")
    #   
    #   widget <- selectInput(ns("FDRcontrol_volcanocontrast"), "Select contrast",
    #                         choices = rv.custom$Scenario_constratnames,
    #                         selected = rv.widgets$FDRcontrol_volcanocontrast,
    #                         width = "300px")
    #   
    #   MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDRcontrol"])
    # })
    
    
    GetComparisons <- reactive({
      req(rv.widgets$Scenario_choice == "Contrast")
      
      if(rv.widgets$Foldchange_contrastchoice == "Stacked"){
        comp <- unlist(strsplit(as.character(rv.widgets$FDRcontrol_volcanocontrast), "_vs_"))
      }else{
        comp <- unlist(strsplit(as.character(rv.widgets$Foldchange_uniquechoice), "_vs_"))
      }
      
      comp
    })
    
    # Prostar2::mod_volcanoplot_server(
    #   id = "FDR_volcano",
    #   dataIn = reactive({Get_Dataset_to_Analyze()}), !!!
    #   comparison = reactive({GetComparisons()}),
    #   group = reactive({DaparToolshed::design.qf(rv$dataIn)$Condition}),
    #   thlogfc = reactive({rv.widgets$Foldchange_thlogFC}),
    #   thpval = reactive({rv.custom$FDRcontrol_thpval}),
    #   tooltip = reactive({rv.custom$Pairwisecomparison_tooltipInfo}), !!!
    #   remoteReset = reactive({remoteReset()}),
    #   is.enabled = reactive({rv$steps.enabled["FDRcontrol"]})
    # )
    # 
    # output$FDR_volcanoplot_UI <- renderUI({
    #   req(rv.widgets$Scenario_choice == "Contrast")
    #   
    #   widget <- div(
    #     mod_volcanoplot_ui(ns("FDR_volcano"))
    #   )
    #   
    #   MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDRcontrol"])
    # })
    
    
    logpval <- Prostar2::mod_set_pval_threshold_server(id = "FDRcontrol_signifthreshold",
                                                       pval_init = reactive({10^(-rv.custom$FDRcontrol_thpval)}),
                                                       fdr = reactive({Get_FDR()}),
                                                       remoteReset = reactive({0}),
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
                    options = list(initComplete = initComplete(),
                                   dom = "frtip",
                                   pageLength = 100,
                                   scrollY = 500,
                                   scroller = TRUE,
                                   server = FALSE,
                                   columnDefs = .coldefs,
                                   ordering = !rv.widgets$FDRcontrol_viewAdjPval
                    )
      ) %>%
        DT::formatStyle(
          paste0("isDifferential (",
                 as.character(rv.custom$comparison), ")"),
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
    
    
    output$FDR_download_SelectedItems_UI <- downloadHandler(
      filename = function() {rv.custom$filename},
      content = function(fname) {
        wb <- BuildPairwiseComp_wb()
        openxlsx::saveWorkbook(wb, file = fname, overwrite = TRUE)
      }
    )
    

    Get_FDR <- reactive({
      req(rv.custom$FDRcontrol_thpval)
      req(Build_pval_table())
      
      adj.pval <- Build_pval_table()$Adjusted_PValue
      logpval <- Build_pval_table()$Log_PValue
      .logfc <- Build_pval_table()$logFC
      upitems_logpval <- which(logpval >= rv.custom$FDRcontrol_thpval)
      upItems_logfcinf <- which(abs(.logfc) < rv.widgets$Foldchange_thlogFC)
      upitems_logpval <- setdiff(upitems_logpval, upItems_logfcinf)
      
      fdr <- max(adj.pval[upitems_logpval], na.rm = TRUE)
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
      req(rv.widgets$Foldchange_thlogFC)
      req(rv.custom$FDRcontrol_thpval)
      req(rv$dataIn)
      req(GetCalibrationMethod())
      
      #rv.widgets$Pairwisecomparison_Comparison
      .logfc <- unlist(rv.custom$res_pval_FC$logFC)
      .pval <- unlist(rv.custom$res_pval_FC$P_Value)
      .digits <- 3
      
      if (rv.widgets$Foldchange_contrastchoice == "Stacked"){
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
                                 which(abs(pval_table$logFC) >= rv.widgets$Foldchange_thlogFC))
        pval_table[signifItems,'isDifferential'] <- 1
        
        upItems_pval <- which(-log10(.pval) >= rv.custom$FDRcontrol_thpval)
        #push to 1 proteins with logFC under threshold
        pval_pushfc <- .pval
        upItems_logfcinf <- which(abs(.logfc) < rv.widgets$Foldchange_thlogFC)
        upItems_pushepval <- which(.pval > 1)
        upItems_logfcinf <- setdiff(upItems_logfcinf, upItems_pushepval)
        if (length(upItems_logfcinf) != 0){
          pval_pushfc[upItems_logfcinf] <- 1
        }  
        if (length(upItems_pushepval) != 0){
          pval_pushfc <- pval_pushfc[-upItems_pushepval]
        }
        rv.custom$adjusted_pvalues <- diffAnaComputeAdjustedPValues(
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
        
        rv.custom$adjusted_pvalues <- diffAnaComputeAdjustedPValues(
          .pval,
          GetCalibrationMethod())
        pval_table[, 'Adjusted_PValue'] <- rv.custom$adjusted_pvalues
        }

      # Set only significant values
      pval_table$logFC <- signif(pval_table$logFC, digits = 4)
      pval_table$P_Value <- signif(pval_table$P_Value, digits = 4)
      pval_table$Adjusted_PValue <- signif(pval_table$Adjusted_PValue, digits = 4)
      pval_table$Log_PValue <- signif(pval_table$Log_PValue, digits = 4)
      
      if (rv.widgets$Foldchange_contrastchoice == "Stacked"){
        tmp <- as.data.frame(
          rep(SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])[, rv.custom$Pairwisecomparison_tooltipInfo], 
              ncol(rv.custom$res_pval_FC_complete$P_Value))
        )
      }else{
        tmp <- as.data.frame(
          SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])[, rv.custom$Pairwisecomparison_tooltipInfo])
      }
      
      names(tmp) <- rv.custom$Pairwisecomparison_tooltipInfo
      pval_table <- cbind(pval_table, tmp)
      colnames(pval_table)[2:6] <- paste0(colnames(pval_table)[2:6], " (", as.character(rv.custom$comparison), ")")
      
      pval_table
    })
    
    isContainedIn <- function(strA, strB) {
      return(all(strA %in% strB))
    }
    
    # >>> END: Definition of the widgets
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('FDRcontrol', btnEvents()))
      
      if (is.null(rv$dataIn)) {
        shinyjs::info(btnVentsMasg)
        
      } else {
        .history <- rv.custom$history[['FDRcontrol']]
        .history[['FDR_Threshold']] <- rv.custom$FDRcontrol_thpval
        rv.custom$history[['FDRcontrol']] <- .history
        
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
        content = uiOutput(ns('dl_ui'))
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
          
          # DO NOT MODIFY THE THREE FOLLOWINF LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- rv$dataIn
          rv$steps.status['Save'] <- MagellanNTK::stepStatus$VALIDATED
          
          
          Prostar2::download_dataset_server(paste0(id, '_createQuickLink'), dataIn = reactive({dataOut$value}))
        })
      }
    })
    
    # <<< end ------------------------------------------------------------------

    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  }
  )
}
