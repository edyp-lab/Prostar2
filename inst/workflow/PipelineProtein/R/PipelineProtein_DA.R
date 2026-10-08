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
PipelineProtein_DA_conf <- function() {
  MagellanNTK::Config(
    fullname = "PipelineProtein_DA",
    mode = "process",
    steps = c("Pairwise comparison", "P-value calibration", "FDR"),
    mandatory = c(TRUE, TRUE, TRUE)
  )
}


#' @rdname PipelineProtein_DA
#' @export
#'
PipelineProtein_DA_ui <- function(id) {
  ns <- NS(id)
}


#' @rdname PipelineProtein_DA
#' @export
#'
PipelineProtein_DA_server <- function(
  id,
  dataIn = reactive({
    NULL
  }),
  steps.enabled = reactive({
    NULL
  }),
  remoteReset = reactive({
    0
  }),
  steps.status = reactive({
    NULL
  }),
  current.pos = reactive({
    1
  }),
  btnEvents = reactive({
    NULL
  })
) {
  pkgs_require(c("QFeatures", "SummarizedExperiment", "S4Vectors", "magrittr", "grDevices"))
  requireNamespace("DaparToolshed")

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
      TotalPushed = "0",
      TotalNonPushed = "0",
      stringsAsFactors = FALSE
    ),
    thpval = 0,
    thlogfc = 0,
    containsNA = NULL,
    DatasettoAnalyze = NULL,
    nbTotalAnaDiff = NULL,
    calibrationRes = NULL,
    errMsgcalibrationPlot = NULL,
    errMsgCalibrationPlotAll = NULL,
    GetCalibrationMethod = NULL,
    pi0 = NULL,
    AnaDiff_indices = reactive({
      NULL
    }),
    Condition1 = NULL,
    Condition2 = NULL,
    FDR_tooltipInfo = NULL
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
      mode = "process",
      name = id,
      w.names = names(widgets.default.values),
      rv.custom.names = names(rv.custom.default.values)
    )

    eval(str2expression(core.code))
    add_resourcePath()


    ########################################################################### -
    #
    #-----------------------------DESCRIPTION-----------------------------------
    #
    ########################################################################### -
    output$Description <- renderUI({
      file <- normalizePath(file.path(
        system.file("workflow", package = "Prostar2"),
        unlist(strsplit(id, "_"))[1],
        "md",
        paste0(id, ".Rmd")
      ))

      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(),
        content = tagList(
          if (file.exists(file)) {
            includeMarkdown(file)
          } else {
            p("No Description available")
          }
        )
      )
    })

    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl("Description", btnEvents()))
      req(dataIn())
      req(inherits(dataIn(), "QFeatures"))

      shiny::withProgress(message = paste0("Preparing data", id), {
        shiny::incProgress(0.5)

        # Copy the input dataset to use it during this step
        rv$dataIn <- dataIn()

        # Adds an assay to work on
        last_se <- rv$dataIn[[length(rv$dataIn)]]
        rv$dataIn <- QFeatures::addAssay(
          rv$dataIn,
          last_se,
          "DA"
        )

        # Get logfc threshold from Hypothesis test dataset
        history_tmp <- DaparToolshed::paramshistory(last_se)
        ind_logFC <- which(history_tmp[, "Parameter"] == "thlogFC")
        rv.custom$thlogfc <- as.numeric(history_tmp[ind_logFC, "Value"])

        # Initialize push p-value table
        rv.custom$Pairwisecomparison_pushPval_SummaryDT <- data.frame(
          comparison = "-",
          query = "-",
          nbPushed = "0",
          TotalPushed = "0",
          TotalNonPushed = nrow(last_se),
          stringsAsFactors = FALSE
        )

        # Update reactive values
        rv.custom$res_AllPairwiseComparisons <- DaparToolshed::HypothesisTest(last_se)
        rv.widgets$Pairwisecomparison_tooltipInfo <- DaparToolshed::idcol(last_se)
        rv.custom$comparisonNames <- Get_Pairwisecomparison_Names(rv.custom$res_AllPairwiseComparisons)
        rv.custom$pushed <- setNames(rep(list(NULL), length(rv.custom$comparisonNames)), rv.custom$comparisonNames)

        # DO NOT MODIFY THE NEXT THREE LINES
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status["Description"] <- MagellanNTK::stepStatus$VALIDATED
        shiny::incProgress(1)
      })
    })


    ########################################################################### -
    #
    #--------------------------PAIRWISE COMPARISON------------------------------
    #
    ########################################################################### -
    output$Pairwisecomparison <- renderUI({
      shinyjs::useShinyjs()

      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          tags$div(
            id = ns("div_Pairwisecomparison_Comparison_UI"),
            uiOutput(ns("Pairwisecomparison_Comparison_UI")),
            uiOutput(ns("Pairwisecomparison_pushpval_UI"))
          )
        ),
        content = tagList(
          div(
            style = "display: flex; margin-top: 10px; gap: 25px;",
            uiOutput(ns("Pairwisecomparison_volcano_UI")),
            uiOutput(ns("Pairwisecomparison_tooltipInfo_UI"))
          ),
          br(),
          uiOutput(ns("Pairwisecomparison_pushPval_DT_UI")),
          br()
        )
      )
    })

    #### _sidebar -----
    # Widget - select comparison
    output$Pairwisecomparison_Comparison_UI <- renderUI({
      req(rv.custom$res_AllPairwiseComparisons)

      widget <- selectInput(ns("Pairwisecomparison_Comparison"), "Select a comparison",
        choices = c("None", rv.custom$comparisonNames),
        selected = rv.widgets$Pairwisecomparison_Comparison,
        width = "200px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pairwisecomparison"])
    })

    # Widget - filter creation for push p-values (server)
    observe({
      req(rv$steps.enabled["Pairwisecomparison"])
      req(rv$dataIn)
      req(rv.widgets$Pairwisecomparison_Comparison)
      req(rv.widgets$Pairwisecomparison_Comparison != "None")

      if (length(grep("all-", rv.widgets$Pairwisecomparison_Comparison)) == 1) {
        conds <- DaparToolshed::design_qf(rv$dataIn)$Condition
      } else {
        split <- unlist(strsplit(rv.widgets$Pairwisecomparison_Comparison, "_vs_"))
        idxcond <- DaparToolshed::design_qf(rv$dataIn)$Condition %in% split
        conds <- DaparToolshed::design_qf(rv$dataIn)$Condition[idxcond]
      }

      rv.custom$AnaDiff_indices <- Prostar2::mod_qMetacell_FunctionFilter_Generator_server(
        id = "AnaDiff_query",
        dataIn = reactive({
          Get_Dataset_to_Analyze()
        }),
        conds = reactive({
          conds
        }),
        keep_vs_remove = reactive({
          stats::setNames(c("Push p-value", "Keep original p-value"),
            nm = c("delete", "keep")
          )
        }),
        remoteReset = reactive({
          remoteReset()
        }),
        is.enabled = reactive({
          rv$steps.enabled["Pairwisecomparison"]
        })
      )
    })

    # Widget - filter creation for push p-values (ui)
    output$Pairwisecomparison_pushpval_UI <- renderUI({
      req(rv.widgets$Pairwisecomparison_Comparison != "None")

      widget <- tagList(
        p(
          style = "font-weight: bold;margin-bottom: 5px;",
          "Push p-value"
        ),
        Prostar2::mod_qMetacell_FunctionFilter_Generator_ui(ns("AnaDiff_query"))
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pairwisecomparison"])
    })

    #### _content -----
    # Plot - volcano plot (ui)
    output$Pairwisecomparison_volcano_UI <- renderUI({
      widget <- div(
        id = ns("div_Pairwisecomparison_volcano"),
        mod_volcanoplot_ui(ns("Pairwisecomparison_volcano"))
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pairwisecomparison"])
    })

    # Plot - volcano plot (server)
    Prostar2::mod_volcanoplot_server(
      id = "Pairwisecomparison_volcano",
      dataIn = reactive({
        Get_Dataset_to_Analyze()
      }),
      comparison = reactive({
        c(rv.custom$Condition1, rv.custom$Condition2)
      }),
      group = reactive({
        DaparToolshed::design_qf(rv$dataIn)$Condition
      }),
      thlogfc = reactive({
        rv.custom$thlogfc
      }),
      tooltip = reactive({
        rv.custom$Pairwisecomparison_tooltipInfo
      }),
      remoteReset = reactive({
        remoteReset()
      })
    )

    # Widget - tooltip selection
    output$Pairwisecomparison_tooltipInfo_UI <- renderUI({
      req(rv$dataIn)
      req(rv.widgets$Pairwisecomparison_Comparison != "None")
      req(rv.widgets$Pairwisecomparison_tooltipInfo)

      widget <- tagList(
        div(style = "margin-top: 25px;", ""),
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
          class = "btn-info"
        )
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pairwisecomparison"])
    })

    observeEvent(input$Pairwisecomparison_validTooltipInfo, {
      rv.custom$Pairwisecomparison_tooltipInfo <- rv.widgets$Pairwisecomparison_tooltipInfo
    })

    # Push p-values
    observeEvent(req(length(rv.custom$AnaDiff_indices()$value$ll.fun) > 0), {
      ind <- unlist(rv.custom$AnaDiff_indices()$value$ll.indices)

      if (length(ind) > 1 && length(ind) <= nrow(Get_Dataset_to_Analyze())) {
        result <- pushPvalues(
          data = rv$dataIn[[length(rv$dataIn)]],
          ind = ind,
          command = rv.custom$AnaDiff_indices()$value$ll.widgets.value[[1]]$keep_vs_remove,
          query = rv.custom$AnaDiff_indices()$value$ll.query,
          comparison = rv.widgets$Pairwisecomparison_Comparison,
          dt = rv.custom$Pairwisecomparison_pushPval_SummaryDT,
          pushed = rv.custom$pushed
        )

        rv$dataIn[[length(rv$dataIn)]] <- result$data
        rv.custom$res_AllPairwiseComparisons <- DaparToolshed::HypothesisTest(result$data)
        rv.custom$Pairwisecomparison_pushPval_SummaryDT <- result$dt
        rv.custom$pushed <- result$pushed
      }
    })

    # Update pushed p-values DT
    observeEvent(
      {
        rv.widgets$Pairwisecomparison_Comparison
        rv.custom$Pairwisecomparison_pushPval_SummaryDT
      },
      {
        req(rv.widgets$Pairwisecomparison_Comparison != "None")
        req(rv.custom$Pairwisecomparison_pushPval_SummaryDT)

        rv.custom$Pairwisecomparison_pushPval_SummaryDT_comp <- updatePushedDT(
          dt = rv.custom$Pairwisecomparison_pushPval_SummaryDT,
          comparison = rv.widgets$Pairwisecomparison_Comparison
        )
      }
    )

    # Pushed p-values DT (ui)
    output$Pairwisecomparison_pushPval_DT_UI <- renderUI({
      req(rv.widgets$Pairwisecomparison_Comparison != "None")
      req(rv.custom$Pairwisecomparison_pushPval_SummaryDT_comp)

      MagellanNTK::format_DT_ui(ns("dt"))
    })

    # Pushed p-values DT (server)
    MagellanNTK::format_DT_server("dt",
      dataIn = reactive({
        rv.custom$Pairwisecomparison_pushPval_SummaryDT_comp
      })
    )

    # Dataset with only the considered conditions
    Get_Dataset_to_Analyze <- reactive({
      req(rv$dataIn)
      req(rv.widgets$Pairwisecomparison_Comparison != "None")

      split <- unlist(strsplit(rv.widgets$Pairwisecomparison_Comparison, "_vs_"))
      rv.custom$Condition1 <- split[1]
      rv.custom$Condition2 <- split[2]

      GetdatasetToAnalyze(
        data = rv$dataIn,
        comparison = rv.widgets$Pairwisecomparison_Comparison
      )
    })

    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl("Pairwisecomparison", btnEvents()))
      req(rv$dataIn)

      if (rv.widgets$Pairwisecomparison_Comparison == "None") {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("Preparing contrast", id), {
          shiny::incProgress(0.5)

          # Update values
          .logfc <- paste0(rv.widgets$Pairwisecomparison_Comparison, "_logFC")
          .pval <- paste0(rv.widgets$Pairwisecomparison_Comparison, "_pval")
          rv.custom$resAnaDiff <- list(
            logFC = (rv.custom$res_AllPairwiseComparisons)[, .logfc],
            P_Value = (rv.custom$res_AllPairwiseComparisons)[, .pval],
            condition1 = rv.custom$Condition1,
            condition2 = rv.custom$Condition2,
            pushed = length(unlist(rv.custom$pushed[rv.widgets$Pairwisecomparison_Comparison]))
          )

          rv.custom$DatasettoAnalyze <- Get_Dataset_to_Analyze()
          rv.custom$containsNA <- checkNA(rv$dataIn)

          # Perform pairwise comparison sub-step
          result <- pairwiseComparisonProt(
            history = rv.custom$history,
            comparison = rv.widgets$Pairwisecomparison_Comparison,
            dt = rv.custom$Pairwisecomparison_pushPval_SummaryDT_comp,
            pushed = rv.custom$resAnaDiff$pushed
          )
          rv.custom$history <- result$history


          # DO NOT MODIFY THE NEXT THREE LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["Pairwisecomparison"] <- MagellanNTK::stepStatus$VALIDATED
          shiny::incProgress(1)
        })
      }
    })


    ########################################################################### -
    #
    #--------------------------P-VALUE CALIBRATION------------------------------
    #
    ########################################################################### -
    output$Pvaluecalibration <- renderUI({
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns("Pvaluecalibration_calibrationMethod_UI")),
          uiOutput(ns("Pvaluecalibration_numericValCalibration_UI")),
          div(
            style = "margin: 10px 0px 15px 20px; font-size: 20px; font-weight: 900;",
            paste0("pi0 = ", round(as.numeric(rv.custom$pi0), digits = 2))
          ),
          div(
            style = "white-space: nowrap;",
            uiOutput(ns("Pvaluecalibration_nBins_UI"))
          )
        ),
        content = tagList(
          fluidRow(
            tags$style(HTML(".cp-container img {margin: 0 !important;}")),
            div(
              class = "cp-container", style = "display: flex; margin-top: 10px; margin-bottom: 125px;",
              imageOutput(ns("calibrationPlotAll")),
              imageOutput(ns("calibrationPlot"))
            ),
            plotly::plotlyOutput(ns("histPValue"))
          )
        )
      )
    })

    #### _sidebar -----
    # Widget - calibration method
    output$Pvaluecalibration_calibrationMethod_UI <- renderUI({
      widget <- selectInput(ns("Pvaluecalibration_calibrationMethod"), "Calibration method",
        choices = c("None" = "None", GetCalibMethod()),
        selected = rv.widgets$Pvaluecalibration_calibrationMethod,
        width = "200px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pvaluecalibration"])
    })

    # Widget - numeric value
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

    # Update numeric value
    observeEvent(input$Pvaluecalibration_numericValCalibration, {
      rv.widgets$Pvaluecalibration_numericValCalibration <- input$Pvaluecalibration_numericValCalibration
    })

    # Debounce the reactive value ONLY for the plot (300ms delay)
    # Makes it so the plot does not flicker if changes too quickly
    debounced_numericVal <- debounce(
      reactive({
        rv.widgets$Pvaluecalibration_numericValCalibration
      }),
      300 # Adjust delay as needed
    )

    # Widget - nb bins
    output$Pvaluecalibration_nBins_UI <- renderUI({
      req(rv.custom$resAnaDiff)
      req(rv.custom$pi0)

      widget <- selectInput(
        ns("Pvaluecalibration_nBinsHistpval"),
        "n bins of p-value histogram",
        choices = c(1, seq(from = 10, to = 100, by = 10)),
        selected = rv.widgets$Pvaluecalibration_nBinsHistpval,
        width = "100px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Pvaluecalibration"])
    })

    #### _content -----
    # Get values for all methods calibration plot
    calibrationAllData <- reactive({
      req(rv.custom$resAnaDiff)
      req(rv$dataIn)
      req(length(rv.custom$resAnaDiff$logFC) > 0)
      req(!rv.custom$containsNA)

      getPValueNonPushed(rv.custom$resAnaDiff$P_Value)
    })

    # Plot - all methods calibration plot
    output$calibrationPlotAll <- renderImage(
      {
        dat <- calibrationAllData()
        withProgress(message = "", detail = "", value = 0, {
          incProgress(0.5, detail = "Building calibration plots...")

          outfile <- tempfile(fileext = ".png")
          # Open the PNG device FIRST
          grDevices::png(outfile, width = 600, height = 500)
          # Run the function while the PNG device is active.
          # wrapperCalibrationPlot() draws into this device.
          ll <- tryCatch(
            catchToList(DaparToolshed::wrapperCalibrationPlot(
              dat,
              "ALL"
            )),
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
      },
      deleteFile = TRUE
    )

    # Txt if error in all methods calibration plot
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

      div(
        id = ns("div_errMsgCalibrationPlotAll"),
        HTML(txt), style = "color:red"
      )
    })

    # Get values for selected method calibration plot
    calibrationData <- reactive({
      req(rv$dataIn)
      req(rv.custom$resAnaDiff)
      req(length(rv.custom$resAnaDiff$logFC) > 0)
      req(!rv.custom$containsNA)

      pval <- getPValueNonPushed(rv.custom$resAnaDiff$P_Value)

      calibration_value <- get_calibration_method(
        rv.widgets$Pvaluecalibration_calibrationMethod,
        debounced_numericVal()
      )

      list(pval = pval, calibration_value = calibration_value)
    })

    # Plot - selected method calibration plot
    output$calibrationPlot <- renderImage(
      {
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
              DaparToolshed::wrapperCalibrationPlot(
                dat$pval,
                dat$calibration_value
              )
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
      },
      deleteFile = TRUE
    )

    # Txt if error in selected method calibration plot
    output$errMsgCalibrationPlot <- renderUI({
      req(rv.custom$errMsgCalibrationPlot)
      req(rv$dataIn)

      txt <- NULL

      for (i in seq_along(rv.custom$errMsgCalibrationPlot)) {
        txt <- paste(txt, "errMsgCalibrationPlot: ",
          rv.custom$errMsgCalibrationPlot[i], "<br>",
          sep = ""
        )
      }

      div(
        id = ns("div_errMsgCalibrationPlot"),
        HTML(txt), style = "color:red"
      )
    })

    # Plot - histogram
    output$histPValue <- plotly::renderPlotly({
      req(rv.custom$resAnaDiff)
      req(rv.custom$pi0)
      req(rv.widgets$Pvaluecalibration_nBinsHistpval)
      req(!rv.custom$containsNA)

      DaparToolshed::histPValue_HC(getPValueNonPushed(rv.custom$resAnaDiff$P_Value),
        bins = as.numeric(rv.widgets$Pvaluecalibration_nBinsHistpval),
        pi0 = rv.custom$pi0
      )
    })

    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl("Pvaluecalibration", btnEvents()))
      req(rv$dataIn)

      if (is.null(rv.widgets$Pvaluecalibration_calibrationMethod)) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("Running P-value calibration", id), {
          shiny::incProgress(0.5)

          # Get calibration method
          rv.custom$GetCalibrationMethod <- get_calibration_method(
            calibration_method = rv.widgets$Pvaluecalibration_calibrationMethod,
            numeric_value = rv.widgets$Pvaluecalibration_numericValCalibration
          )

          # Perform p-value calibration sub-step
          result <- pvalCalibrationProt(
            history = rv.custom$history,
            calibmet = rv.custom$GetCalibrationMethod,
            pi0 = rv.custom$calibrationRes$pi0,
            h1concent = rv.custom$calibrationRes$h1.concentration,
            unifunder = rv.custom$calibrationRes$unif.under
          )
          rv.custom$history <- result$history

          # Update widget and values
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


    ########################################################################### -
    #
    #----------------------------------FDR--------------------------------------
    #
    ########################################################################### -
    output$FDR <- renderUI({
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns("FDR_widgets_ui")),
          uiOutput(ns("showFDR_UI")),
          tags$hr(),
          uiOutput(ns("FDR_showHideDT_UI")),
          uiOutput(ns("FDR_viewAdjPval_UI"))
        ),
        content = tagList(
          div(
            style = "display: flex; gap: 20px;",
            uiOutput(ns("FDR_volcanoplot_UI")),
            div(
              uiOutput(ns("FDR_nbSelectedItems_ui")),
              uiOutput(ns("FDR_tooltipInfo_UI"))
            )
          ),
          downloadButton(ns("FDR_download_SelectedItems_UI"),
            "Selected final results (Excel file)",
            class = "btn-info"
          ),
          DT::DTOutput(ns("FDR_selectedItems_UI")),
          br()
        )
      )
    })

    #### _sidebar -----
    # Widget - set p-value (ui)
    output$FDR_widgets_ui <- renderUI({
      widget <- mod_set_pval_threshold_ui(ns("Title"))

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDR"])
    })

    # set p-value (server)
    logpval <- Prostar2::mod_set_pval_threshold_server(
      id = "Title",
      pval_init = reactive({
        10^(-rv.custom$thpval)
      }),
      remoteReset = reactive({
        remoteReset()
      }),
      is.enabled = reactive({
        rv$steps.enabled["FDR"]
      })
    )

    # Update FDR when p-value threshold changes
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

    # Txt FDR
    output$showFDR_UI <- renderUI({
      fdr <- Get_FDR()
      req(fdr)
      txt <- "FDR = NA"
      if (!is.infinite(fdr)) {
        txt <- paste0("FDR = ", round(100 * fdr, digits = 2), " %")
      }
      div(
        style = "margin: 15px 0px 15px 20px; font-size: 20px; font-weight: 900;",
        txt
      )
    })

    # Widget - show DT
    output$FDR_showHideDT_UI <- renderUI({
      widget <- checkboxInput(
        ns("FDR_showtable"),
        "Show table",
        value = FALSE
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDR"])
    })

    # Widget - view adjusted p-values
    output$FDR_viewAdjPval_UI <- renderUI({
      req(!is.null(rv.widgets$FDR_showtable))

      widget <- div(
        style = "margin-left: 10px; margin-top: -20px;",
        checkboxInput(ns("FDR_viewAdjPval"),
          span(
            style = "font-weight: 100 !important;",
            "View adjusted p-value"
          ),
          value = rv.widgets$FDR_viewAdjPval
        )
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDR"] && rv.widgets$FDR_showtable)
    })


    #### _content -----
    # Plot - volcano plot (server)
    Prostar2::mod_volcanoplot_server(
      id = "FDR_volcano",
      dataIn = reactive({
        rv.custom$DatasettoAnalyze
      }),
      comparison = reactive({
        c(rv.custom$Condition1, rv.custom$Condition2)
      }),
      group = reactive({
        DaparToolshed::design_qf(rv$dataIn)$Condition
      }),
      thlogfc = reactive({
        rv.custom$thlogfc
      }),
      thpval = reactive({
        rv.custom$thpval
      }),
      tooltip = reactive({
        rv.custom$FDR_tooltipInfo
      }),
      remoteReset = reactive({
        remoteReset()
      }),
      is.enabled = reactive({
        rv$steps.enabled["FDR"]
      })
    )

    # Plot - volcano plot (ui)
    output$FDR_volcanoplot_UI <- renderUI({
      widget <- mod_volcanoplot_ui(ns("FDR_volcano"))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDR"])
    })

    # Txt info selected proteins
    output$FDR_nbSelectedItems_ui <- renderUI({
      req(rv.custom$thpval)
      req(rv$dataIn)
      req(Pval_table())

      nbSelectedAnaDiff <- count_selected_prot(
        pval_table = Pval_table(),
        thpval = rv.custom$thpval,
        thlogfc = rv.custom$thlogfc
      )
      nbTotalAnaDiff <- nrow(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]]))

      # Getinfos
      A <- nbTotalAnaDiff
      B <- A - rv.custom$resAnaDiff$pushed
      C <- nbSelectedAnaDiff
      D <- (A - C)
      datatype <- DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]])

      # Make box
      tagList(
        div(
          class = "bloc_page",
          p(paste0("Total number of ", datatype, "(s) = ", A)),
          tags$em(p(
            style = "padding:0 0 0 20px;",
            paste0("Total remaining after push p-values = ", B)
          )),
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

    # Widget - tooltip selection
    output$FDR_tooltipInfo_UI <- renderUI({
      req(rv$dataIn)
      req(rv.widgets$Pairwisecomparison_Comparison != "None")
      req(rv.widgets$FDR_tooltipInfo)

      widget <- tagList(
        div(style = "margin-top: 25px;", ""),
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
          class = "btn-info"
        )
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["FDR"])
    })

    # Update tooltip selection
    observeEvent(input$FDR_validTooltipInfo, {
      rv.custom$FDR_tooltipInfo <- rv.widgets$FDR_tooltipInfo
    })

    # Table DT
    output$FDR_selectedItems_UI <- DT::renderDT({
      req(rv$steps.status["Pvaluecalibration"] == MagellanNTK::stepStatus$VALIDATED)
      req(rv.widgets$FDR_showtable)

      makeDTselectedProt(
        pval_table = Pval_table(),
        view_adj = rv.widgets$FDR_viewAdjPval,
        comparison = rv.widgets$Pairwisecomparison_Comparison
      )
    })

    # Button download DT
    output$FDR_download_SelectedItems_UI <- downloadHandler(
      filename = function() {
        paste0(
          "anaDiff_", rv.custom$Condition1, "_vs_",
          rv.custom$Condition2, ".xlsx"
        )
      },
      content = function(fname) {
        wb <- build_pairwise_comparison_workbook(
          pval_table = Pval_table(),
          comparison = rv.widgets$Pairwisecomparison_Comparison
        )
        openxlsx::saveWorkbook(wb, file = fname, overwrite = TRUE)
      }
    )

    # Compute FDR
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

    # Get number of significant proteins
    Get_Nb_Significant <- reactive({
      count_significant(
        Pval_table(),
        rv.widgets$Pairwisecomparison_Comparison
      )
    })

    # Create DT
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
      req(grepl("FDR", btnEvents()))
      req(rv$dataIn)

      if (is.null(rv$dataIn) || is.null(rv.custom$thpval)) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("Computing FDR", id), {
          shiny::incProgress(0.5)

          # Perform FDR sub-step
          result <- fdrProt(
            history = rv.custom$history,
            thpval = rv.custom$thpval,
            FDR = Get_FDR(),
            nbSignif = Get_Nb_Significant()
          )
          rv.custom$history <- result$history

          # DO NOT MODIFY THE NEXT THREE LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["FDR"] <- MagellanNTK::stepStatus$VALIDATED
          shiny::incProgress(1)
        })
      }
    })


    ########################################################################### -
    #
    #-------------------------------------SAVE----------------------------------
    #
    ########################################################################### -
    output$Save <- renderUI({
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(),
        content = tagList(
          uiOutput(ns("save_txt")),
          uiOutput(ns("dl_ui"))
        )
      )
    })

    #### _content -----
    # Save text (before saving)
    output$save_txt <- renderUI({
      req(rv$steps.status["Save"] != MagellanNTK::stepStatus$VALIDATED)
      req(config@mode == "process")

      save_txt_ui()
    })

    # Download (ui) (after saving)
    output$dl_ui <- renderUI({
      req(rv$steps.status["Save"] == MagellanNTK::stepStatus$VALIDATED)
      req(config@mode == "process")

      Prostar2::download_dataset_ui(ns(paste0(id, "_createQuickLink")))
    })

    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl("Save", btnEvents()))

      if (isTRUE(all.equal(SummarizedExperiment::assays(rv$dataIn), SummarizedExperiment::assays(dataIn())))) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("Saving process", id), {
          shiny::incProgress(0.5)
          # Do some stuff
          rv$dataIn <- prepareQFsave(
            data = rv$dataIn,
            history = rv.custom$history,
            namePipeline = "PipelineProtein"
          )

          # Add the result of pairwise comparison to the coldata
          DaparToolshed::DifferentialAnalysis(rv$dataIn[[length(rv$dataIn)]]) <- Pval_table()

          # DO NOT MODIFY THE NEXT THREE LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- rv$dataIn
          rv$steps.status["Save"] <- MagellanNTK::stepStatus$VALIDATED

          # Download (server)
          Prostar2::download_dataset_server(paste0(id, "_createQuickLink"), dataIn = reactive({
            dataOut$value
          }))
          shiny::incProgress(1)
        })
      }
    })

    ####### _END_ -----

    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  })
}
