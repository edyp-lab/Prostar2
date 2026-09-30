#' @title PipelineProtein HypothesisTest module
#'
#' @description
#' This module contains the hypothesisTest step of the protein pipeline.
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
#'   Prostar2("PipelineProtein_HypothesisTest")
#' }
#'
#' @name PipelineProtein_HypothesisTest
#'
#' @importFrom stats setNames rnorm
#' @importFrom shinyjs useShinyjs
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#'
NULL


#' @rdname PipelineProtein_HypothesisTest
#' @export
#'
PipelineProtein_HypothesisTest_conf <- function() {
  MagellanNTK::Config(
    fullname = "PipelineProtein_HypothesisTest",
    mode = "process",
    steps = c("HypothesisTest"),
    mandatory = c(TRUE)
  )
}


#' @rdname PipelineProtein_HypothesisTest
#' @export
#'
PipelineProtein_HypothesisTest_ui <- function(id) {
  ns <- NS(id)
}


#' @rdname PipelineProtein_HypothesisTest
#' @export
#'
PipelineProtein_HypothesisTest_server <- function(
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
  requireNamespace("DaparToolshed")
  pkgs_require(c("QFeatures", "SummarizedExperiment", "S4Vectors"))

  # Default values for widgets
  widgets.default.values <- list(
    HypothesisTest_design = "None",
    HypothesisTest_method = "None",
    HypothesisTest_ttestOptions = "Student",
    HypothesisTest_thlogFC = 0
  )

  # Default values for reactive values
  rv.custom.default.values <- list(
    history = MagellanNTK::InitializeHistory(),
    logFC_onevsone = NULL,
    logFC_onevsall = NULL,
    containsNA = FALSE,
    enable_Limma = NULL,
    listNomsComparaison = NULL,
    n = NULL,
    swap.history = NULL,
    AllPairwiseComp = NULL,
    AllPairwiseCompMsg = NULL
  )

  ### -------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
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
        sidebar = tagList(
          uiOutput(ns("open_dataset_UI"))
        ),
        content = div(
          id = ns("div_content"),
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

      # Copy the input dataset to use it during this step
      rv$dataIn <- dataIn()

      # Check if there is missing values
      NApresent <- checkNA(rv$dataIn)

      if (NApresent) {
        warntxt <- "The dataset contains missing values.
        It must be first filtered or imputed."
        MagellanNTK::mod_errorModal_server("warn_NA",
          title = "Warning",
          text = warntxt
        )
      } else {
        shiny::withProgress(message = paste0("Initializing HypothesisTest", id), {
          shiny::incProgress(0.5)

          # Get logFC
          rv.custom$logFC_onevsone <- getlogFC(rv$dataIn, type = "OnevsOne")
          rv.custom$logFC_onevsall <- getlogFC(rv$dataIn, type = "OnevsAll")

          # Check if limma can be used
          rv.custom$enable_Limma <- checkLimma(rv$dataIn)

          # DO NOT MODIFY THE NEXT THREE LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["Description"] <- MagellanNTK::stepStatus$VALIDATED
        })
      }
    })


    ########################################################################### -
    #
    #----------------------------HYPOTHESIS TEST--------------------------------
    #
    ########################################################################### -
    output$HypothesisTest <- renderUI({
      shinyjs::useShinyjs()

      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns("HypothesisTest_design_ui")),
          uiOutput(ns("HypothesisTest_method_ui")),
          uiOutput(ns("HypothesisTest_ttestOptions_ui")),
          uiOutput(ns("HypothesisTest_thlogFC_ui")),
          uiOutput(ns("HypothesisTest_correspondingRatio_ui"))
        ),
        content = uiOutput(ns("HypothesisTest_plots_ui"))
      )
    })

    #### _sidebar -----
    # Widget - contrast
    output$HypothesisTest_design_ui <- renderUI({
      widget <- selectInput(ns("HypothesisTest_design"), "Contrast",
        choices = c(
          "None" = "None",
          "One vs One" = "OnevsOne",
          "One vs All" = "OnevsAll"
        ),
        selected = rv.widgets$HypothesisTest_design,
        width = "150px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["HypothesisTest"] &&
        !isTRUE(rv.custom$containsNA))
    })

    # Widget - test
    output$HypothesisTest_method_ui <- renderUI({
      .methods <- c("None" = "None", "t-tests" = "ttests")
      if (rv.custom$enable_Limma) {
        .methods <- c(.methods, "Limma" = "Limma")
      }

      widget <- selectInput(ns("HypothesisTest_method"), "Statistical test",
        choices = .methods,
        selected = rv.widgets$HypothesisTest_method,
        width = "150px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["HypothesisTest"] &&
        !isTRUE(rv.custom$containsNA))
    })

    # Widget - parameters for t-test
    output$HypothesisTest_ttestOptions_ui <- renderUI({
      req(rv.widgets$HypothesisTest_method == "ttests")

      widget <- radioButtons(ns("HypothesisTest_ttestOptions"),
        "t-tests options",
        choices = c("Student", "Welch"),
        selected = rv.widgets$HypothesisTest_ttestOptions,
        width = "150px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["HypothesisTest"] &&
        !isTRUE(rv.custom$containsNA))
    })

    # Widget - logFC value
    output$HypothesisTest_thlogFC_ui <- renderUI({
      widget <- shinyWidgets::autonumericInput(
        ns("HypothesisTest_thlogFC"),
        label = "log(FC) threshold",
        value = isolate(rv.widgets$HypothesisTest_thlogFC),
        width = "150px",
        minimumValue = 0,
        decimalCharacter = ".",
        decimalPlaces = 2,
        modifyValueOnWheel = TRUE,
        align = "left"
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["HypothesisTest"] &&
        !isTRUE(rv.custom$containsNA))
    })


    #### _content -----
    # Txt if limma cannot be applied
    output$HypothesisTest_warning_conditions_ui <- renderUI({
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

    # Plot - logFC density (ui)
    output$HypothesisTest_plots_ui <- renderUI({
      req(rv$dataIn)

      tagList(
        uiOutput(ns("HypothesisTest_warning_conditions_ui")),
        plotly::plotlyOutput(ns("FoldChangePlot")),
        div(
          style = "margin-left: 25px;",
          uiOutput(ns("HypothesisTest_swapConds_ui"))
        )
      )
    })

    # Plot - logFC density (server)
    output$FoldChangePlot <- plotly::renderPlotly({
      req(rv.custom$logFC)
      req(rv.widgets$HypothesisTest_thlogFC)
      logFC_th <- Extract_Value(rv.widgets$HypothesisTest_thlogFC, "numeric")
      req(!is.na(logFC_th))

      withProgress(message = "Computing plot...", detail = "", value = 0.5, {
        pal <- DaparToolshed::ExtendPalette(ncol(as.data.frame(rv.custom$logFC)), "Paired")

        DaparToolshed::hc_logFC_DensityPlot(
          df_logFC = as.data.frame(rv.custom$logFC),
          th_logFC = as.numeric(logFC_th),
          pal = pal
        )
      })
    })

    # Change which logFC to show on the plot
    observeEvent(req(rv.widgets$HypothesisTest_design != "None"), {
      req(rv$dataIn)
      # Get logFC
      if (rv.widgets$HypothesisTest_design == "OnevsOne") {
        rv.custom$logFC <- rv.custom$logFC_onevsone
      } else if (rv.widgets$HypothesisTest_design == "OnevsAll") {
        rv.custom$logFC <- rv.custom$logFC_onevsall
      }
      # Get comparison names
      rv.custom$listNomsComparaison <- colnames(rv.custom$logFC)
      rv.custom$listNomsComparaison <- unlist(strsplit(rv.custom$listNomsComparaison, split = "_logFC"))
      # Get number of comparison
      rv.custom$n <- ncol(rv.custom$logFC)
      rv.custom$swap.history <- rep(0, rv.custom$n)
    })

    # Widget - swap conditions
    output$HypothesisTest_swapConds_ui <- renderUI({
      req(rv.widgets$HypothesisTest_design != "None")
      req(rv.custom$listNomsComparaison)

      n <- length(rv.custom$listNomsComparaison)

      widget <- lapply(seq_len(n), function(i) {
        conds <- strsplit(rv.custom$listNomsComparaison[i], "_vs_", fixed = TRUE)[[1]]

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
        rv$steps.enabled["HypothesisTest"] && !isTRUE(rv.custom$containsNA)
      )
    })

    # When changes in swap conditions checkbox
    observeEvent(lapply(
      seq_along(rv.custom$listNomsComparaison),
      function(i) {
        input[[paste0("compswap_", i)]]
      }
    ), ignoreInit = TRUE, {
      n <- length(rv.custom$listNomsComparaison)

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
      req(grepl("HypothesisTest", btnEvents()))
      req(rv$dataIn)

      if (is.null(rv$dataIn) || rv.widgets$HypothesisTest_method == "None" || rv.widgets$HypothesisTest_design == "None" ||
        (rv.widgets$HypothesisTest_method == "ttests" && rv.widgets$HypothesisTest_ttestOptions == "None")) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = "Computing Hypothesis Test", {
          shiny::incProgress(0.5)

          result <- hypothesisTestProt(
            data = rv$dataIn,
            method = rv.widgets$HypothesisTest_method,
            history = rv.custom$history,
            logFC_thr = rv.widgets$HypothesisTest_thlogFC,
            design = rv.widgets$HypothesisTest_design,
            ttest_type = rv.widgets$HypothesisTest_ttestOptions
          )
          rv.custom$AllPairwiseComp <- result$AllPairwiseComp
          rv.custom$history <- result$history
          rv.custom$AllPairwiseCompMsg <- result$message

          if (is.null(rv.custom$AllPairwiseComp)) {
            MagellanNTK::mod_SweetAlert_server(
              id = "sweetalert_PerformLogFCPlot",
              text = rv.custom$AllPairwiseCompMsg,
              type = "error"
            )
          } else if (inherits(rv.custom$AllPairwiseComp, "try-error")) {
            MagellanNTK::mod_SweetAlert_server(
              id = "sweetalert_PerformLogFCPlot",
              text = rv.custom$AllPairwiseComp[[1]],
              type = "error"
            )
          } else {
            req(rv.custom$AllPairwiseComp$P_Value)
            req(rv.custom$AllPairwiseComp$logFC)

            new.dataset <- rv$dataIn[[length(rv$dataIn)]]
            df <- cbind(
              rv.custom$AllPairwiseComp$logFC,
              rv.custom$AllPairwiseComp$P_Value
            )
            DaparToolshed::HypothesisTest(new.dataset) <- as.data.frame(df)

            rv$dataIn <- QFeatures::addAssay(rv$dataIn, new.dataset, "HypothesisTest")

            # DO NOT MODIFY THE NEXT THREE LINES
            dataOut$trigger <- MagellanNTK::Timestamp()
            dataOut$value <- NULL
            rv$steps.status["HypothesisTest"] <- MagellanNTK::stepStatus$VALIDATED
          }
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
        shiny::withProgress(message = paste0("Reseting process", id), {
          shiny::incProgress(0.5)

          rv$dataIn <- prepareQFsave(
            data = rv$dataIn,
            history = rv.custom$history,
            namePipeline = "PipelineProtein"
          )

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
