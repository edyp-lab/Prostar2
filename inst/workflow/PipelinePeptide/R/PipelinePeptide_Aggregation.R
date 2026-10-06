#' @title PipelinePeptide Aggregation module
#'
#' @description
#' This module contains the aggregation step of the peptide pipeline.
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
#'   Prostar2("PipelinePeptide_Aggregation")
#' }
#'
#' @name PipelinePeptide_Aggregation
#'
#' @importFrom stats setNames rnorm
#' @import omXplore
#' @importFrom shinyjs hidden useShinyjs toggle
#' @importFrom shinyFeedback showFeedbackWarning hideFeedback
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#'
NULL


#' @rdname PipelinePeptide_Aggregation
#' @export
#'
PipelinePeptide_Aggregation_conf <- function() {
  MagellanNTK::Config(
    fullname = "PipelinePeptide_Aggregation",
    mode = "process",
    steps = c("Aggregation"),
    mandatory = c(TRUE)
  )
}


#' @rdname PipelinePeptide_Aggregation
#' @export
#'
PipelinePeptide_Aggregation_ui <- function(id) {
  ns <- NS(id)
}


#' @rdname PipelinePeptide_Aggregation
#' @export
#'
PipelinePeptide_Aggregation_server <- function(
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
  pkgs_require(c("QFeatures", "SummarizedExperiment", "S4Vectors"))
  requireNamespace("DaparToolshed")

  # Default values for widgets
  widgets.default.values <- list(
    Aggregation_includeSharedPeptides = "Yes_Iterative_Redistribution",
    Aggregation_ponderation = "Global",
    Aggregation_operator = "Mean",
    Aggregation_considerPeptides = "allPeptides",
    Aggregation_proteinId = "None",
    Aggregation_topN = 3,
    Aggregation_addRowData = NULL,
    Addmetadata_columnsForProteinDataset = NULL,
    Aggregation_maxiter = 500
  )

  # Default values for reactive values
  rv.custom.default.values <- list(
    history = MagellanNTK::InitializeHistory(),
    nbEmptyLines = NULL,
    aggregationStatsPept = NULL,
    aggregationStatsProt = NULL
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
      req(inherits(dataIn(), "QFeatures"))

      # Copy the input dataset to use it during this step
      rv$dataIn <- dataIn()

      # Make stats tables
      tableStat <- aggregStatTables(rv$dataIn)

      rv.custom$aggregationStatsPept <- tableStat$pept
      rv.custom$aggregationStatsProt <- tableStat$prot


      # DO NOT MODIFY THE NEXT THREE LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status["Description"] <- MagellanNTK::stepStatus$VALIDATED
    })


    ########################################################################### -
    #
    #-----------------------------AGGREGATION-----------------------------------
    #
    ########################################################################### -
    output$Aggregation <- renderUI({
      shinyjs::useShinyjs()

      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns("Aggregation_chooseProteinId_ui")),
          uiOutput(ns("Aggregation_includeSharedPeptides_ui")),
          # tags$hr(),
          uiOutput(ns("Aggregation_considerPeptides_ui")),
          # tags$hr(),
          uiOutput(ns("Aggregation_operator_ui")),
          # tags$hr(),
          uiOutput(ns("Aggregation_addRowData_ui"))
        ),
        content = tagList(
          uiOutput(ns("Aggregation_emptyrow_ui")),
          uiOutput(ns("Aggregation_warning_ui")),
          uiOutput(ns("Aggregation_AggregationDone_ui")),
          uiOutput(ns("Aggregation_aggregationStats_ui"))
        )
      )
    })

    #### _sidebar -----
    # Widget - shared peptide and parameters for redistribution
    output$Aggregation_includeSharedPeptides_ui <- renderUI({
      widget <- radioButtons(ns("Aggregation_includeSharedPeptides"),
        "Include shared peptides",
        choices = c(
          "No" = "No",
          "Yes (as protein specific)" = "Yes_As_Specific",
          "Yes (simple redistribution)" = "Yes_Simple_Redistribution",
          "Yes (iterative redistribution)" = "Yes_Iterative_Redistribution"
        ),
        selected = rv.widgets$Aggregation_includeSharedPeptides
      )
      widget2 <- selectInput(ns("Aggregation_ponderation"),
        "Ponderation",
        choices = c(
          "Global" = "Global",
          "Condition" = "Condition",
          "Sample" = "Sample"
        ),
        selected = rv.widgets$Aggregation_ponderation,
        width = "130px"
      )
      widget3 <- numericInput(ns("Aggregation_maxiter"),
        "Max iteration",
        value = rv.widgets$Aggregation_maxiter,
        min = 1,
        step = 1,
        width = "95px"
      )


      tagList(
        MagellanNTK::toggleWidget(widget, rv$steps.enabled["Aggregation"]),
        if (rv.widgets$Aggregation_includeSharedPeptides == "Yes_Simple_Redistribution") {
          MagellanNTK::toggleWidget(widget2, rv$steps.enabled["Aggregation"])
        } else if (rv.widgets$Aggregation_includeSharedPeptides == "Yes_Iterative_Redistribution") {
          tagList(div(
            style = "display: flex; gap: 10px;",
            MagellanNTK::toggleWidget(widget2, rv$steps.enabled["Aggregation"]),
            MagellanNTK::toggleWidget(widget3, rv$steps.enabled["Aggregation"])
          ))
        }
      )
    })

    # Widget - which peptides to consider
    output$Aggregation_considerPeptides_ui <- renderUI({
      widget <- selectInput(ns("Aggregation_considerPeptides"),
        "Consider",
        choices = c(
          "All peptides" = "allPeptides",
          "N most abundant" = "topN"
        ),
        selected = rv.widgets$Aggregation_considerPeptides,
        width = "155px"
      )
      widget2 <- numericInput(ns("Aggregation_topN"),
        " N",
        value = rv.widgets$Aggregation_topN,
        min = 0,
        step = 1,
        width = "70px"
      )

      tagList(
        div(
          style = "display: flex; gap: 10px;",
          MagellanNTK::toggleWidget(widget, rv$steps.enabled["Aggregation"]),
          if (rv.widgets$Aggregation_considerPeptides == "topN") {
            MagellanNTK::toggleWidget(widget2, rv$steps.enabled["Aggregation"])
          }
        )
      )
    })

    # Widget - aggregation function
    output$Aggregation_operator_ui <- renderUI({
      widget <- selectInput(ns("Aggregation_operator"),
        "Function",
        choices = c(
          "Sum" = "Sum",
          "Mean" = "Mean",
          "Median" = "Median",
          "medianPolish" = "medianPolish",
          "robustSummary" = "robustSummary"
        ),
        selected = rv.widgets$Aggregation_operator,
        width = "235px"
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Aggregation"])
    })

    # Widget - rowData columns to aggregate
    output$Aggregation_addRowData_ui <- renderUI({
      req(rv$dataIn)

      widget <- selectInput(ns("Aggregation_addRowData"),
        "RowData columns to aggregate",
        colnames(SummarizedExperiment::rowData(DaparToolshed::last_assay(rv$dataIn))),
        selected = rv.widgets$Aggregation_addRowData,
        multiple = TRUE,
        width = "200px"
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Aggregation"])
    })

    # Widget - choose the protein id no previously selected in data
    output$Aggregation_chooseProteinId_ui <- renderUI({
      req(rv$dataIn)
      req(DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]]) != "protein")
      req(is.null(DaparToolshed::parentProtId(rv$dataIn[[length(rv$dataIn)]])))

      .choices <- colnames(SummarizedExperiment::rowData(DaparToolshed::last_assay(rv$dataIn)))
      widget <- selectInput(ns("Aggregation_proteinId"),
        "Choose the protein ID",
        choices = c("None", .choices),
        selected = rv.widgets$Aggregation_proteinId
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Aggregation"])
    })

    #### _content -----
    # Txt warning if NA
    output$Aggregation_warning_ui <- renderUI({
      req(rv$dataIn)
      req(DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]]) != "protein")

      containsNA <- checkNA(rv$dataIn)

      if (containsNA) {
        tags$p(
          style = "color: red;",
          tags$b("Warning:"), " Your dataset contains missing values.
        For better results, you should impute them first"
        )
      }
    })

    # Txt warning if empty lines
    output$Aggregation_emptyrow_ui <- renderUI({
      req(rv$dataIn)
      req(DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]]) != "protein")

      .data <- SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])
      rv.custom$nbEmptyLines <- DaparToolshed::getNumberOfEmptyLines(.data)
      if (rv.custom$nbEmptyLines > 0) {
        tags$p(
          style = "color: red;",
          tags$b("Warning:"), "Your dataset contains empty lines (fully filled with missing values).
               Please remove them using the filtering step."
        )
      }
    })

    # Table info pept + prot (ui)
    output$Aggregation_aggregationStats_ui <- renderUI({
      tagList(
        MagellanNTK::format_DT_ui(ns("dtaggregationStatsPept")),
        MagellanNTK::format_DT_ui(ns("dtaggregationStatsProt"))
      )
    })

    # Table info pept (server)
    MagellanNTK::format_DT_server(
      "dtaggregationStatsPept",
      reactive({
        rv.custom$aggregationStatsPept
      })
    )

    # Table info prot (server)
    MagellanNTK::format_DT_server(
      "dtaggregationStatsProt",
      reactive({
        rv.custom$aggregationStatsProt
      })
    )

    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl("Aggregation", btnEvents()))

      if (is.null(rv$dataIn) ||
        rv.custom$nbEmptyLines != 0) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = "", detail = "", value = 0, {
          incProgress(0.5, detail = "Aggregation process")

          # Perform aggregation
          result <- aggregationPept(
            data = rv$dataIn,
            history = rv.custom$history,
            sharePept = rv.widgets$Aggregation_includeSharedPeptides,
            operator = rv.widgets$Aggregation_operator,
            considerPept = rv.widgets$Aggregation_considerPeptides,
            ponderation = rv.widgets$Aggregation_ponderation,
            n = rv.widgets$Aggregation_topN,
            aggCol = rv.widgets$Aggregation_addRowData,
            maxIter = rv.widgets$Aggregation_maxiter,
            rmEmptyLines = TRUE # can be made as widget instead if needed
          )

          # Update values
          .tmp <- result$data
          rv.custom$history <- result$history

          if (!is.null(S4Vectors::metadata(.tmp)$aggQmetacell_issues) &&
            length(S4Vectors::metadata(.tmp)$aggQmetacell_issues) > 0) {
            MagellanNTK::mod_SweetAlert_server(
              id = "sweetalert_perform_aggregation",
              text = "The aggregation process did not succeed because some sets of peptides contains missing values and quantitative values at the same time.",
              type = "error"
            )
          } else {
            rv$dataIn <- .tmp

            # DO NOT MODIFY THE NEXT THREE LINES
            dataOut$trigger <- MagellanNTK::Timestamp()
            dataOut$value <- NULL
            rv$steps.status["Aggregation"] <- MagellanNTK::stepStatus$VALIDATED
          }
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

      if (isTRUE(all.equal(
        SummarizedExperiment::assays(dataIn()),
        SummarizedExperiment::assays(rv$dataIn)
      ))) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("Saving process", id), {
          shiny::incProgress(0.5)

          # Rename the new dataset and add the history
          rv$dataIn <- prepareQFsave(
            data = rv$dataIn,
            history = rv.custom$history,
            namePipeline = "PipelinePeptide",
            SEname = "Aggregation"
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
