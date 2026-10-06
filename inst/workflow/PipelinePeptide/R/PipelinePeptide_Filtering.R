#' @title PipelinePeptide Filtering module
#'
#' @description
#' This module contains the filtering step of the peptide pipeline.
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
#'   Prostar2("PipelinePeptide_Filtering")
#' }
#'
#' @name PipelinePeptide_Filtering
#'
#' @importFrom stats setNames rnorm
#' @importFrom shinyFeedback showFeedbackWarning hideFeedback
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#'
NULL


#' @rdname PipelinePeptide_Filtering
#' @export
#'
PipelinePeptide_Filtering_conf <- function() {
  MagellanNTK::Config(
    fullname = "PipelinePeptide_Filtering",
    mode = "process",
    steps = c("Cell metadata filtering", "Variable filtering"),
    mandatory = c(FALSE, FALSE)
  )
}


#' @rdname PipelinePeptide_Filtering
#' @export
#'
PipelinePeptide_Filtering_ui <- function(id) {
  ns <- NS(id)
}


#' @rdname PipelinePeptide_Filtering
#' @export
#'
PipelinePeptide_Filtering_server <- function(id,
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
                                             path = NULL,
                                             btnEvents = reactive({
                                               NULL
                                             })) {
  pkgs_require(c("QFeatures", "SummarizedExperiment", "S4Vectors"))

  # Default values for widgets
  widgets.default.values <- list(
    Variablefiltering_cname = "None",
    Variablefiltering_value = NA,
    Variablefiltering_keep_vs_remove = "delete",
    Variablefiltering_operator = "None"
  )

  # Default values for reactive values
  rv.custom.default.values <- list(
    history = MagellanNTK::InitializeHistory(),
    dataIn1 = NULL,
    dataIn2 = NULL,
    funFilter = reactive({
      NULL
    }),
    qMetacell_Filter_SummaryDT = data.frame(
      query = "-",
      nbDeleted = "0",
      TotalMainAssay = "0",
      stringsAsFactors = FALSE
    ),
    Variablefiltering_variable_Filter_SummaryDT = data.frame(
      query = "-",
      nbDeleted = "0",
      TotalMainAssay = "0",
      stringsAsFactors = FALSE
    ),
    wrongValueType = NULL
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

      # Store dataset in a variable for each sub-step
      rv.custom$dataIn1 <- rv$dataIn
      rv.custom$dataIn2 <- rv$dataIn

      # Update tables for each sub-step
      dtFilt <- data.frame(
        query = "-",
        nbDeleted = "0",
        TotalMainAssay = nrow(rv$dataIn[[length(rv$dataIn)]]),
        stringsAsFactors = FALSE
      )

      rv.custom$qMetacell_Filter_SummaryDT <- dtFilt
      rv.custom$Variablefiltering_variable_Filter_SummaryDT <- dtFilt

      # DO NOT MODIFY THE NEXT THREE LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status["Description"] <- MagellanNTK::stepStatus$VALIDATED
    })


    ########################################################################### -
    #
    #------------------------CELL METADATA FILTERING----------------------------
    #
    ########################################################################### -
    output$Cellmetadatafiltering <- renderUI({
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns("Cellmetadatafiltering_buildQuery_ui"))
        ),
        content = tagList(
          uiOutput(ns("qMetacell_Filter_DT_UI")),
          uiOutput(ns("Cellmetadatafiltering_qMetacell_Filter_DT")),
          uiOutput(ns("Cellmetadatafiltering_plots_ui"))
        )
      )
    })

    #### _sidebar -----
    # Widget - filter creation (server)
    observe({
      req(rv$steps.enabled["Cellmetadatafiltering"])
      req(rv.custom$dataIn1)
      rv.custom$funFilter <- mod_qMetacell_FunctionFilter_Generator_server(
        id = "query",
        dataIn = reactive({
          rv.custom$dataIn1[[length(rv.custom$dataIn1)]]
        }),
        conds = reactive({
          DaparToolshed::design_qf(rv.custom$dataIn1)$Condition
        }),
        keep_vs_remove = reactive({
          stats::setNames(c("Push p-value", "Keep original p-value"), nm = c("delete", "keep"))
        }),
        val_vs_percent = reactive({
          stats::setNames(nm = c("Count", "Percentage"))
        }),
        operator = reactive({
          stats::setNames(nm = DaparToolshed::SymFilteringOperators())
        }),
        remoteReset = reactive({
          remoteReset()
        }),
        is.enabled = reactive({
          rv$steps.enabled["Cellmetadatafiltering"]
        })
      )
    })

    # Widget - filter creation (ui)
    output$Cellmetadatafiltering_buildQuery_ui <- renderUI({
      widget <- mod_qMetacell_FunctionFilter_Generator_ui(ns("query"))

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Cellmetadatafiltering"])
    })

    #### _content -----
    # Apply filter
    observeEvent(req(length(rv.custom$funFilter()$value$ll.fun) > 0), ignoreInit = FALSE, {
      req(rv.custom$dataIn1)
      shiny::withProgress(message = paste0("Applying filter", id), {
        shiny::incProgress(0.5)

        result <- cellmetadataFiltering(
          data = rv.custom$dataIn1,
          filters = rv.custom$funFilter()$value$ll.fun,
          query = rv.custom$funFilter()$value$ll.query,
          history = rv.custom$history,
          original_length = length(dataIn())
        )

        # Update data
        rv.custom$dataIn1 <- result$data

        # Update table
        rv.custom$qMetacell_Filter_SummaryDT <- rbind(
          rv.custom$qMetacell_Filter_SummaryDT,
          result$summary
        )

        # Update history
        rv.custom$history <- result$history

        shiny::incProgress(1)
      })
    })

    # Plot - histograms
    output$Cellmetadatafiltering_plots_ui <- renderUI({
      req(rv.custom$funFilter()$value$ll.pattern)

      mod_ds_metacell_Histos_server(
        id = "plots",
        dataIn = reactive({
          rv.custom$dataIn1[[length(rv.custom$dataIn1)]]
        }),
        pattern = reactive({
          rv.custom$funFilter()$value$ll.pattern
        }),
        group = reactive({
          DaparToolshed::design_qf(rv.custom$dataIn1)$Condition
        }),
        pal = DaparToolshed::GetColorsForConditions(
          unique(DaparToolshed::design_qf(rv.custom$dataIn1)$Condition),
          DaparToolshed::ExtendPalette(length(unique(DaparToolshed::design_qf(rv.custom$dataIn1)$Condition)))
        )
      )

      widget <- mod_ds_metacell_Histos_ui(ns("plots"))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Cellmetadatafiltering"])
    })

    # Table (server)
    MagellanNTK::format_DT_server("dt",
      dataIn = reactive({
        rv.custom$qMetacell_Filter_SummaryDT
      })
    )

    # Table (ui)
    output$qMetacell_Filter_DT_UI <- renderUI({
      req(rv.custom$qMetacell_Filter_SummaryDT)
      MagellanNTK::format_DT_ui(ns("dt"))
    })

    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl("Cellmetadatafiltering", btnEvents()))
      req(rv.custom$dataIn1)

      if (isTRUE(all.equal(
        SummarizedExperiment::assays(rv.custom$dataIn1),
        SummarizedExperiment::assays(dataIn())
      )) ||
        !("Cellmetadatafiltering" %in% names(rv.custom$dataIn1))) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("Applying filter", id), {
          shiny::incProgress(0.5)

          # Update dataset for the next sub-step
          rv.custom$dataIn2 <- rv.custom$dataIn1

          # Update table for the next sub-step
          rv.custom$Variablefiltering_variable_Filter_SummaryDT <- data.frame(
            Variablefiltering_query = "-",
            Variablefiltering_nbDeleted = "0",
            Variablefiltering_TotalMainAssay = nrow(rv.custom$dataIn2[[length(rv.custom$dataIn2)]]),
            stringsAsFactors = FALSE
          )

          # DO NOT MODIFY THE NEXT THREE LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["Cellmetadatafiltering"] <- MagellanNTK::stepStatus$VALIDATED
          shiny::incProgress(1)
        })
      }
    })


    ########################################################################### -
    #
    #--------------------------VARIABLE FILTERING-------------------------------
    #
    ########################################################################### -
    output$Variablefiltering <- renderUI({
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          tags$style(HTML("
            .radio-inline {
              margin-right: 20px;
              margin-left: 10px;
              margin-bottom: -10px;
            }
          ")),
          uiOutput(ns("Variablefiltering_chooseKeepRemove_ui")),
          uiOutput(ns("Variablefiltering_cname_ui")),
          uiOutput(ns("Variablefiltering_operator_ui")),
          uiOutput(ns("Variablefiltering_value_ui")),
          uiOutput(ns("Variablefiltering_wrongValueType_ui")),
          uiOutput(ns("Variablefiltering_addFilter_btn_ui"))
        ),
        content = tagList(
          uiOutput(ns("Variablefiltering_DT_UI"))
        )
      )
    })

    #### _sidebar -----
    # Widget - keep delete
    output$Variablefiltering_chooseKeepRemove_ui <- renderUI({
      req(rv.custom$dataIn2)

      widget <- radioButtons(ns("Variablefiltering_keep_vs_remove"),
        "Type of filter operation",
        choices = rv.widgets$Variablefiltering_keep_vs_remove,
        selected = rv.widgets$Variablefiltering_keep_vs_remove
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Variablefiltering"])
    })

    # Widget - column name
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

    # Widget - operator
    output$Variablefiltering_operator_ui <- renderUI({
      req(rv.custom$dataIn2)
      req(rv.widgets$Variablefiltering_cname %in% colnames(SummarizedExperiment::rowData(rv.custom$dataIn2[[length(rv.custom$dataIn2)]])))

      # Change possibles operator depending whether the column is numeric or not
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

    # Widget - value
    output$Variablefiltering_value_ui <- renderUI({
      req(rv.custom$dataIn2)
      req(rv.widgets$Variablefiltering_cname %in% colnames(SummarizedExperiment::rowData(rv.custom$dataIn2[[length(rv.custom$dataIn2)]])))

      widget <- textInput(ns("Variablefiltering_value"),
        "Value",
        placeholder = "Enter value...",
        width = "175px"
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Variablefiltering"])
    })

    # Txt - numeric value expected
    output$Variablefiltering_wrongValueType_ui <- renderUI({
      req(rv.custom$wrongValueType)
      req(rv.widgets$Variablefiltering_value != "")
      p(
        style = "margin-top: -15px; font-weight: bold; color: red; font-size: 13px;",
        "/!\\ Numeric value expected"
      )
    })

    # Check wrongValueType when selected column or value changes
    observeEvent(c(rv.widgets$Variablefiltering_value, rv.widgets$Variablefiltering_cname), {
      req(rv.custom$dataIn2)
      req(!is.null(rv.widgets$Variablefiltering_value))
      req(rv.widgets$Variablefiltering_cname != "None")

      if (is.numeric(SummarizedExperiment::rowData(rv.custom$dataIn2[[length(rv.custom$dataIn2)]])[, rv.widgets$Variablefiltering_cname])) {
        rv.custom$wrongValueType <- is.na(Extract_Value(rv.widgets$Variablefiltering_value, "numeric"))
      } else {
        rv.custom$wrongValueType <- is.na(Extract_Value(rv.widgets$Variablefiltering_value, "character"))
      }
    })

    # Widget - add filter button
    output$Variablefiltering_addFilter_btn_ui <- renderUI({
      widget <- actionButton(ns("Variablefiltering_addFilter_btn"), "Add filter",
        class = "btn-info"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Variablefiltering"])
    })

    #### _content -----
    # Apply filter
    observeEvent(input$Variablefiltering_addFilter_btn,
      ignoreInit = FALSE,
      ignoreNULL = TRUE,
      {
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
          shiny::withProgress(message = paste0("Applying filter", id), {
            shiny::incProgress(0.5)

            result <- variableFiltering(
              data = rv.custom$dataIn2,
              widgets = reactiveValuesToList(rv.widgets),
              history = rv.custom$history,
              original_length = length(dataIn())
            )

            # Update data
            rv.custom$dataIn2 <- result$data

            # Update table
            rv.custom$Variablefiltering_variable_Filter_SummaryDT <- rbind(
              rv.custom$Variablefiltering_variable_Filter_SummaryDT,
              result$summary
            )

            # Update history
            rv.custom$history <- result$history

            shiny::incProgress(1)
          })
        }
      }
    )

    # Table (server)
    MagellanNTK::format_DT_server("Variablefiltering_dt",
      dataIn = reactive({
        rv.custom$Variablefiltering_variable_Filter_SummaryDT
      })
    )

    # Table (ui)
    output$Variablefiltering_DT_UI <- renderUI({
      req(rv.custom$Variablefiltering_variable_Filter_SummaryDT)
      MagellanNTK::format_DT_ui(ns("Variablefiltering_dt"))
    })

    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl("Variablefiltering", btnEvents()))
      req(rv.custom$dataIn2)

      if (isTRUE(all.equal(
        SummarizedExperiment::assays(rv.custom$dataIn2),
        SummarizedExperiment::assays(rv.custom$dataIn1)
      )) ||
        !("Variablefiltering" %in% names(rv.custom$dataIn2))) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("Applying filter", id), {
          shiny::incProgress(0.5)

          # DO NOT MODIFY
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["Variablefiltering"] <- MagellanNTK::stepStatus$VALIDATED
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
        SummarizedExperiment::assays(rv.custom$dataIn2),
        SummarizedExperiment::assays(dataIn())
      ))) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("Saving process", id), {
          shiny::incProgress(0.5)

          # Rename the new dataset and add the history
          rv.custom$dataIn2 <- prepareQFsave(
            data = rv.custom$dataIn2,
            history = rv.custom$history,
            namePipeline = "PipelinePeptide",
            SEname = "Filtering"
          )

          # DO NOT MODIFY THE NEXT THREE LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- rv.custom$dataIn2
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
