#' @title PipelineProtein Imputation module
#'
#' @description
#' This module contains the imputation step of the protein pipeline.
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
#'   Prostar2("PipelineProtein_Imputation")
#' }
#'
#' @name PipelineProtein_Imputation
#'
#' @importFrom stats setNames rnorm
#' @importFrom shinyjs useShinyjs
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#'
NULL


#' @rdname PipelineProtein_Imputation
#' @export
#'
PipelineProtein_Imputation_conf <- function() {
  MagellanNTK::Config(
    fullname = "PipelineProtein_Imputation",
    mode = "process",
    steps = c("POV Imputation", "MEC Imputation"),
    mandatory = c(FALSE, FALSE)
  )
}


#' @rdname PipelineProtein_Imputation
#' @export
#'
PipelineProtein_Imputation_ui <- function(id) {
  ns <- NS(id)
}


#' @rdname PipelineProtein_Imputation
#' @export
#'
PipelineProtein_Imputation_server <- function(
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

  # Default values for widgets
  widgets.default.values <- list(
    POVImputation_algorithm = NULL,
    POVImputation_KNN_n = 10,
    POVImputation_detQuant_quantile = 2.5,
    POVImputation_detQuant_factor = 1,
    MECImputation_algorithm = NULL,
    MECImputation_KNN_n = 10,
    MECImputation_detQuant_quantile = 2.5,
    MECImputation_detQuant_factor = 1,
    MECImputation_fixedValue = 0
  )

  # Default values for reactive values
  rv.custom.default.values <- list(
    history = MagellanNTK::InitializeHistory(),
    dataIn1 = NULL,
    dataIn2 = NULL,
    POVImputation_SummaryDT = data.frame(
      Operation = "-",
      nbImputed = "0",
      TotalMissingValues = "0",
      stringsAsFactors = FALSE
    ),
    MECImputation_SummaryDT = data.frame(
      Operation = "-",
      nbImputed = "0",
      TotalMissingValues = "0",
      stringsAsFactors = FALSE
    )
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
      req(inherits(dataIn(), 'QFeatures'))

      # Copy the input dataset to use it during this step
      rv$dataIn <- dataIn()

      # Store dataset in a variable for each sub-step
      rv.custom$dataIn1 <- rv$dataIn
      rv.custom$dataIn2 <- rv$dataIn

      # Update tables for each sub-step
      dtImput <- data.frame(
        Operation = "-",
        nbImputed = "0",
        TotalMissingValues = QFeatures::nNA(rv$dataIn[[length(rv$dataIn)]])$nNA[, "nNA"],
        stringsAsFactors = FALSE
      )

      rv.custom$POVImputation_SummaryDT <- dtImput
      rv.custom$MECImputation_SummaryDT <- dtImput

      # DO NOT MODIFY THE NEXT THREE LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status["Description"] <- MagellanNTK::stepStatus$VALIDATED
    })


    ########################################################################### -
    #
    #----------------------------POV IMPUTATION---------------------------------
    #
    ########################################################################### -
    output$POVImputation <- renderUI({
      shinyjs::useShinyjs()

      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns("POVImputation_algorithm_UI")),
          uiOutput(ns("POVImputation_KNN_nbNeighbors_UI")),
          uiOutput(ns("POVImputation_detQuant_UI"))
        ),
        content = div(
          tags$style(HTML(".mv-container img {margin: 0 !important;}")),
          uiOutput(ns("POVImputation_DT_UI")),
          uiOutput(ns("POVImputation_showDetQuantValues")),
          div(
            class = "mv-container", style = "display: flex; margin-top: 20px;",
            uiOutput(ns("mvplots_ui"))
          )
        )
      )
    })

    #### _sidebar -----
    # Widget - type of imputation to perform
    output$POVImputation_algorithm_UI <- renderUI({
      widget <- selectInput(ns("POVImputation_algorithm"),
        "Algorithm for POV",
        choices = c("None" = "None", GetPOVimputMet()),
        selected = rv.widgets$POVImputation_algorithm,
        width = "150px",
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["POVImputation"])
    })

    # Widget - detQuant parameters
    output$POVImputation_detQuant_UI <- renderUI({
      req(rv.widgets$POVImputation_algorithm == "detQuantile")

      widget <- div(
        style = "display: flex; gap: 10px;",
        shinyWidgets::autonumericInput(
          ns("POVImputation_detQuant_quantile"),
          label = "Quantile",
          value = isolate(rv.widgets$POVImputation_detQuant_quantile),
          width = "100px",
          minimumValue = 0,
          maximumValue = 100,
          decimalCharacter = ".",
          currencySymbol = " %",
          decimalPlaces = 1,
          modifyValueOnWheel = TRUE,
          currencySymbolPlacement = "s",
          align = "left"
        ),
        shinyWidgets::autonumericInput(
          ns("POVImputation_detQuant_factor"),
          label = "Factor",
          value = isolate(rv.widgets$POVImputation_detQuant_factor),
          width = "100px",
          minimumValue = 0,
          maximumValue = 10,
          decimalCharacter = ".",
          decimalPlaces = 1,
          modifyValueOnWheel = TRUE,
          align = "left"
        )
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["POVImputation"])
    })

    # Widget - KNN parameters
    output$POVImputation_KNN_nbNeighbors_UI <- renderUI({
      req(rv.widgets$POVImputation_algorithm == "KNN")

      widget <- shinyWidgets::autonumericInput(
        ns("POVImputation_KNN_nbNeighbors"),
        label = "Neighbors",
        value = isolate(rv.widgets$POVImputation_KNN_n),
        width = "100px",
        minimumValue = 1,
        maximumValue = max(nrow(rv.custom$dataIn1), widgets.default.values$POVImputation_KNN_n),
        decimalCharacter = ".",
        decimalPlaces = 0,
        modifyValueOnWheel = TRUE,
        align = "left"
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["POVImputation"])
    })

    #### _content -----
    # Table (server)
    MagellanNTK::format_DT_server("POV_dt",
      dataIn = reactive({
        rv.custom$POVImputation_SummaryDT
      })
    )

    # Table (ui)
    output$POVImputation_DT_UI <- renderUI({
      req(rv.custom$POVImputation_SummaryDT)
      MagellanNTK::format_DT_ui(ns("POV_dt"))
    })

    # Plot - NA plots (ui)
    output$mvplots_ui <- renderUI({
      widget <- mod_mv_plots_ui(ns("POVImputation_mvplots"))

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["POVImputation"])
    })

    # Plot - NA plots (server)
    observe({
      req(rv.custom$dataIn1)

      pal <- DaparToolshed::GetColorsForConditions(
        unique(DaparToolshed::design_qf(rv.custom$dataIn1)$Condition),
        DaparToolshed::ExtendPalette(length(unique(DaparToolshed::design_qf(rv.custom$dataIn1)$Condition)))
      )

      mod_mv_plots_server("POVImputation_mvplots",
        data = reactive({
          rv.custom$dataIn1[[length(rv.custom$dataIn1)]]
        }),
        grp = reactive({
          omXplore::get_group(rv.custom$dataIn1)
        }),
        mytitle = reactive({
          "POV imputation"
        }),
        pal = pal,
        pattern = reactive({
          c("Missing", "Missing POV", "Missing MEC")
        })
      )
    })

    # Table value detQuant
    output$POVImputation_showDetQuantValues <- renderUI({
      req(rv.widgets$POVImputation_algorithm == "detQuantile")

      mod_DetQuantImpValues_server(
        id = "POVImputation_DetQuantValues_DT",
        dataIn = reactive({
          rv.custom$dataIn1[[length(rv.custom$dataIn1)]]
        }),
        quant = reactive({
          rv.widgets$POVImputation_detQuant_quantile
        }),
        factor = reactive({
          rv.widgets$POVImputation_detQuant_factor
        })
      )

      mod_DetQuantImpValues_ui(ns("POVImputation_DetQuantValues_DT"))
    })

    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl("POVImputation", btnEvents()))
      req(rv.custom$dataIn1)

      if (is.null(rv.custom$dataIn1) ||
        rv.widgets$POVImputation_algorithm == "None") {
        shinyjs::info(btnVentsMasg)
      } else {
        withProgress(message = "", detail = "", value = 0, {
          incProgress(0.5, detail = "Imputing POV")

          nbPOVBefore <- countPattern(
            data = rv.custom$dataIn1[[length(rv.custom$dataIn1)]],
            pattern = "Missing POV"
          )

          # Perform imputation
          imp <- imputationProtPOV(
            data = rv.custom$dataIn1,
            method = rv.widgets$POVImputation_algorithm,
            history = rv.custom$history,
            quantile = rv.widgets$POVImputation_detQuant_quantile,
            factor = rv.widgets$POVImputation_detQuant_factor,
            n = rv.widgets$POVImputation_KNN_n
          )

          # Update values
          .tmp <- imp$data
          rv.custom$history <- imp$history

          if (inherits(.tmp, "try-error") || inherits(.tmp, "try-warning")) {
            MagellanNTK::mod_SweetAlert_server(
              id = "sweetalert_perform_POVimputation_button",
              text = .tmp,
              type = "error"
            )
          } else {
            incProgress(1, detail = "Finalize POV imputation")

            nbPOVAfter <- countPattern(
              data = .tmp,
              pattern = "Missing POV"
            )
            nbPOVimputed <- nbPOVBefore - nbPOVAfter

            rv.custom$dataIn1 <- Prostar2::addDatasets(
              rv.custom$dataIn1,
              .tmp,
              "POVImputation"
            )

            # Add infos
            # nBefore <- QFeatures::nNA(rv.custom$dataIn1[[length(rv.custom$dataIn1) - 1]])$nNA[, "nNA"]
            nAfter <- QFeatures::nNA(rv.custom$dataIn1[[length(rv.custom$dataIn1)]])$nNA[, "nNA"]

            rv.custom$POVImputation_SummaryDT <- rbind(
              rv.custom$POVImputation_SummaryDT,
              c("POV Imputation", nbPOVimputed, nAfter)
            )

            rv.custom$dataIn2 <- rv.custom$dataIn1

            rv.custom$MECImputation_SummaryDT <- rv.custom$POVImputation_SummaryDT

            # DO NOT MODIFY THE NEXT THREE LINES
            dataOut$trigger <- MagellanNTK::Timestamp()
            dataOut$value <- NULL
            rv$steps.status["POVImputation"] <- MagellanNTK::stepStatus$VALIDATED
          }
        })
      }
    })


    ########################################################################### -
    #
    #----------------------------MEC IMPUTATION---------------------------------
    #
    ########################################################################### -
    output$MECImputation <- renderUI({
      shinyjs::useShinyjs()

      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns("MECImputation_chooseImputationMethod_ui")),
          uiOutput(ns("MECImputation_Params_ui"))
        ),
        content = tagList(
          tags$style(HTML(".mv-container img {margin: 0 !important;}")),
          uiOutput(ns("MECImputation_DT_UI")),
          uiOutput(ns("warningMECImputation")),
          uiOutput(ns("MECImputation_showDetQuantValues_ui")),
          tags$hr(),
          withProgress(message = "", detail = "", value = 0, {
            incProgress(0.5, detail = "Building plots...")
            uiOutput(ns("MECImputation_mvplots_ui"))
          })
        )
      )
    })

    #### _sidebar -----
    # Widget - type of imputation to perform
    output$MECImputation_chooseImputationMethod_ui <- renderUI({
      req(checkNA(rv.custom$dataIn2))

      widget <- selectInput(ns("MECImputation_algorithm"), "Algorithm for MEC",
        choices = c("None" = "None", GetMECimputMet()),
        selected = rv.widgets$MECImputation_algorithm,
        width = "150px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["MECImputation"])
    })

    # Widget - parameters
    output$MECImputation_Params_ui <- renderUI({
      req(checkNA(rv.custom$dataIn2))
      req(rv.widgets$MECImputation_algorithm != "None")

      widget <- switch(rv.widgets$MECImputation_algorithm,
        detQuantile = {
          widget <- div(
            style = "display: flex; gap: 10px;",
            shinyWidgets::autonumericInput(
              ns("MECImputation_detQuant_quantile"),
              label = "Quantile",
              value = isolate(rv.widgets$MECImputation_detQuant_quantile),
              width = "100px",
              minimumValue = 0,
              maximumValue = 100,
              decimalCharacter = ".",
              currencySymbol = " %",
              decimalPlaces = 1,
              modifyValueOnWheel = TRUE,
              currencySymbolPlacement = "s",
              align = "left"
            ),
            shinyWidgets::autonumericInput(
              ns("MECImputation_detQuant_factor"),
              label = "Factor",
              value = isolate(rv.widgets$MECImputation_detQuant_factor),
              width = "100px",
              minimumValue = 0,
              maximumValue = 10,
              decimalCharacter = ".",
              decimalPlaces = 1,
              modifyValueOnWheel = TRUE,
              align = "left"
            )
          )
        },
        fixedValue = {
          shinyWidgets::autonumericInput(
            ns("MECImputation_fixedValue"),
            label = "Factor",
            value = isolate(rv.widgets$MECImputation_fixedValue),
            width = "100px",
            minimumValue = 0,
            maximumValue = 100,
            decimalCharacter = ".",
            decimalPlaces = 1,
            modifyValueOnWheel = TRUE,
            align = "left"
          )
        }
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["MECImputation"])
    })

    #### _content -----
    # Table (server)
    MagellanNTK::format_DT_server("MEC_dt",
      dataIn = reactive({
        rv.custom$MECImputation_SummaryDT
      })
    )

    # Table (ui)
    output$MECImputation_DT_UI <- renderUI({
      req(rv.custom$MECImputation_SummaryDT)
      MagellanNTK::format_DT_ui(ns("MEC_dt"))
    })

    # Plot - NA plots (ui)
    output$MECImputation_mvplots_ui <- renderUI({
      widget <- mod_mv_plots_ui(ns("MECImputation_mvplots"))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["MECImputation"])
    })

    # Plot - NA plots (server)
    observe({
      req(rv.custom$dataIn2)

      pal <- DaparToolshed::GetColorsForConditions(
        unique(DaparToolshed::design_qf(rv.custom$dataIn2)$Condition),
        DaparToolshed::ExtendPalette(length(unique(DaparToolshed::design_qf(rv.custom$dataIn2)$Condition)))
      )

      mod_mv_plots_server("MECImputation_mvplots",
        data = reactive({
          rv.custom$dataIn2[[length(rv.custom$dataIn2)]]
        }),
        grp = reactive({
          omXplore::get_group(rv.custom$dataIn2)
        }),
        mytitle = reactive({
          "MEC imputation"
        }),
        pal = pal,
        pattern = reactive({
          c("Missing", "Missing POV", "Missing MEC")
        })
      )
    })

    output$MECImputation_showDetQuantValues_ui <- renderUI({
      req(rv.widgets$MECImputation_algorithm == "detQuantile")

      mod_DetQuantImpValues_server(
        id = "MECImputation_DetQuantValues_DT",
        dataIn = reactive({
          rv.custom$dataIn2[[length(rv.custom$dataIn2)]]
        }),
        quant = reactive({
          rv.widgets$MECImputation_detQuant_quantile
        }),
        factor = reactive({
          rv.widgets$MECImputation_detQuant_factor
        })
      )

      tagList(
        # h5("The MEC will be imputed by the following values :"),
        mod_DetQuantImpValues_ui(ns("MECImputation_DetQuantValues_DT"))
      )
    })

    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl("MECImputation", btnEvents()))
      req(rv.custom$dataIn2)

      if (is.null(rv.custom$dataIn2) ||
        rv.widgets$MECImputation_algorithm == "None") {
        shinyjs::info(btnVentsMasg)
      } else {
        withProgress(message = "", detail = "", value = 0, {
          incProgress(0.5, detail = "Imputing MEC")

          nbMECBefore <- countPattern(
            data = rv.custom$dataIn2[[length(rv.custom$dataIn2)]],
            pattern = "Missing MEC"
          )

          # Perform imputation
          imp <- imputationProtMEC(
            data = rv.custom$dataIn2,
            method = rv.widgets$MECImputation_algorithm,
            history = rv.custom$history,
            quantile = rv.widgets$MECImputation_detQuant_quantile,
            factor = rv.widgets$MECImputation_detQuant_factor,
            fixVal = rv.widgets$MECImputation_fixedValue
          )

          # Update values
          .tmp <- imp$data
          rv.custom$history <- imp$history

          if (inherits(.tmp, "try-error")) {
            MagellanNTK::mod_SweetAlert_server(
              id = "sweetalert_perform_MECimputation_button",
              text = .tmp,
              type = "error"
            )
          } else {
            incProgress(1, detail = "Finalize MEC imputation")

            nbMECAfter <- countPattern(
              data = .tmp,
              pattern = "Missing MEC"
            )
            nbMECimputed <- nbMECBefore - nbMECAfter

            rv.custom$dataIn2 <- Prostar2::addDatasets(
              rv.custom$dataIn2,
              .tmp,
              "MECImputation"
            )

            # Add infos
            # nBefore <- QFeatures::nNA(rv.custom$dataIn2[[length(rv.custom$dataIn2) - 1]])$nNA[, "nNA"]
            nAfter <- QFeatures::nNA(rv.custom$dataIn2[[length(rv.custom$dataIn2)]])$nNA[, "nNA"]

            rv.custom$MECImputation_SummaryDT <- rbind(
              rv.custom$MECImputation_SummaryDT,
              c(
                "MEC Imputation",
                nbMECimputed,
                nAfter
              )
            )

            # DO NOT MODIFY THE NEXT THREE LINES
            dataOut$trigger <- MagellanNTK::Timestamp()
            dataOut$value <- NULL
            rv$steps.status["MECImputation"] <- MagellanNTK::stepStatus$VALIDATED
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

      if (isTRUE(all.equal(
        SummarizedExperiment::assays(rv$dataIn),
        SummarizedExperiment::assays(rv.custom$dataIn2)
      ))) {
        shinyjs::info(btnVentsMasg)
        
      } else {
        shiny::withProgress(message = paste0("Saving process", id), {
          shiny::incProgress(0.5)
          len_start <- length(rv$dataIn)
          len_end <- length(rv.custom$dataIn2)
          len_diff <- len_end - len_start

          req(len_diff > 0)

          if (len_diff == 2) {
            rv.custom$dataIn2 <- QFeatures::removeAssay(
              rv.custom$dataIn2,
              length(rv.custom$dataIn2) - 1
            )
          }

          # Rename the new dataset and add the history
          rv.custom$dataIn2 <- prepareQFsave(
            data = rv.custom$dataIn2,
            history = rv.custom$history,
            namePipeline = "PipelineProtein",
            SEname = "Imputation"
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
