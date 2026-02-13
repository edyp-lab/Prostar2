#' @title Filtering Shiny module
#'
#' @description
#' This function is a shiny module to xxx
#' This function is written with specifications of the package `MagellanNTK` so
#' as to be easily integrated into workflfow compliant with `MagellanNTK`.
#'
#' @name mod_Pept_Imputation
#' 
#' @param id xxx
#' @param dataIn An instance of the class `QFeatures`
#' @param i xx
#' @param remoteReset A `logical(1)` which acts as a remote command to reset
#' the module to its default values. Default is FALSE.
#' @param is.enabled A `logical(1)` that indicates whether the module is
#' enabled or disabled. This is a remote command.
#'
#'
#' @return As for all modules used with `MagellanNTK`, the return value is a
#' `list()` of two items:
#' - trigger : xxx
#' - value: In this case, it contains a list() of three slots:
#'   - ll.var: a list() of instances of the class `Filtering`,
#'   - ll.query: a list of `character()` which describe the queries in natural
#'   language,
#'   - ll.widgets.value: a list of the values of widgets.
#'
#' @examples
#' if (interactive()){
#' data(Exp1_R25_pept, package = "DaparToolshedData")
#' obj <- Exp1_R25_pept[seq_len(100)]
#' shiny::runApp(mod_Pept_Imputation(obj, 1))
#' }
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut format_DT_server Timestamp toggleWidget
#'
NULL





#' @export
#'
#' @rdname mod_Pept_Imputation
#'
mod_Pept_Imputation_ui <- function(id) {
  ns <- NS(id)

  wellPanel(
    # uiOutput for all widgets in this UI
    # This part is mandatory
    # The renderUlength(rv$dataIn) function of each widget is managed by MagellanNTK
    # The dev only have to define a reactive() function for each
    # widget he want to insert
    # Be aware of the naming convention for ids in uiOutput()
    # For more details, please refer to the dev document.
    uiOutput(ns("Imp_UI")),
    uiOutput(ns("mvplots_ui"))
  )
}





#' @rdname mod_Pept_Imputation
#'
#' @export
#' 
#' @importFrom DaparToolshed typeDataset
#' @importFrom SummarizedExperiment rowData assay
#'
mod_Pept_Imputation_server <- function(
    id,
    dataIn = reactive({
      NULL
    }),
    i = reactive({
      NULL
    }),
    remoteReset = reactive({
      0
    }),
    is.enabled = reactive({
      TRUE
    })) {
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list(
    Imp_algorithm = NULL,
    Imp_imp4p_withLapala = FALSE,
    Imp_imp4p_nbiter = 10,
    Imp_imp4p_qmin = 0,
    Imp_KNN_n = 10,
    Imp_detQuant_quantile = 2.5,
    Imp_detQuant_factor = 1
  )

  rv.custom.default.values <- list()


  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # .localStyle <- "display:inline-block; vertical-align: top; padding-right: 20px;"
    # eval(
    #   str2expression(
    #     MagellanNTK::Get_AdditionalModule_Core_Code(
    #       w.names = names(widgets.default.values),
    #       rv.custom.names = names(rv.custom.default.values)
    #     )
    #   )
    # )

    core <- paste0(
      MagellanNTK::Get_Code_Declare_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_ObserveEvent_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_rv_reactiveValues(),
      MagellanNTK::Get_Code_Declare_rv_custom(names(rv.custom.default.values)),
      MagellanNTK::Get_Code_for_dataOut(),
      MagellanNTK::Get_Code_for_remoteReset(
        widgets = TRUE,
        custom = TRUE,
        dataIn = "dataIn()"
      ),
      sep = "\n"
    )

    eval(str2expression(core))


    # observeEvent(req(remoteReset()), ignoreInit = FALSE, {
    #   rv$dataIn <- dataIn()
    #   req(rv$dataIn)
    #
    # })


    observeEvent(dataIn(), ignoreNULL = TRUE, ignoreInit = FALSE, {
        req(dataIn())
        req(inherits(dataIn(), "QFeatures"))
        rv$dataIn <- dataIn()
      },
      priority = 1000
    )



    output$mvplots_ui <- renderUI({
      widget <- mod_mv_plots_ui(ns("mvplots"))
      MagellanNTK::toggleWidget(widget, is.enabled())
    })


    observe({
      req(rv$dataIn)

      mod_mv_plots_server("mvplots",
        data = reactive({
          rv$dataIn[[length(rv$dataIn)]]
        }),
        grp = reactive({
          get_group(rv$dataIn)
        }),
        mytitle = reactive({
          "POV imputation"
        }),
        pal = reactive({
          NULL
        }),
        pattern = reactive({
          c("Missing", "Missing POV", "Missing MEC")
        })
      )
    })



    output$Imp_UI <- renderUI({
      # Checks if
      .data <- SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])
      nbEmptyLines <- getNumberOfEmptyLines(.data)
      if (nbEmptyLines > 0) {
        tags$p("Your dataset contains empty lines (fully filled with missing
    values). In order to use the imputation tool, you must delete them by
      using the filter tool.")
      } else if (sum(is.na(.data)) == 0) {
        tags$p("Your dataset does not contains missing values.")
      } else {
        tagList(
          uiOutput(ns("Imp_warning")),
          uiOutput(ns("Imp_algorithm_UI")),
          uiOutput(ns("Imp_imp4p_UI")),
          uiOutput(ns("Imp_imp4pOpts2_UI")),
          uiOutput(ns("Imp_KNN_nbNeighbors_UI")),
          uiOutput(ns("Imp_detQuant_UI")),
          uiOutput(ns("Imp_MLE_UI")),
          uiOutput(ns("Imp_showDetQuantValues_UI")),
          # Insert validation button
          uiOutput(ns("mod_Pept_Imputation_btn_validate_ui")),
          htmlOutput("helpForImputation")
        )
      }
    })


    output$Imp_warning <- renderText({
      req(rv.widgets$Imp_algorithm != "None")

      txtForBasics <- "<font color='red'>Please note that none of these
         'Basic methods' impute the MEC (Missing on the Entire Condition) and
         aggregation of peptides won't be possible if MEC data aren't imputed.
          <br>To do so, please choose the 'imp4p' algorithm as imputation
    method and check 'Impute MEC also' option.</font color='red'>"

      t <- switch(rv.widgets$Imp_algorithm,
        imp4p = {
          if (isFALSE(rv.widgets$Imp_imp4p_withLapala)) {
            "<font color='red'>Please note that aggregation of peptides
              won't be possible if MEC (Missing on the Entire Condition)
              data aren't imputed. To do so, check 'Impute MEC also' option.
           </font color='red'>"
          } else {
            "<font color='red'><strong>Warning:</strong> Imputed MEC
               (Missing on the Entire Condition)
                values must be very cautiously interpreted <br>[see the
           User manual, Section 6.3.1].</font color='red'>"
          }
        },
        KNN = txtForBasics,
        detQuantile = txtForBasics,
        MLE = txtForBasics
      )

      HTML(t)
    })


    output$Imp_algorithm_UI <- renderUI({
      widget <- selectInput(ns("Imp_algorithm"), "Method",
        choices = list(
          "None" = "None",
          "imp4p" = "imp4p",
          "Det quantile" = "detQuantile",
          "KNN" = "KNN",
          "MLE" = "MLE"
        ),
        selected = rv.widgets$Imp_algorithm,
        width = "150px"
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })


    output$showDetQuantValues <- renderUI({
      req(rv.widgets$Imp_algorithm == "detQuantile")

      mod_DetQuantImpValues_server(
        id = "DetQuantValues_DT",
        dataIn = reactive({
          rv$dataIn[[length(rv$dataIn)]]
        }),
        quant = reactive({
          rv.widgets$Imp_detQuant_quantile
        }),
        factor = reactive({
          rv.widgets$Imp_detQuant_factor
        })
      )

      tagList(
        h5("The missing values will be imputed by the following values :"),
        mod_DetQuantImpValues_ui(ns("DetQuantValues_DT"))
      )
    })



    output$Imp_KNN_nbNeighbors_UI <- renderUI({
      req(rv.widgets$Imp_algorithm == "KNN")

      widget <- numericInput(ns("Imp_KNN_nbNeighbors"), "Neighbors",
        value = rv.widgets$Imp_KNN_n, step = 1, min = 0,
        max = max(nrow(rv$dataIn), rv.widgets$Imp_KNN_n),
        width = "100px"
      )

      MagellanNTK::toggleWidget(widget, is.enabled())
    })


    output$Imp_detQuant_UI <- renderUI({
      req(rv.widgets$Imp_algorithm == "detQuantile")

      widget <- tagList(
        tags$div(
          numericInput(ns("detQuant_quantile"), "Quantile",
            value = rv.widgets$Imp_detQuant_quantile,
            step = 0.5, min = 0, max = 100, width = "100px"
          )
        ),
        tags$div(
          numericInput(ns("detQuant_factor"), "Factor",
            value = rv.widgets$Imp_detQuant_factor,
            step = 0.1, min = 0, max = 10, width = "100px"
          )
        )
      )

      MagellanNTK::toggleWidget(widget, is.enabled())
    })


    output$Imp_imp4p_UI <- renderUI({
      req(rv.widgets$Imp_algorithm == "imp4p")

      widget <- tagList(
        tags$div(
          numericInput(ns("Imp_imp4p_nbiter"), "Iterations",
            value = rv.widgets$Imp_imp4p_nbiter,
            step = 1, min = 1, width = "100px"
          )
        ),
        tags$div(
          checkboxInput(ns("Imp_imp4p_withLapala"), "Impute MEC also",
            value = rv.widgets$Imp_imp4p_withLapala
          )
        )
      )

      MagellanNTK::toggleWidget(widget, is.enabled())
    })


    output$Imp_imp4pOpts2UI <- renderUI({
      req(rv.widgets$Imp_imp4p_withLapala)
      G_imp4PDistributionType_Choices <- c(
        "uniform" = "unif",
        "beta" = "beta"
      )

      widget <- tagList(
        tags$div(
          numericInput(ns("Imp_imp4p_qmin"), "Upper lapala bound",
            value = rv.widgets$Imp_imp4p_qmin,
            step = 0.1, min = 0, max = 100,
            width = "100px"
          )
        ),
        tags$div(
          radioButtons(ns("Imp_imp4pLAPALA_distrib"),
            "Distribution type",
            choices = G_imp4PDistributionType_Choices,
            selected = rv.widgets$Imp_imp4pLAPALA_distrib
          )
        )
      )
    })

    output$mod_Pept_Imputation_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("mod_Pept_Imputation_btn_validate"),
        "Validate step",
        class = "btn-success"
      )

      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    # >>> END: Definition of the widgets


    observeEvent(input$mod_Pept_Imputation_btn_validate, {
      req(rv$dataIn)
      req(rv.widgets$Imp_algorithm != "None")

      m <- DaparToolshed::match.metacell(
        qMetacell(rv$dataIn[[length(rv$dataIn)]]),
        pattern = c("Missing", "Missing POV", "Missing MEC"),
        level = DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]])
      )
      nbPOVBefore <- length(which(m))
      withProgress(message = "", detail = "", value = 0, {
        incProgress(0.25, detail = "Find MEC blocks")

        .tmp <- NULL
        .param <- list()

        try({
          switch(rv.widgets$Imp_algorithm,
            None = .tmp <- rv$dataIn[[length(rv$dataIn)]],
            imp4p = {
              incProgress(0.5, detail = "Imp4p imputation")
              if (rv.widgets$Imp_imp4p_withLapala) {
                .tmp <- wrapper.dapar.impute.mi(
                  obj = rv$dataIn[[length(rv$dataIn)]],
                  design = DaparToolshed::design.qf(rv$dataIn),
                  nb.iter = rv.widgets$Imp_imp4p_nbiter,
                  lapala = rv.widgets$Imp_imp4p_withLapala,
                  q.min = rv.widgets$Imp_imp4p_qmin / 100,
                  distribution = as.character(rv.widgets$Imp_imp4pLAPALA_distrib)
                )
                .param <- list(
                  algorithm = rv.widgets$Imp_algorithm,
                  nb.iter = rv.widgets$Imp_imp4p_nbiter,
                  lapala = rv.widgets$Imp_imp4p_withLapala,
                  q.min = rv.widgets$Imp_imp4p_qmin / 100,
                  distribution = as.character(rv.widgets$Imp_imp4pLAPALA_distrib)
                )
              } else {
                .tmp <- wrapper.dapar.impute.mi(
                  obj = rv$dataIn[[length(rv$dataIn)]],
                  design = DaparToolshed::design.qf(rv$dataIn),
                  nb.iter = rv.widgets$Imp_imp4p_nbiter,
                  lapala = rv.widgets$Imp_imp4p_withLapala
                )
                .param <- list(
                  algorithm = rv.widgets$Imp_algorithm,
                  nb.iter = rv.widgets$Imp_imp4p_nbiter,
                  lapala = rv.widgets$Imp_imp4p_withLapala
                )
              }
            },
            detQuantile = {
              incProgress(0.5, detail = "det quantile Imputation")
              .tmp <- wrapper.impute.detQuant(
                obj = rv$dataIn[[length(rv$dataIn)]],
                qval = rv.widgets$Imp_detQuant_quantile / 100,
                factor = rv.widgets$Imp_detQuant_factor,
                na.type = "Missing POV"
              )
              .param <- list(
                algorithm = rv.widgets$Imp_algorithm,
                qval = rv.widgets$Imp_detQuant_quantile / 100,
                factor = rv.widgets$Imp_detQuant_factor,
                na.type = "Missing POV"
              )
            },
            KNN = {
              incProgress(0.5, detail = "KNN Imputation")

              .tmp <- wrapper.impute.KNN(
                obj = rv$dataIn[[length(rv$dataIn)]],
                grp = DaparToolshed::design.qf(rv$dataIn)$Condition,
                K = rv.widgets$Imp_KNN_n
              )
              .param <- list(
                algorithm = rv.widgets$Imp_algorithm,
                K = rv.widgets$Imp_KNN_n
              )
            },
            MLE = {
              incProgress(0.5, detail = "MLE Imputation")

              .tmp <- wrapper.impute.mle(
                obj = rv$dataIn[[length(rv$dataIn)]],
                grp = DaparToolshed::design.qf(rv$dataIn)$Condition
              )
              .param <- list(
                algorithm = rv.widgets$Imp_algorithm
              )
            },
            None = {}
          )
        })

        if (inherits(.tmp, "try-error") || inherits(.tmp, "try-warning")) {
          mod_SweetAlert_server(
            id = "sweetalert_perform_POVimputation_button",
            text = .tmp,
            type = "error"
          )
        } else {
          # sendSweetAlert(
          #   session = session,
          #   title = "Success",
          #   type = "success"
          # )
          # rv$dataIn[[length(rv$dataIn)]] <- .tmp
          # incProgress(0.75, detail = 'Reintroduce MEC blocks')
          incProgress(1, detail = "Finalize imputation")


          m <- DaparToolshed::match.metacell(qMetacell(.tmp),
            pattern = "Missing POV",
            level = DaparToolshed::typeDataset(.tmp)
          )
          nbPOVAfter <- length(which(m))
          rv$nbPOVimputed <- nbPOVBefore - nbPOVAfter
        }

        rv$dataIn <- Prostar2::addDatasets(
          rv$dataIn,
          .tmp,
          "Imputation"
        )

        DaparToolshed::paramshistory(rv$dataIn[[length(rv$dataIn)]]) <- .param

        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- rv$dataIn
      })
    })

    return(reactive({
      dataOut
    }))
  })
}




#' @export
#' @rdname mod_Pept_Imputation
#'
mod_Pept_Imputation <- function(dataIn, i) {
  ui <- mod_Pept_Imputation_ui("pov")

  server <- function(input, output, session) {
    res <- mod_Pept_Imputation_server("pov",
      dataIn = reactive({
        dataIn
      }),
      i = reactive({
        i
      })
    )

    observeEvent(res()$trigger, {
      print(res()$value)
    })
  }

  app <- shiny::shinyApp(ui, server)
}
