#' @title Filtering Shiny module
#'
#' @description
#' This function is a shiny module to xxx
#' This function is written with specifications of the package `MagellanNTK` so
#' as to be easily integrated into workflfow compliant with `MagellanNTK`.
#'
#' @name mod_Prot_Normalization
#' 
#' @param id xxx
#' @param dataIn An instance of the class `QFeatures`
#' @param i xxx
#' @param remoteReset A `integer(1)` xxxx
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
#' data(Exp1_R25_prot, package = "DaparToolshedData")
#' obj <- Exp1_R25_prot[seq_len(100)]
#' mod_Prot_Normalization(obj, 1)
#' }
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut format_DT_server Timestamp toggleWidget
#'
NULL





#' @export
#'
#' @rdname mod_Prot_Normalization
#' 
#' @importFrom shinyjs toggle hidden
#'
mod_Prot_Normalization_ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    # uiOutput for all widgets in this UI
    # This part is mandatory
    # The renderUlength(rv$dataIn) function of each widget is managed by MagellanNTK
    # The dev only have to define a reactive() function for each
    # widget he want to insert
    # Be aware of the naming convention for ids in uiOutput()
    # For more details, please refer to the dev document.
    tagList(
      uiOutput(ns("Normalization_btn_validate_ui")),
      uiOutput(ns("Normalization_method_ui")),
      div(
        id = "div_Normalization_type_ui",
        shinyjs::hidden(uiOutput(ns("Normalization_type_ui")))
      ),
      div(
        shinyjs::hidden(uiOutput(ns("Normalization_spanLOESS_ui"))),
        uiOutput(ns("Normalization_quantile_ui")),
        uiOutput(ns("Normalization_varReduction_ui"))
      ),
      div(
        uiOutput(ns("tracking")),
        shinyjs::hidden(uiOutput(ns("Normalization_sync_ui")))
      ),
      tags$hr(),
      fluidRow(
        column(
          width = 5,
          omXplore::omXplore_density_ui(ns("densityPlot_Norm"))
        ),
        column(
          width = 5,
          omXplore::omXplore_intensity_ui(ns("boxPlot_Norm"))
        ),
        column(
          width = 5,
          highcharter::highchartOutput(ns("viewComparisonNorm_hc"))
        )
      )
    )
  )
}





#' @rdname mod_Prot_Normalization
#' 
#' @importFrom stats setNames
#' @importFrom S4Vectors metadata
#' @importFrom SummarizedExperiment rowData assay
#' @importFrom shinyjs toggle hidden
#' @importFrom DaparToolshed normalizeMethods idcol compareNormalizationD_HC GlobalQuantileAlignment QuantileCentering SumByColumns LOESS vsn paramshistory
#'
#' @export
#'
mod_Prot_Normalization_server <- function(
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
    Normalization_method = "None",
    Normalization_type = "overall",
    Normalization_spanLOESS = 0.7,
    Normalization_quantile = 0.15,
    Normalization_varReduction = FALSE,
    Normalization_sync = FALSE
  )

  rv.custom.default.values <- list(
    tmp.dataset = NULL,
    # init.dataset = NULL,
    history = NULL,
    selectProt = reactive({
      NULL
    })
  )


  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    .localStyle <- "display:inline-block; vertical-align: top; padding-right: 20px;"

    pkgs.require('omXplore')
    #
    # core.code <- MagellanNTK::Get_Workflow_Core_Code(
    #   mode = 'process',
    #   name = id,
    #   w.names = names(widgets.default.values),
    #   rv.custom.names = names(rv.custom.default.values)
    # )
    #
    # eval(str2expression(core.code))

    core <- paste0(
      Get_Code_Declare_widgets(names(widgets.default.values)),
      Get_Code_for_ObserveEvent_widgets(names(widgets.default.values)),
      Get_Code_for_rv_reactiveValues(),
      Get_Code_Declare_rv_custom(names(rv.custom.default.values)),
      Get_Code_for_dataOut(),
      Get_Code_for_remoteReset(
        widgets = TRUE,
        custom = TRUE,
        dataIn = "NULL"
      ),
      sep = "\n"
    )

    eval(str2expression(core))


    observeEvent(dataIn(), ignoreNULL = TRUE, ignoreInit = FALSE, {
        # req(dataIn())
        req(inherits(dataIn(), "QFeatures"))
        rv$dataIn <- dataIn()

        # browser()
      },
      priority = 1000
    )


    selectProt <- omXplore::plots_tracking_server(
      id = "tracker",
      dataIn = reactive({
        rv$dataIn[[length(rv$dataIn)]]
      }),
      remoteReset = reactive({
        remoteReset()
      })
    )

    omXplore::omXplore_intensity_server("boxPlot_Norm",
      dataIn = reactive({
        rv$dataIn
      }),
      i = reactive({
        length(rv$dataIn)
      }),
      track.indices = reactive({
        selectProt()$indices
      }),
      remoteReset = reactive({
        remoteReset()
      }),
      is.enabled = reactive({
        is.enabled()
      })
    )

    omXplore::omXplore_density_server("densityPlot_Norm",
      dataIn = reactive({
        rv$dataIn
      }),
      i = reactive({
        length(rv$dataIn)
      })
    )


    # observeEvent(selectProt()$indices, ignoreNULL = FALSE,{
    #   print("in observeEvent(selectProt()$indices")
    #   print('new value fo selectProt()$indices = ')
    #   print(selectProt()$indices)
    # })
    #
    # >>> START: Definition of the widgets

    # This part must be customized by the developer of a new module
    output$Normalization_method_ui <- renderUI({
      widget <- selectInput(
        ns("Normalization_method"),
        "Normalization method",
        choices = setNames(nm = c("None", DaparToolshed::normalizeMethods())),
        selected = rv.widgets$Normalization_method,
        width = "250px"
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })


    output$Normalization_type_ui <- renderUI({
      widget <- selectInput(ns("Normalization_type"),
        "Normalization type",
        choices = stats::setNames(
          nm = c("overall", "within conditions")
        ),
        selected = rv.widgets$Normalization_type,
        width = "150px"
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })


    output$Normalization_spanLOESS_ui <- renderUI({
      widget <- textInput(
        ns("Normalization_spanLOESS"),
        "Span",
        value = rv.widgets$Normalization_spanLOESS,
        width = "100px"
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })


    output$Normalization_quantile_ui <- renderUI({
      req(rv.widgets$Normalization_method == "QuantileCentering")
      widget <- textInput(
        ns("Normalization_quantile"),
        MagellanNTK::mod_popover_for_help_ui(ns("quantile_help")),
        value = rv.widgets$Normalization_quantile,
        width = "100px"
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })


    output$Normalization_varReduction_ui <- renderUI({
      req(rv.widgets$Normalization_method == "MeanCentering")
      widget <- checkboxInput(
        ns("Normalization_varReduction"),
        "Include variance reduction",
        value = rv.widgets$Normalization_varReduction
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })


    output$Normalization_sync_ui <- renderUI({
      widget <- checkboxInput(
        ns("Normalization_sync"),
        "Synchronise with selection above",
        value = rv.widgets$Normalization_sync
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })


    output$tracking <- renderUI({
      req(rv.widgets$Normalization_method %in% c("QuantileCentering", "MeanCentering", "SumByColumns"))
      widget <- omXplore::plots_tracking_ui(ns("tracker"))
      MagellanNTK::toggleWidget(widget, is.enabled())
    })




    output$viewComparisonNorm_hc <- highcharter::renderHighchart({
      req(rv$dataIn)
      req(length(rv$dataIn) > 1)
      withProgress(message = " Build plot", {
        incProgress(0.5)
        
        obj1 <- rv$dataIn[[length(rv$dataIn)]]
      obj2 <- rv$dataIn[[length(rv$dataIn) - 1]]

      req(obj1)
      req(obj2)
      protId <- DaparToolshed::idcol(rv$dataIn[[length(rv$dataIn)]])

      if (!is.null(selectProt()$indices)) {
        .n <- length(selectProt()$indices)
        .subset <- selectProt()$indices
      } else {
        .n <- floor(0.02 * nrow(obj1))
        .subset <- seq(nrow(obj1))
      }



      DaparToolshed::compareNormalizationD_HC(
        qDataBefore = SummarizedExperiment::assay(rv$dataIn, length(rv$dataIn)),
        qDataAfter = SummarizedExperiment::assay(rv$dataIn, length(rv$dataIn) - 1),
        keyId = rowData(rv$dataIn[[length(rv$dataIn)]])[, protId],
        conds = design.qf(rv$dataIn)$Condition,
        pal = NULL,
        # Consider only 2% of the entire dataset
        n = .n,
        subset.view = .subset
      )
      
      })
    })




    observeEvent(rv.widgets$Normalization_method, {
      req(rv.widgets$Normalization_method)
      req(rv$dataIn)
      shinyjs::toggle("Normalization_btn_validate",
        condition = rv.widgets$Normalization_method != "None"
      )

      shinyjs::toggle("spanLOESS",
        condition = rv.widgets$Normalization_method == "LOESS"
      )

      .choice <- c(
        "QuantileCentering", "MeanCentering", "SumByColumns",
        "LOESS", "vsn"
      )

      shinyjs::toggle("Normalization_type_ui",
        condition = (rv.widgets$Normalization_method %in% .choice)
      )

      cond <- S4Vectors::metadata(rv$dataIn[[length(rv$dataIn)]])[["typeDataset"]] == "protein"

      .meths <- DaparToolshed::normalizeMethods("withTracking")
      trackAvailable <- rv.widgets$Normalization_method %in% .meths
      shinyjs::toggle("Normalization_sync_ui",
        condition = cond && trackAvailable
      )

      shinyjs::toggle("Normalization_sync_ui",
        condition = cond && trackAvailable
      )
    })

    MagellanNTK::mod_popover_for_help_server(
      id = "quantile_help",
      title = "Normalization quantile",
      content = "lower limit/noise (quantile = 0.15),
            median (quantile = 0.5). Min value=0, max value=1"
    )

    output$Normalization_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Normalization_btn_validate"),
        "Run Normalization",
        class = "btn-success"
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    # >>> END: Definition of the widgets

    observeEvent(input$Normalization_btn_validate, {
      # Do some stuff
      req(rv.widgets$Normalization_method)
      req(rv$dataIn)

      withProgress(message = "Normalizing", {
        incProgress(0.5)
      rv.custom$tmpAssay <- NULL
      try({
        .conds <- SummarizedExperiment::colData(rv$dataIn)[, "Condition"]
        qdata <- SummarizedExperiment::assay(rv$dataIn, length(rv$dataIn))


        switch(rv.widgets$Normalization_method,
          G_noneStr = {
            rv.custom$tmpAssay <- rv$dataIn[[length(rv$dataIn)]]
          },
          GlobalQuantileAlignment = {
            rv.custom$tmpAssay <- DaparToolshed::GlobalQuantileAlignment(qdata)
            rv.custom$history[["Normalization_method"]] <- rv.widgets$Normalization_method
          },
          QuantileCentering = {
            quant <- NA
            if (!is.null(rv.widgets$Normalization_quantile)) {
              quant <- as.numeric(rv.widgets$Normalization_quantile)
            }

            rv.custom$tmpAssay <- DaparToolshed::QuantileCentering(
              qData = qdata,
              conds = .conds,
              type = rv.widgets$Normalization_type,
              subset.norm = selectProt()$indices,
              quantile = quant
            )

            rv.custom$history[["Normalization_method"]] <- rv.widgets$Normalization_method
            rv.custom$history[["Normalization_quantile"]] <- quant
            rv.custom$history[["Normalization_type"]] <- rv.widgets$Normalization_type
            rv.custom$history[["subset.norm"]] <- selectProt()$indices
          },
          MeanCentering = {
            rv.custom$tmpAssay <- DaparToolshed::MeanCentering(
              qData = qdata,
              conds = .conds,
              type = rv.widgets$Normalization_type,
              scaling = rv.widgets$Normalization_varReduction,
              subset.norm = selectProt()$indices
            )

            rv.custom$history[["Normalization_method"]] <- rv.widgets$Normalization_method
            rv.custom$history[["Normalization_varReduction"]] <- rv.widgets$Normalization_varReduction
            rv.custom$history[["Normalization_type"]] <- rv.widgets$Normalization_type
            rv.custom$history[["subset.norm"]] <- selectProt()$indices
          },
          SumByColumns = {
            rv.custom$tmpAssay <- DaparToolshed::SumByColumns(
              qData = qdata,
              conds = .conds,
              type = rv.widgets$Normalization_type,
              subset.norm = selectProt()$indices
            )

            rv.custom$history[["Normalization_method"]] <- rv.widgets$Normalization_method
            rv.custom$history[["Normalization_type"]] <- rv.widgets$Normalization_type
            rv.custom$history[["subset.norm"]] <- selectProt()$indices
          },
          LOESS = {
            rv.custom$tmpAssay <- DaparToolshed::LOESS(
              qData = qdata,
              conds = .conds,
              type = rv.widgets$Normalization_type,
              span = as.numeric(rv.widgets$Normalization_spanLOESS)
            )

            rv.custom$history[["Normalization_method"]] <- rv.widgets$Normalization_method
            rv.custom$history[["Normalization_type"]] <- rv.widgets$Normalization_type
            rv.custom$history[["Normalization_spanLOESS"]] <- as.numeric(rv.widgets$Normalization_spanLOESS)
          },
          vsn = {
            rv.custom$tmpAssay <- DaparToolshed::vsn(
              qData = qdata,
              conds = .conds,
              type = rv.widgets$Normalization_type
            )

            rv.custom$history[["Normalization_method"]] <- rv.widgets$Normalization_method
            rv.custom$history[["Normalization_type"]] <- rv.widgets$Normalization_type
          }
        )
      })



      if (inherits(rv.custom$tmpAssay, "try-error") || is.null(rv.custom$tmpAssay)) {
        MagellanNTK::mod_SweetAlert_server(
          id = "sweetalert_perform_normalization",
          text = rv.custom$tmpAssay[[1]],
          type = "error"
        )

        # DO NOT MODIFY THE THREE FOLLOWING LINES
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- rv$dataIn
      } else {
        new.dataset <- rv$dataIn[[length(rv$dataIn)]]
        SummarizedExperiment::assay(new.dataset) <- rv.custom$tmpAssay
        DaparToolshed::paramshistory(new.dataset) <- NULL
        DaparToolshed::paramshistory(new.dataset) <- rv.custom$history
        rv$dataIn <- addAssay(rv$dataIn, new.dataset, "Normalization")
      }

      # DO NOT MODIFY THE THREE FOLLOWING LINES
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
#' @rdname mod_Prot_Normalization
#'
mod_Prot_Normalization <- function(dataIn, i) {
  ui <- fluidPage(
    actionButton("Reset", "Reset"),
    mod_Prot_Normalization_ui("pov")
  )

  server <- function(input, output, session) {
    rv.custom <- reactiveValues(
      tmp.norm = reactive({
        NULL
      })
    )

    rv.custom$tmp.norm <- mod_Prot_Normalization_server("pov",
      dataIn = reactive({
        dataIn
      }),
      i = reactive({
        i
      }),
      is.enabled = reactive({
        TRUE
      }),
      remoteReset = reactive({
        input$Reset
      })
    )

    observeEvent(rv.custom$tmp.norm()$trigger, ignoreInit = TRUE, {
      print("Result of process:")
      print(rv.custom$tmp.norm()$value)
    })
  }

  shiny::shinyApp(ui, server)
}
