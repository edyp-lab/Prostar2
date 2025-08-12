#' @title  Tracking of entities within plots
#'
#' @description
#'
#' This shiny module offers a UI to select a subset of a dataset and
#' superimpose quantitative values of this selection on the complete plot
#' Three modes of selection are implemented:
#'
#' - 'Protein list': xxx,
#' - 'Random': xxx,
#' - 'Specific column': xxx
#'
#' @name mod_tracker
#' 
#' @param id xxx
#' @param object A instance of the class `SummarizedExperiment`
#' @param dataIn xxx
#' @param remoteReset xxx
#' @param is.enabled xxx
#' 
#' @examples
#' if (interactive()){
#' data(Exp1_R25_prot, package = "DaparToolshedData")
#' shiny::runApp(mod_tracker(Exp1_R25_prot))
#' }
#' 
#' @import QFeatures
#' @import DaparToolshed
#' @import MagellanNTK
#'
NULL


#' @param id shiny id
#' @export
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyjs useShinyjs hidden
#'
#' @rdname mod_tracker
#'
#' @return NA
#'
mod_tracker_ui <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    uiOutput(ns("typeSelect_ui")),
    uiOutput(ns("listSelect_ui")),
    uiOutput(ns("randSelect_ui")),
    uiOutput(ns("colSelect_ui"))
  )
}


#' @rdname mod_tracker
#'
#' @export
#'
#' @importFrom shinyjs toggle hidden show hide
#' @importFrom stats setNames
#' @importFrom DaparToolshed idcol
#'
#' @return A `list()` of integers
#'
mod_tracker_server <- function(
    id,
    object = reactive({
      NULL
    }),
    remoteReset = reactive({
      0
    }),
    is.enabled = reactive({
      TRUE
    })) {
  widgets.default.values <- list(
    typeSelect = "None",
    listSelect = NULL,
    randSelect = NULL,
    colSelect = "None"
  )

  rv.custom.default.values <- list(
    # indices = NULL
  )


  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(is.enabled(), {
      print("tytyty")
      print(is.enabled())
    })


    eval(
      str2expression(
        MagellanNTK::Get_AdditionalModule_Core_Code(
          w.names = names(widgets.default.values),
          rv.custom.names = names(rv.custom.default.values)
        )
      )
    )

    observeEvent(req(object()),
      {
        req(inherits(object(), "SummarizedExperiment"))

        rv$dataIn <- object()

        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
      },
      priority = 1000
    )

    observeEvent(remoteReset(), ignoreInit = FALSE, ignoreNULL = TRUE, {
      lapply(names(rv.widgets), function(x) {
        rv.widgets[[x]] <- widgets.default.values[[x]]
      })

      lapply(names(rv.custom), function(x) {
        rv.custom[[x]] <- rv.custom.default.values[[x]]
      })
    })

    output$typeSelect_ui <- renderUI({
      tmp <- c("None", "ProteinList", "Random", "Column")
      nm <- c("None", "Protein list", "Random", "Specific Column")


      if (length(Get_LogicalCols_in_Dataset()) > 0) {
        .choices <- c(.choices, "Column" = "Column")
      }



      widget <- selectInput(ns("typeSelect"),
        "Type of selection",
        choices = stats::setNames(tmp, nm),
        selected = rv.widgets$typeSelect,
        width = "130px"
      )

      MagellanNTK::toggleWidget(widget, is.enabled())
    })


    output$listSelect_ui <- renderUI({
      req(rv$dataIn)
      req(rv.widgets$typeSelect == "ProteinList")

      widget <- selectInput(ns("listSelect"),
        "Select protein",
        choices = setNames(nm = rowData(rv$dataIn)[, DaparToolshed::idcol(rv$dataIn)]),
        multiple = TRUE,
        selected = rv.widgets$listSelect,
        width = "200px",
        selectize = TRUE
      )

      MagellanNTK::toggleWidget(widget, is.enabled())
    })


    output$colSelect_ui <- renderUI({
      req(rv$dataIn)
      req(rv.widgets$typeSelect == "Column")

      widget <- selectInput(ns("colSelect"),
        "Column of rowData",
        choices = setNames(nm = c("None", colnames(rowData(rv$dataIn)))),
        selected = rv.widgets$colSelect
      )

      MagellanNTK::toggleWidget(widget, is.enabled())
    })

    output$randSelect_ui <- renderUI({
      req(rv.widgets$typeSelect == "Random")

      widget <- textInput(ns("randSelect"),
        "Random",
        value = rv.widgets$randSelect,
        width = ("120px")
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })



    # Catch event on the list selection
    observeEvent(rv.widgets$listSelect, {
      req(rv$dataIn)

      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- match(
        rv.widgets$listSelect,
        rowData(rv$dataIn)[[DaparToolshed::idcol(rv$dataIn)]]
      )
    })


    observeEvent(rv.widgets$randSelect, {
      req(rv$dataIn)

      cond <- is.null(rv.widgets$randSelect)
      cond <- cond || rv.widgets$randSelect == ""
      cond <- cond || (as.numeric(rv.widgets$randSelect) < 0)
      cond <- cond || (as.numeric(rv.widgets$randSelect) > nrow(rv$dataIn))
      if (!cond) {
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- sample(seq_len(nrow(rv$dataIn)),
          as.numeric(rv.widgets$randSelect),
          replace = FALSE
        )
      }
    })

    observeEvent(rv.widgets$colSelect, {
      req(rv.widgets$colSelect != "None")

      .op1 <- rowData(rv$dataIn)[, rv.widgets$colSelect]
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- which(.op1 == 1)
    })

    return(reactive({
      dataOut
    }))
  })
}



#' @export
#' @rdname mod_tracker
#'
mod_tracker <- function(dataIn) {
  ui <- fluidPage(
    actionButton("rst_btn", "Reset"),
    checkboxInput("isEnabled_btn", "Enable/Disable"),
    mod_tracker_ui("track"),
    uiOutput("show")
  )

  server <- function(input, output, session) {
    rv <- reactiveValues(
      res = reactive({
        NULL
      })
    )
    rv$res <- mod_tracker_server(
      id = "track",
      object = reactive({
        dataIn
      }),
      remoteReset = reactive({
        input$rst_btn
      }),
      is.enabled = reactive({
        input$isEnabled_btn
      })
    )

    observeEvent(rv$res()$trigger, {
      # browser()
      print(rv$res()$value)
    })
  }
  app <- shiny::shinyApp(ui = ui, server = server)
}
