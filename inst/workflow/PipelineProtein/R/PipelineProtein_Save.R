#' @title PipelineProtein Save module
#'
#' @description
#' This module contains the save step of the protein pipeline.
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
#'   Prostar2("PipelineProtein_Save")
#' }
#'
#' @name PipelineProtein_Save
#'
#' @importFrom stats setNames rnorm
#' @import omXplore
#' @importFrom shinyjs hidden useShinyjs toggle
#' @importFrom shinyFeedback showFeedbackWarning hideFeedback
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#'
NULL


#' @rdname PipelineProtein_Save
#' @export
#'
PipelineProtein_Save_conf <- function() {
  MagellanNTK::Config(
    fullname = "PipelineProtein_Save",
    mode = "process"
  )
}


#' @rdname PipelineProtein_Save
#' @export
#'
PipelineProtein_Save_ui <- function(id) {
  ns <- NS(id)
}


#' @rdname PipelineProtein_Save
#' @export
#'
PipelineProtein_Save_server <- function(
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
  widgets.default.values <- list()

  # Default values for reactive values
  rv.custom.default.values <- list(
    history = MagellanNTK::InitializeHistory()
  )

  ### ------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ### ------------------------------------------------------------###
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
    #-------------------------------------SAVE----------------------------------
    #
    ########################################################################### -
    output$Save <- renderUI({
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(),
        content = tagList(
          uiOutput(ns("dl_ui"))
        )
      )
    })

    #### _content -----
    # Copy the input dataset to use it during this step
    observeEvent(req(dataIn()), {
      rv$dataIn <- dataIn()
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

      shiny::withProgress(message = paste0("Saving all processes", id), {
        shiny::incProgress(0.5)

        S4Vectors::metadata(rv$dataIn)$name.pipeline <- "PipelineProtein"

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
    })

    ####### _END_ -----

    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  })
}
