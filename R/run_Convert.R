#' @title Wrapper to Convert pipeline
#'
#' @description
#' These functions are inspired by the functions run_workflow() in the package
#' `MagellanNTK`. They wrap the entire workflow into a single function
#'
#' @param id xxx
#' @param remoteReset xxx
#' @param is.enabled xxx
#' 
#' 
#' @examples
#' if (interactive()){
#' library(DaparToolshed)
#' library(Prostar2)
#' library(spsComps)
#' shiny::runApp(Prostar2::convert_dataset())
#' }
#'
#' @name Convert_wrapper
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets 
#' source_shinyApp_files nav_process_ui nav_process_server source_wf_files 
#' Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut 
#' format_DT_ui format_DT_server Timestamp toggleWidget 
#' mod_popover_for_help_server mod_popover_for_help_ui
#'
NULL


#' @rdname Convert_wrapper
#' @export
convert_dataset_ui <- function(id) {
  requireNamespace("MagellanNTK")
  ns <- NS(id)
  tagList(
    MagellanNTK::nav_process_ui(ns("PipelineConvert_Convert"))
  )
}


#' @export
#' @rdname Convert_wrapper
#'
convert_dataset_server <- function(
    id,
    remoteReset = reactive({NULL}),
    is.enabled = reactive({TRUE})) {
  requireNamespace("MagellanNTK")


  path <- system.file("workflow/PipelineConvert", package = "Prostar2")
  dataIn <- NULL
  mode <- "user"


  MagellanNTK::source_shinyApp_files()

  MagellanNTK::source_wf_files(path)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dataOut <- reactiveVal()
    session$userData$workflow.path <- path

    dataOut(nav_process_server(
      id = "PipelineConvert_Convert",
      dataIn = reactive({data.frame()}),
      remoteReset = reactive({remoteReset()})
    )
    )
    
    return(reactive({dataOut()}))
  })
}





#' @rdname Convert_wrapper
#' @export
convert_dataset <- function() {
  ui <- fluidPage(
    uiOutput("test")
  )

  server <- function(input, output, session) {
    rv.core <- reactiveValues(
      result_convert = reactive({NULL})
    )




    output$test <- renderUI({
      rv.core$result_convert <- convert_dataset_server("Convert")

      convert_dataset_ui("Convert")
    })


    
    observeEvent(req(rv.core$result_convert()$dataOut()$trigger), ignoreNULL = TRUE, {
      print(rv.core$result_convert())
      print(rv.core$result_convert())
    })
  }

  app <- shiny::shinyApp(ui, server)
}
