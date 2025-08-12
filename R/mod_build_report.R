#' @title dl
#'
#' @description  A shiny Module.
#'
#'
#' @param id internal
#' @param dataIn internal
#' @param filename xxx
#'
#' @return NA
#'
#' @name build_report
#' @examples
#' if (interactive()){
#' data(sub_R25)
#' shiny::runApp(build_report(sub_R25))
#'
#' shiny::runApp(build_report(sub_R25, filename = "myDataset"))
#' }
#' 
#' @import QFeatures
#' @import DaparToolshed
#' @import MagellanNTK
#'
NULL


#' @importFrom shiny NS tagList actionLink fluidRow column uiOutput hr
#'
#' @rdname build_report
#'
#' @export
#'
build_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Build report")
  )
}

#' @rdname build_report
#'
#' @export
#'
build_report_server <- function(
    id,
    dataIn = reactive({
      NULL
    }),
    filename = "myDataset") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues()

    observeEvent(dataIn(), ignoreNULL = TRUE, {

    })
  })
}




#' @rdname build_report
#'
#' @export
#'
build_report <- function(dataIn, filename = "myDataset") {
  ui <- build_report_ui("report")

  server <- function(input, output, session) {
    build_report_server("report",
      dataIn = reactive({
        dataIn
      }),
      filename = filename
    )
  }

  app <- shiny::shinyApp(ui = ui, server = server)
}
