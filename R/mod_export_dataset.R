#' @title Export dataset Shiyny app
#'
#' @description  A shiny Module.
#'
#' @param id xxx
#' @param dataIn xxx
#'
#' @name mod_export_dataset
#'
#' @examples
#' \dontrun{
#' data(lldata)
#' shiny::runApp(export_dataset(lldata))
#' }
#'
#' @return A list
#'
NULL




#' @export
#' @rdname mod_export_dataset
#' @import shiny
#'
export_dataset_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel(
      "Export dataset",
      textInput(ns("nameExport"),
        label = "",
        placeholder = "Enter the name"
      ),
      download_dataset_ui(ns("dl")),
      actionButton(ns("export_btn"), "Export")
    ),
    tabPanel(
      "Build pdf report",
      actionButton(ns("report_btn"), "Build report (pdf)")
    )
  )
}


#' @rdname mod_export_dataset
#'
#' @export
#' @importFrom shinyjs info
#' @importFrom shiny moduleServer reactiveValues observeEvent
#'
export_dataset_server <- function(id, dataIn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns



    download_dataset_server("dl",
      dataIn = reactive({
        data
      }),
      extension = c("csv", "xlsx", "qf"),
      filename = reactive({
        nameExport
      })
    )


    ## -- Open a MSnset File --------------------------------------------
    observeEvent(input$export_btn, ignoreInit = TRUE, {


    })
  })
}




#' @rdname mod_export_dataset
#'
#' @export
#' @importFrom shiny fluidPage tagList textOutput reactiveValues observeEvent
#' shinyApp
#'
export_dataset <- function(dataIn) {
  ui <- fluidPage(
    tagList(
      export_dataset_ui("export")
    )
  )

  server <- function(input, output, session) {
    export_dataset_server("export",
      dataIn = reactive({
        dataIn
      })
    )
  }

  app <- shiny::shinyApp(ui, server)
}
