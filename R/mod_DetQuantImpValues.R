#' @title   popover_for_help_ui and popover_for_help_server
#' @description  A shiny Module.
#'
#' @export
#' @importFrom shiny NS tagList
#' @importFrom shinyjs inlineCSS useShinyjs
#'
#' @param id xxx
#' @param obj xxx
#' @param quant xxx
#' @param factor xxx
#' @param dataIn xxx
#' @param remoteReset xxx
#' @param is.enabled xxx
#'
#' @name mod_DetQuantImpValues
#'
#' @examples
#' \dontrun{
#' data(ft_na)
#' shiny::runApp(mod_DetQuantImpValues(ft_na[[1]]))
#' }
#'
NULL




#' @rdname mod_DetQuantImpValues
#' @export
#'
mod_DetQuantImpValues_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5("The missing values will be imputed by the following values :"),
    DT::dataTableOutput(ns("detQuantValues_DT"))
  )
}

#' @rdname mod_DetQuantImpValues
#' @export
#'
mod_DetQuantImpValues_server <- function(
    id,
    dataIn = reactive({
      NULL
    }),
    quant = reactive({
      1
    }),
    factor = reactive({
      1
    }),
    remoteReset = reactive({
      0
    }),
    is.enabled = reactive({
      TRUE
    })) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$detQuantValues_DT <- DT::renderDataTable(server = TRUE, {
      req(dataIn())

      values <- getQuantile4Imp(
        SummarizedExperiment::assay(dataIn()),
        quant() / 100, factor()
      )
      DT::datatable(as.data.frame(t(values$shiftedImpVal)),
        rownames = FALSE,
        options = list(
          initComplete = initComplete(),
          dom = "t",
          bLengthChange = FALSE
        )
      )
    })
  })
}



#' @rdname mod_DetQuantImpValues
#' @export
#'
mod_DetQuantImpValues <- function(obj) {
  ui <- fluidPage(
    mod_DetQuantImpValues_ui("Title")
  )
  server <- function(input, output) {
    mod_DetQuantImpValues_server(
      id = "Title",
      dataIn = reactive({
        dataIn
      })
    )
  }

  app <- shiny::shinyApp(ui, server)
}
