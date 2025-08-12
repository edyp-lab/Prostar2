#' @title dl
#'
#' @description  A shiny Module.
#'
#'
#' @param id internal
#' @param dataIn internal
#' @param extension Available values are `csv` (default), `qf` and `Excel`.
#' @param widget.type Available values are `Button` and `Link` (default).
#' @param filename internal
#' @param excel.style xxx
#' @param remoteReset xxx
#' @param is.enabled xxx
#'
#' @return NA
#'
#' @name download_dataset
#' @examples
#' if (interactive()){
#' data(Exp1_R25_prot, package = "DaparToolshedData")
#' shiny::runApp(download_dataset(Exp1_R25_prot))
#' }
#' 
#' @import QFeatures
#' @import DaparToolshed
#' @import MagellanNTK
#'
NULL


#' @importFrom shiny NS tagList actionLink fluidRow column uiOutput hr reactive
#' @importFrom DaparToolshed write.excel
#'
#' @rdname download_dataset
#'
#' @export
#'
download_dataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("dl_xl")),
    uiOutput(ns("dl_csv")),
    uiOutput(ns("dl_raw"))
  )
}

#' @rdname download_dataset
#' @importFrom shiny moduleServer reactiveValues observeEvent NS tagList actionLink fluidRow column uiOutput hr reactive
#'
#' @export
#'
download_dataset_server <- function(
    id,
    dataIn = reactive({NULL}),
    extension = c("xlsx", "qf"),
    widget.type = "Link",
    filename = "myDataset",
    excel.style = NULL,
    remoteReset = reactive({0}),
    is.enabled = reactive({TRUE})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      UI_type = NULL,
      export_file_qf = NULL,
      export_file_xlsx = NULL
    )

    observeEvent(req(dataIn()), ignoreNULL = TRUE, {
      rv$export_file_xlsx <- tryCatch(
        {
          out.xlsx <- tempfile(fileext = ".xlsx")
          DaparToolshed::write.excel(obj = dataIn(), filename = out.xlsx)
          out.xlsx
        },
        warning = function(w) w,
        error = function(e) e
      )

      rv$export_file_qf <- tryCatch(
        {
          out.qf <- tempfile(fileext = ".qf")
          saveRDS(dataIn(), file = out.qf)
          out.qf
        },
        warning = function(w) w,
        error = function(e) e
      )
    })

    GetType <- reactive({
      if (length(extension) != length(widget.type)) {
        warning("Widget.type is not correctly configured. As one cannot decide,
            all values are set to default ('Link')")
        rv$UI_type <- rep("Link", length(extension))
      } else {
        rv$UI_type <- widget.type
      }

      rv$UI_type
    })


    output$dl_xl <- renderUI({
      req("xlsx" %in% extension)
      req(rv$export_file_xlsx)
      type <- GetType()[which(extension == "xlsx")]
      do.call(
        paste0("download", type),
        list(
          ns("downloadDataExcel"),
          "xlsx",
          class = if (type == "Button") actionBtnClass else ""
        )
      )
    })

    output$dl_raw <- renderUI({
      req("qf" %in% extension)
      req(rv$export_file_qf)
      type <- GetType()[which(extension == "qf")]

      do.call(
        paste0("download", type),
        list(
          ns("downloadDataQf"),
          "qf",
          class = if (type == "Button") actionBtnClass else ""
        )
      )
    })



    output$downloadDataQf <- downloadHandler(
      filename = function() {
        # paste ("data-", Sys.Date(), ".qf", sep = "")
        paste(filename, ".qf", sep = "")
      },
      content = function(file) {
        file.copy(
          from = rv$export_file_qf,
          to = file
        )
      }
    )

    output$downloadDataExcel <- downloadHandler(
      filename = function() {
        # paste("data-", Sys.Date(), ".xlsx", sep = "")
        paste(filename, ".xlsx", sep = "")
      },
      content = function(file) {
        file.copy(
          from = rv$export_file_xlsx,
          to = file
        )
      }
    )
  })
}




#' @rdname download_dataset
#'
#' @export
#'
download_dataset <- function(
    dataIn, 
  filename = "myDataset") {
  ui <- download_dataset_ui("dl")

  server <- function(input, output, session) {
    download_dataset_server("dl",
      dataIn = reactive({dataIn}),
      extension = c("csv", "xlsx", "qf"),
      filename = filename
    )
  }

  app <- shiny::shinyApp(ui = ui, server = server)
}
