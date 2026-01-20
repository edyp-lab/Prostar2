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
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets 
#' Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut 
#' format_DT_server Timestamp toggleWidget
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
    h3('Download dataset'),
    p("Please be patient after clicking the button, as the file may need some time to be created."),
    uiOutput(ns("dl_xl")),
    #uiOutput(ns("dl_csv")),
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
  widget.type = c('Button','Button'),
  filename = "myDataset",
  excel.style = NULL,
  remoteReset = reactive({0}),
  is.enabled = reactive({TRUE})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      UI_type = NULL,
      export_file_qf = NULL,
      export_file_xlsx = NULL,
      data_save = NULL
    )
    
    GetType <- reactive({
      if (length(extension) != length(widget.type)) {
        # warning("Widget.type is not correctly configured. As one cannot decide,
        #     all values are set to default ('Link')")
        rv$UI_type <- rep("Link", length(extension))
      } else {
        rv$UI_type <- widget.type
      }
      
      rv$UI_type
    })
    
    observeEvent(req(dataIn()), ignoreNULL = TRUE, {
      rv$data_save <- dataIn()
    })
    
    ## Save as .xlsx -----
    output$dl_xl <- renderUI({
      req("xlsx" %in% extension)
      #req(rv$export_file_xlsx)
      
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
    
    output$downloadDataExcel <- downloadHandler(
      filename = function() {
        #paste("data-", Sys.Date(), ".xlsx", sep = "")
        paste(filename, '.xlsx', sep = "")
      },
      content = function(file) {
        rv$export_file_xlsx <- tryCatch({
          #browser()
          shiny::withProgress(message = paste0("Builds Excel file", id), {
            shiny::incProgress(0.5)
            print(paste0(id, ' : shiny::withProgress(message = paste0("Builds Excel file", id)'))
            out.xlsx <- tempfile(fileext = ".xlsx")
            DaparToolshed::write.excel(obj = rv$data_save, filename = out.xlsx)
            out.xlsx
          })
        },
          warning = function(w) w,
          error = function(e) e
        )
        file.copy(from = rv$export_file_xlsx, to = file)
      }
    )
    
    
    ## Save as .qf -----
    output$dl_raw <- renderUI({
      req("qf" %in% extension)
      #req(rv$export_file_qf)
      
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
        rv$export_file_qf <- tryCatch({
          shiny::withProgress(message = paste0("Builds QFeatures file", id), {
            shiny::incProgress(0.5)
            print(paste0(id, ' : shiny::withProgress(message = paste0("Builds QFeatures file", id),'))
            out.qf <- tempfile(fileext = ".qf")
            saveRDS(rv$data_save, file = out.qf)
            out.qf
          })
        },
          warning = function(w) w,
          error = function(e) e
        )
        file.copy(from = rv$export_file_qf, to = file)
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
