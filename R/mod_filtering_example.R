#' @title Filtering example Shiny module
#'
#' @description
#' xxxx
#'
#' @name filtering-example
#'
#' @param id xxx
#' @param obj An instance of the class `SummarizedExperiment`
#' @param indices xxx
#' @param operation A character(1) that indicates whether to keep or remove
#' lines identified by indices. Available values are 'keep' (default)
#' or 'delete'
#' @param title xxx
#' @param dataIn xxx
#' @param remoteReset xxx
#' @param is.enabled xxx
#'
#' @return NA
#'
#' @examples
#' if (interactive()){
#' library(Prostar2)
#' library(shinyBS)
#' library(shiny)
#' library(DT)
#' data(Exp1_R25_prot, package = "DaparToolshedData")
#' obj <- Exp1_R25_prot[[1]]
#' indices <- 1:5
#' operation <- "delete"
#' shiny::runApp(mod_filtering_example(obj, indices, operation))
#' }
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut format_DT_server Timestamp toggleWidget
#'
NULL


#' @rdname filtering-example
#' @importFrom shinyBS bsModal
#' @importFrom shiny NS actionLink tagList radioButtons uiOutput
#' @importFrom DT dataTableOutput renderDataTable datatable formatStyle styleEqual
#' @importFrom stats setNames
#' @importFrom shiny moduleServer reactiveValues observeEvent NS tagList actionLink fluidRow column uiOutput hr reactive
#' @export
#'
mod_filtering_example_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("Preview_btn_UI")),
    uiOutput(ns('toto'))
  )
}




#' @rdname filtering-example
#' @importFrom shinyBS bsModal
#' @importFrom DT renderDataTable datatable formatStyle styleEqual renderDT DTOutput
#' @importFrom shiny moduleServer reactiveValues observeEvent NS tagList actionLink fluidRow column uiOutput hr reactive
#' @importFrom DaparToolshed typeDataset
#' @import shinyjqui
#' @export
#'
mod_filtering_example_server <- function(
    id,
    dataIn = reactive({NULL}),
    indices = reactive({NULL}),
    operation = "keep",
    title = "myTitle",
    remoteReset = reactive({0}),
    is.enabled = reactive({TRUE})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pkgs.require('magrittr')


    output$Preview_btn_UI <- renderUI({
      widget <- actionButton(ns("Preview_btn"), "Preview",
        class = "btn-info"
      )
      #MagellanNTK::toggleWidget(widget, TRUE)
      widget
    })
    
    
    observeEvent(input$Preview_btn, {
      showModal(shinyjqui::draggableModalDialog(
        id = ns('example_modal'),
        DT::DTOutput(ns("example_tab_filtered")),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Fermer")
        )
      ))

    })
    
    
    legendTypeMV <- list(
      MEC = "Missing in Entire Condition (MEC)",
      POV = "Partially Observed Value (POV)",
      identified = "Quant. by direct id",
      recovered = "Quant. by recovery",
      combined = "Combined tags"
    )


    rgb2col <- function(rgbmat) {
      ProcessColumn <- function(col) {
        grDevices::rgb(rgbmat[1, col],
          rgbmat[2, col],
          rgbmat[3, col],
          maxColorValue = 255
        )
      }
      sapply(1:ncol(rgbmat), ProcessColumn)
    }



    DarkenColors <- function(ColorsHex) {
      pkgs.require('grDevices')
      # Convert to rgb
      # This is the step where we get the matrix
      ColorsRGB <- grDevices::col2rgb(ColorsHex)

      # Darken colors by lowering values of RGB
      ColorsRGBDark <- round(ColorsRGB * 0.5)

      # Convert back to hex
      ColorsHexDark <- rgb2col(ColorsRGBDark)

      return(ColorsHexDark)
    }

      output$example_tab_filtered <- DT::renderDT   ({
        
      df <- omXplore::Build_enriched_qdata(dataIn())
      is.enriched <- !isTRUE(all.equal(dataIn(), df))
      colors <- omXplore::custom_metacell_colors()
      c.tags <- names(colors)
       c.colors <- unlist(colors, use.names = FALSE)
       c.tags <- c(c.tags, paste0("darken_", c.tags))
       c.colors <- c(c.colors, DarkenColors(c.colors))


      range.invisible <- c(((2 + (ncol(df) - 1) / 2)):ncol(df))

      .colDef <- if (is.enriched) {
        list(
          list(
            targets = range.invisible,
            visible = FALSE
          )
        )
      } else {
        NULL
      }

      index2darken <- NULL

      # Darken lines that will be filtered
      if (!is.null(indices())) {
        if (operation() == "keep") {
          index2darken <- (1:nrow(dataIn()))[-indices()]
        } else if (operation() == "delete") {
          index2darken <- indices()
        }
      }


      for (i in index2darken)
        df[i, range.invisible] <- paste0("darken_", df[i, range.invisible])


      dt <- DT::datatable(df,
        extensions = c("Scroller"),
        options = list(
          dom = "Brtip",
          pageLength = 15,
          orderClasses = TRUE,
          autoWidth = TRUE,
          deferRender = TRUE,
          bLengthChange = FALSE,
          scrollX = 200,
          scrollY = 500,
          scroller = TRUE,
          server = FALSE,
          columnDefs = .colDef
        )
      )


      if (is.enriched) {
        dt <- dt %>%
          DT::formatStyle(
            colnames(df)[2:(((ncol(df) - 1) / 2) + 1)],
            colnames(df)[range.invisible],
            backgroundColor = DT::styleEqual(c.tags, c.colors)
          )
      }

      dt
    })
  })
}





#' @rdname filtering-example
#' @importFrom shinyBS bsModal
#' @importFrom shiny moduleServer reactiveValues observeEvent NS tagList actionLink fluidRow column uiOutput hr reactive fluidPage
#' @importFrom DT renderDataTable datatable formatStyle styleEqual
#' @export
#'
mod_filtering_example <- function(
    obj,
    indices = NULL,
    operation = "keep",
    title = "myTitle") {
  ui <- fluidPage(
    tagList(
          mod_filtering_example_ui("tree")
    )
  )

  server <- function(input, output) {

    mod_filtering_example_server("tree",
      dataIn = reactive({obj}),
      indices = reactive({indices}),
      operation = reactive({operation}),
      title = reactive({title})
    )
  }

  app <- shiny::shinyApp(ui = ui, server = server)
}
