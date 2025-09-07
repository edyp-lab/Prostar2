#' @title  Help popover windows
#'
#' @description  A shiny Module.
#'
#' @param id A `character(1)` xxx
#' @param df xxx
#' @param quantCols A vector of
#' @param remoteReset xxx
#' @param is.enabled xxx
#'
#' @name mod_inputGroup
#'
#' @return A shiny app
#'
#' @examples
#' if (interactive()){
#' shiny::runApp(Prostar2::mod_inputGroup())
#' }
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut format_DT_ui format_DT_server Timestamp toggleWidget
#'
NULL



#' @rdname mod_inputGroup
#' @export
#' @importFrom shiny NS tagList
#' @importFrom shinyjs inlineCSS useShinyjs
#'
mod_inputGroup_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("inputGroup")),
    uiOutput(ns("checkIdentificationTab"))
  )
}


#'
#' @rdname mod_inputGroup
#'
#' @export
#'
mod_inputGroup_server <- function(
    id,
    df = reactive({
      NULL
    }),
    quantCols = reactive({
      NULL
    }),
    remoteReset = reactive({
      0
    }),
    is.enabled = reactive({
      TRUE
    })) {
  requireNamespace("shinyBS")

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      dataOut = NULL
    )


    shinyOutput <- function(FUN, id, num, ...) {
      inputs <- character(num)
      for (i in seq_len(num)) {
        inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
      }
      inputs
    }


    # function for dynamic inputs in DT
    shinyInput <- function(FUN, id, num, ...) {
      inputs <- character(num)
      for (i in seq_len(num)) {
        inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
      }

      inputs
    }


    # function to read DT inputs
    shinyValue <- function(id, num) {
      unlist(lapply(seq_len(num), function(i) {
        value <- input[[paste0(id, i)]]
        if (is.null(value)) NA else value
      }))
    }


    #
    #
    # End of extra functions
    #
    output$inputGroup <- renderUI({
      req(quantCols())
      req(df())
      n <- length(quantCols())


      input_list <- lapply(seq_len(n), function(i) {
        inputName <- ns(paste("input_", i, sep = ""))
        div(
          div(
            style = "align: center;display:inline-block; vertical-align:
          middle;padding-right: 10px;",
            p(tags$strong(paste0("Identification col. for ", quantCols()[i])))
          ),
          div(
            style = "align: center;display:inline-block; vertical-align: middle;",
            selectInput(inputName, "",
              choices = c("None", colnames(df())),
              width = "300px"
            )
          )
        )
      })
      do.call(tagList, input_list)
    })


    observeEvent(req(input[["input_1"]]), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(quantCols())
      req(df())

      n <- length(quantCols())
      lapply(seq(2, n), function(i) {
        inputName <- paste("input_", i, sep = "")
        start <- which(colnames(df()) == input[["input_1"]])

        if (input[["input_1"]] == "None") {
          .select <- "None"
        } else {
          .select <- colnames(df())[(i - 1) + start]
        }
        updateSelectInput(session, inputName, selected = .select)
      })
    })



    isOk <- reactive({
      req(quantCols())
      req(df())

      shinyValue("input_", length(quantCols()))
      temp <- shinyValue("input_", length(quantCols()))

      res <- NULL
      if (length(which(temp == "None")) > 0) {
        txt <- "The identification method is not appropriately defined for
      each sample."
        res <- list(trigger = MagellanNTK::Timestamp(), ok = FALSE, temp = temp, txt = txt)
        rv$dataOut <- NULL
      } else {
        if (length(temp) != length(unique(temp))) {
          txt <- "There are duplicates in identification columns."
          res <- list(trigger = MagellanNTK::Timestamp(), ok = FALSE, temp = temp, txt = txt)
          rv$dataOut <- NULL
        } else {
          txt <- "Correct"
          res <- list(trigger = MagellanNTK::Timestamp(), ok = TRUE, temp = temp, txt = txt)
          rv$dataOut <- temp
        }
      }

      res
    })



    output$checkIdentificationTab <- renderUI({
      req(isOk())
      if (isOk()$ok) {
        img <- "images/Ok.png"
      } else {
        img <- "images/Problem.png"
      }

      tags$div(
        tags$div(
          tags$div(
            style = "display:inline-block;",
            tags$img(src = img, height = 25)
          ),
          tags$div(style = "display:inline-block;", tags$p(isOk()$txt))
        )
      )
    })

    reactive({
      rv$dataOut
    })
  })
}



#' @export
#' @rdname mod_inputGroup
#' @importFrom utils read.csv
#'
mod_inputGroup <- function() {
  ui <- Prostar2::mod_inputGroup_ui("mod_inputGroup")

  server <- function(input, output, session) {
    file <- system.file("extdata/Exp1_R25_prot.txt", package = "DaparToolshedData")
    df <- read.csv(file, header = TRUE, sep = "\t", as.is = T)

    Prostar2::mod_inputGroup_server("mod_inputGroup",
      df = reactive({
        df
      }),
      quantCols = reactive({
        colnames(df)[49:54]
      })
    )
    #
    # observeEvent(toto(), ignoreNULL = FALSE,{
    #   print(toto())
    # })
  }

  app <- shinyApp(ui = ui, server = server)
}
