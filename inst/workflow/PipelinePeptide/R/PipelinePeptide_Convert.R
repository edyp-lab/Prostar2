#' @title PipelinePeptide Convert module
#'
#' @description
#' This module contains the convert process of the peptide pipeline.
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
#'   Prostar2("PipelinePeptide_Convert")
#' }
#'
#' @name PipelinePeptide_Convert
#'
#' @importFrom stats setNames rnorm
#' @import omXplore
#' @importFrom shinyjs hidden useShinyjs toggle
#' @importFrom shinyFeedback showFeedbackWarning hideFeedback
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#'
NULL


#' @rdname PipelinePeptide_Convert
#' @export
#'
PipelinePeptide_Convert_conf <- function() {
  MagellanNTK::Config(
    fullname = "PipelinePeptide_Convert",
    mode = "process",
    steps = c("Select File", "Data Id", "Exp and Feat Data", "Design"),
    mandatory = c(TRUE, TRUE, TRUE, TRUE)
  )
}


#' @rdname PipelinePeptide_Convert
#' @export
#'
PipelinePeptide_Convert_ui <- function(id) {
  ns <- NS(id)
}


#' @rdname PipelinePeptide_Convert
#' @export
#'
PipelinePeptide_Convert_server <- function(
  id,
  dataIn = reactive({
    NULL
  }),
  steps.enabled = reactive({
    TRUE
  }),
  remoteReset = reactive({
    NULL
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
  requireNamespace(c("openxlsx", "shinyalert"))

  # Default values for widgets
  widgets.default.values <- list(
    SelectFile_software = "DIA_NN",
    SelectFile_file = NULL,
    SelectFile_typeOfData = "peptide",
    SelectFile_checkDataLogged = "no",
    SelectFile_replaceAllZeros = TRUE,
    SelectFile_XLSsheets = NULL,
    DataId_datasetId = "",
    DataId_parentProteinID = NULL,
    DataId_show_previewdatasetID = FALSE,
    DataId_show_previewProteinID = FALSE,
    ExpandFeatData_idMethod = FALSE,
    ExpandFeatData_quantCols = NULL,
    ExpandFeatData_inputGroup = reactive({
      NULL
    }),
    Save_analysis = "",
    Save_description = NULL
  )

  # Default values for reactive values
  rv.custom.default.values <- list(
    history = MagellanNTK::InitializeHistory(),
    tab = NULL,
    previewtab = NULL,
    inputGroup = NULL,
    design = NULL,
    name = NULL
  )

  ### -------------------------------------------------------------###
  ###                                                              ###
  ### -------------------- MODULE SERVER --------------------------###
  ###                                                              ###
  ### -------------------------------------------------------------###
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
    #-----------------------------DESCRIPTION-----------------------------------
    #
    ########################################################################### -
    output$Description <- renderUI({
      file <- normalizePath(file.path(
        system.file("workflow", package = "Prostar2"),
        unlist(strsplit(id, "_"))[1],
        "md",
        paste0(id, ".Rmd")
      ))

      MagellanNTK::process_layout_process(session,
        ns = NS(id),
        sidebar = tagList(),
        content = div(
          id = ns("div_content"),
          if (file.exists(file)) {
            includeMarkdown(file)
          } else {
            p("No Description available")
          }
        )
      )
    })

    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl("Description", btnEvents()))

      # DO NOT MODIFY THE NEXT THREE LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status["Description"] <- MagellanNTK::stepStatus$VALIDATED
    })


    ########################################################################### -
    #
    #------------------------------SELECT FILE----------------------------------
    #
    ########################################################################### -
    output$SelectFile <- renderUI({
      shinyjs::useShinyjs()

      MagellanNTK::process_layout_process(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns("SelectFile_software_ui")),
          uiOutput(ns("SelectFile_typeOfData_ui")),
          uiOutput(ns("SelectFile_checkDataLogged_ui")),
          uiOutput(ns("SelectFile_replaceAllZeros_ui"))
        ),
        content = tagList(
          div(
            style = "margin-left: 10px; display: flex; gap: 20px;",
            uiOutput(ns("SelectFile_file_ui")),
            uiOutput(ns("SelectFile_ManageXlsFiles_ui"))
          ),
          div(
            style = "margin-bottom: 5px;",
            uiOutput(ns("SelectFile_btn_previewfile_ui"))
          ),
          uiOutput(ns("SelectFile_previewfile_ui"))
        )
      )
    })

    #### _sidebar -----
    # Widget - data source
    output$SelectFile_software_ui <- renderUI({
      widget <- radioButtons(ns("SelectFile_software"),
        "Data source",
        choices = setNames(nm = c("DIA_NN", "maxquant", "proline")),
        selected = rv.widgets$SelectFile_software
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["SelectFile"])
    })

    # Widget - type of data
    output$SelectFile_typeOfData_ui <- renderUI({
      widget <- radioButtons(ns("SelectFile_typeOfData"),
        "Type of dataset",
        choices = c("precursor or peptide dataset" = "peptide"),
        selected = rv.widgets$SelectFile_typeOfData
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["SelectFile"])
    })

    # Widget - log data
    output$SelectFile_checkDataLogged_ui <- renderUI({
      widget <- radioButtons(ns("SelectFile_checkDataLogged"),
        "Data already log-transformed",
        choices = c(
          "Yes" = "yes",
          "No" = "no"
        ),
        selected = rv.widgets$SelectFile_checkDataLogged
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["SelectFile"])
    })

    # Widget - replace by NA
    output$SelectFile_replaceAllZeros_ui <- renderUI({
      widget <- checkboxInput(ns("SelectFile_replaceAllZeros"),
        "Replace all 0 and NaN by NA",
        value = rv.widgets$SelectFile_replaceAllZeros
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["SelectFile"])
    })

    #### _content -----
    # Widget - data file
    output$SelectFile_file_ui <- renderUI({
      widget <- fileInput(
        ns("SelectFile_file"), "Data file",
        multiple = FALSE,
        accept = c(".txt", ".tsv", ".csv", ".xls", ".xlsx"),
        width = "400px"
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["SelectFile"])
    })

    # Check if file is from one of the authorized extension
    fileExt.ok <- reactive({
      req(rv.widgets$SelectFile_file$name)
      authorizedExts <- c("txt", "csv", "tsv", "xls", "xlsx")
      ext <- MagellanNTK::GetExtension(rv.widgets$SelectFile_file$name)
      !is.na(match(ext, authorizedExts))
    })

    # Widget - select sheet
    output$SelectFile_ManageXlsFiles_ui <- renderUI({
      req(rv.widgets$SelectFile_file)
      req(MagellanNTK::GetExtension(rv.widgets$SelectFile_file$name) %in% c("xls", "xlsx"))

      sheets <- c(DaparToolshed::listSheets(rv.widgets$SelectFile_file$datapath))
      widget <- selectInput(ns("SelectFile_XLSsheets"),
        "Select sheet",
        choices = sheets,
        width = "200px",
        selected = rv.widgets$SelectFile_XLSsheets
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["SelectFile"])
    })

    # Widget - preview button
    output$SelectFile_btn_previewfile_ui <- renderUI({
      req(rv.widgets$SelectFile_file)
      widget <- actionButton(ns("SelectFile_btn_previewfile"), "Preview file",
        class = "info"
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["SelectFile"])
    })

    # Hide preview when new file loaded
    observeEvent(rv.widgets$SelectFile_file, {
      rv.custom$previewtab <- NULL
    })

    # Show preview
    observeEvent(input$SelectFile_btn_previewfile, {
      req(rv.widgets$SelectFile_file)

      ext <- MagellanNTK::GetExtension(rv.widgets$SelectFile_file$name)
      rv.custom$name <- unlist(strsplit(rv.widgets$SelectFile_file$name,
        split = ".", fixed = TRUE
      ))[1]

      if ((ext %in% c("xls", "xlsx")) && (
        is.null(rv.widgets$SelectFile_XLSsheets) ||
          nchar(rv.widgets$SelectFile_XLSsheets) == 0)) {
        return(NULL)
      }

      if (!fileExt.ok()) {
        shinyjs::info("Warning : this file is not a text nor an Excel file !
                      Accepted extensions are .txt, .csv, .tsv, .xls and .xlsx.")
      } else {
        rv.custom$previewtab <- readFileConvert(
          file = rv.widgets$SelectFile_file,
          sheet = rv.widgets$SelectFile_XLSsheets
        )
      }
    })

    # Table preview (ui)
    output$SelectFile_previewfile_ui <- renderUI({
      req(rv.widgets$SelectFile_file)
      req(rv.custom$previewtab)

      MagellanNTK::format_DT_ui(ns("DT_previewfile"))
    })

    # Table preview (server)
    MagellanNTK::format_DT_server(
      "DT_previewfile",
      reactive({
        rv.custom$previewtab[seq_len(3), ]
      })
    )

    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl("SelectFile", btnEvents()))

      rv.widgets$SelectFile_XLSsheets <- input$SelectFile_XLSsheets
      if (is.null(rv.widgets$SelectFile_file) || is.null(rv.widgets$SelectFile_software) ||
        is.null(rv.widgets$SelectFile_typeOfData) || is.null(rv.widgets$SelectFile_checkDataLogged) ||
        is.null(rv.widgets$SelectFile_replaceAllZeros) ||
        (MagellanNTK::GetExtension(rv.widgets$SelectFile_file$name) %in% c("xls", "xlsx") && (
          is.null(rv.widgets$SelectFile_XLSsheets) ||
            nchar(rv.widgets$SelectFile_XLSsheets) == 0))) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("SelectFile process", id), {
          shiny::incProgress(0.5)

          ext <- MagellanNTK::GetExtension(rv.widgets$SelectFile_file$name)
          rv.custom$name <- unlist(strsplit(rv.widgets$SelectFile_file$name,
            split = ".", fixed = TRUE
          ))[1]

          if (!fileExt.ok()) {
            shinyjs::info("Warning : this file is not a text nor an Excel file !
       Accepted extensions are .txt, .csv, .tsv, .xls and .xlsx.")
          } else {
            rv.custom$tab <- readFileConvert(
              file = rv.widgets$SelectFile_file,
              sheet = rv.widgets$SelectFile_XLSsheets
            )

            # DO NOT MODIFY THE NEXT THREE LINES
            dataOut$trigger <- MagellanNTK::Timestamp()
            dataOut$value <- NULL
            rv$steps.status["SelectFile"] <- MagellanNTK::stepStatus$VALIDATED
          }
          shiny::incProgress(1)
        })
      }
    })

    ########################################################################### -
    #
    #--------------------------------DATA ID------------------------------------
    #
    ########################################################################### -
    output$DataId <- renderUI({
      shinyjs::useShinyjs()

      MagellanNTK::process_layout_process(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns("helpTextDataID"))
        ),
        content = tagList(
          tags$style(HTML(".ID_data img { margin: 5px; }")),
          fluidRow(
            column(
              width = 6,
              div(
                style = "display: flex; margin-left: 10px;", class = "ID_data",
                uiOutput(ns("DataId_datasetId_ui")),
                uiOutput(ns("DataId_warningNonUniqueID_img_ui"))
              ),
              uiOutput(ns("DataId_warningNonUniqueID_txt_ui")),
              uiOutput(ns("DataId_show_previewdatasetID_ui")),
              uiOutput(ns("DataId_previewdatasetID_ui"))
            ),
            column(
              width = 6,
              uiOutput(ns("DataId_parentProteinID_ui")),
              uiOutput(ns("DataId_show_previewProteinID_ui")),
              uiOutput(ns("DataId_previewProteinID_ui"))
            )
          )
        )
      )
    })

    #### _sidebar -----
    # Txt help sidebar
    output$helpTextDataID <- renderUI({
      req(rv.widgets$SelectFile_typeOfData)

      t <- switch(rv.widgets$SelectFile_typeOfData,
        protein = "proteins",
        peptide = "peptides/precursors",
        default = ""
      )
      txt <- paste0("Please select the column in your dataset that contains the unique ", t, " IDs.")

      helpText(txt)
    })

    #### _content -----
    # Widget - ID select
    output$DataId_datasetId_ui <- renderUI({
      req(rv.widgets$SelectFile_typeOfData)
      req(rv.custom$tab)

      widget <- selectInput(ns("DataId_datasetId"),
        label = "Peptides IDs",
        choices = setNames(nm = c("", "AutoID", colnames(rv.custom$tab))),
        selected = rv.widgets$DataId_datasetId,
        width = "300px"
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["DataId"])
    })

    # Check if non unique ID
    datasetID_Ok <- reactive({
      req(rv.widgets$DataId_datasetId)
      req(rv.custom$tab)

      if (rv.widgets$DataId_datasetId == "AutoID") {
        t <- TRUE
      } else {
        t <- !anyDuplicated(rv.custom$tab[[rv.widgets$DataId_datasetId]])
      }

      t
    })

    # Img warning non unique ID
    output$DataId_warningNonUniqueID_img_ui <- renderUI({
      req(rv.custom$tab)

      if (!datasetID_Ok()) {
        img <- "images/Problem.png"
      } else {
        img <- "images/Ok.png"
      }

      div(style = "margin: 28px 0px 0px 5px;", img(src = img, height = 25))
    })

    # Txt warning non unique ID
    output$DataId_warningNonUniqueID_txt_ui <- renderUI({
      req(rv.custom$tab)

      if (!datasetID_Ok()) {
        text <- "Warning: Your ID contains duplicate data. Please choose another one."
        p(style = "color: red;", text)
      }
    })

    # Widget - preview ID button
    output$DataId_show_previewdatasetID_ui <- renderUI({
      req(rv.widgets$DataId_datasetId)

      widget <- checkboxInput(ns("DataId_show_previewdatasetID"),
        "Show preview",
        value = rv.widgets$DataId_show_previewdatasetID
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["DataId"])
    })

    # ID preview value
    previewdatasetID <- reactive({
      req(rv.widgets$DataId_datasetId)

      if (rv.widgets$DataId_datasetId == "AutoID") {
        data.frame(AutoID = seq_len(6))
      } else {
        head(rv.custom$tab[, rv.widgets$DataId_datasetId, drop = FALSE])
      }
    })

    # Table ID ID (server)
    MagellanNTK::format_DT_server(
      "DT_previewdatasetID",
      reactive({
        previewdatasetID()
      })
    )

    # Table ID (ui)
    output$DataId_previewdatasetID_ui <- renderUI({
      req(rv.widgets$DataId_datasetId)
      req(rv.widgets$DataId_show_previewdatasetID)

      MagellanNTK::format_DT_ui(ns("DT_previewdatasetID"))
    })

    # Widget - parentProtein ID select
    output$DataId_parentProteinID_ui <- renderUI({
      req(rv.widgets$SelectFile_typeOfData)
      req(rv.custom$tab)

      widget <- selectInput(ns("DataId_parentProteinID"),
        "Select protein IDs",
        choices = setNames(nm = c("", colnames(rv.custom$tab))),
        selected = rv.widgets$DataId_parentProteinID,
        width = "300px"
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["DataId"])
    })

    # Widget - parentProtein preview button
    output$DataId_show_previewProteinID_ui <- renderUI({
      req(rv.widgets$DataId_parentProteinID)

      widget <- checkboxInput(ns("DataId_show_previewProteinID"),
        "Show preview",
        value = rv.widgets$DataId_show_previewProteinID
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["DataId"])
    })

    # parentProt ID preview value
    previewProtID <- reactive({
      req(rv.widgets$DataId_parentProteinID)

      head(rv.custom$tab[, rv.widgets$DataId_parentProteinID, drop = FALSE])
    })

    # Table parentProtein ID (server)
    MagellanNTK::format_DT_server(
      "DT_previewProtID",
      reactive({
        previewProtID()
      })
    )

    # Table parentProtein ID (ui)
    output$DataId_previewProteinID_ui <- renderUI({
      req(rv.widgets$DataId_parentProteinID)
      req(rv.widgets$DataId_show_previewProteinID)

      MagellanNTK::format_DT_ui(ns("DT_previewProtID"))
    })

    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl("DataId", btnEvents()))

      if (is.null(rv.widgets$DataId_datasetId) || rv.widgets$DataId_datasetId == "" || !datasetID_Ok() ||
        (rv.widgets$SelectFile_typeOfData == "peptide" && (is.null(rv.widgets$DataId_parentProteinID) ||
          rv.widgets$DataId_parentProteinID == ""))) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("DataId process", id), {
          shiny::incProgress(0.5)

          # DO NOT MODIFY THE NEXT THREE LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["DataId"] <- MagellanNTK::stepStatus$VALIDATED
          shiny::incProgress(1)
        })
      }
    })


    ########################################################################### -
    #
    #--------------------------EXP AND FEAT DATA--------------------------------
    #
    ########################################################################### -
    output$ExpandFeatData <- renderUI({
      MagellanNTK::process_layout_process(session,
        ns = NS(id),
        sidebar = tagList(),
        content = tagList(
          tags$style(HTML(".ExpandFeatData_content img { margin: 5px; }")),
          div(
            class = "ExpandFeatData_content",
            div(
              style = "margin-bottom: 10px; margin-top: -15px;",
              uiOutput(ns("ExpandFeatData_warningNegValues_ui")),
              uiOutput(ns("ExpandFeatData_warningNonNum_ui"))
            ),
            div(
              style = "display:flex; gap: 25px;",
              uiOutput(ns("ExpandFeatData_quantCols_ui"), style = "margin-left: 15px;"),
              div(
                uiOutput(ns("ExpandFeatData_idMethod_ui")),
                uiOutput(ns("ExpandFeatData_inputGroup_ui"))
              )
            )
          )
        )
      )
    })

    #### _content -----
    # Widget - select quanti col
    output$ExpandFeatData_quantCols_ui <- renderUI({
      req(rv.custom$tab)
      widget <- selectInput(ns("ExpandFeatData_quantCols"),
        "Select quantification columns",
        choices = setNames(nm = colnames(rv.custom$tab)),
        multiple = TRUE, selectize = FALSE,
        width = "300px", size = 20,
        selected = rv.widgets$ExpandFeatData_quantCols
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["ExpandFeatData"])
    })

    # Txt warning neg value
    output$ExpandFeatData_warningNegValues_ui <- renderUI({
      req(rv.widgets$SelectFile_checkDataLogged == "no")
      req(rv.widgets$ExpandFeatData_quantCols)
      req(length(which(rv.custom$tab[, rv.widgets$ExpandFeatData_quantCols] < 0)) > 0)

      div(
        style = "display: flex;",
        img(src = "images/Problem.png", height = 25),
        p(
          style = "color: red;",
          "Warning: Your original dataset may contain negative values, which cannot be log-transformed. \n
          Please check your dataset or review the log-transformation option in the first tab."
        )
      )
    })

    # Txt warning non numerical
    output$ExpandFeatData_warningNonNum_ui <- renderUI({
      req(rv.custom$tab)
      req(rv.widgets$ExpandFeatData_quantCols)
      req(!all(sapply(
        rv.custom$tab[, rv.widgets$ExpandFeatData_quantCols, drop = FALSE],
        is.numeric
      )))

      div(
        style = "display: flex; margin-top: -15px;",
        img(src = "images/Problem.png", height = 25),
        p(
          style = "color: red; margin-top: 6px;",
          "Warning: At least one of the selected columns contains non-numerical data."
        )
      )
    })

    # Widget - provide id method
    output$ExpandFeatData_idMethod_ui <- renderUI({
      widget <- radioButtons(ns("ExpandFeatData_idMethod"),
        "Provide identification method",
        choices = list(
          "No (default values will be computed)" = FALSE,
          "Yes" = TRUE
        ),
        selected = rv.widgets$ExpandFeatData_idMethod
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["ExpandFeatData"])
    })

    # Widget - id col (server)
    observe({
      req(rv$steps.enabled["ExpandFeatData"])
      req(as.logical(rv.widgets$ExpandFeatData_idMethod))

      rv.widgets$ExpandFeatData_inputGroup <- Prostar2::mod_inputGroup_server("inputGroups",
        df = reactive({
          rv.custom$tab
        }),
        quantCols = reactive({
          rv.widgets$ExpandFeatData_quantCols
        }),
        is.enabled = reactive({
          rv$steps.enabled["ExpandFeatData"]
        })
      )
    })

    # Widget - id col (ui)
    output$ExpandFeatData_inputGroup_ui <- renderUI({
      req(as.logical(rv.widgets$ExpandFeatData_idMethod))

      widget <- mod_inputGroup_ui(ns("inputGroups"))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["ExpandFeatData"])
    })

    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl("ExpandFeatData", btnEvents()))

      if (is.null(rv.widgets$ExpandFeatData_quantCols) || !all(sapply(rv.custom$tab[, rv.widgets$ExpandFeatData_quantCols, drop = FALSE], is.numeric)) ||
        (as.logical(rv.widgets$ExpandFeatData_idMethod) && is.null(rv.widgets$ExpandFeatData_inputGroup()))) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("ExpandFeatData process", id), {
          shiny::incProgress(0.5)

          if (as.logical(rv.widgets$ExpandFeatData_idMethod)) {
            req(rv.widgets$ExpandFeatData_inputGroup())

            rv.custom$inputGroup <- rv.widgets$ExpandFeatData_inputGroup()
          }

          # DO NOT MODIFY THE NEXT THREE LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["ExpandFeatData"] <- MagellanNTK::stepStatus$VALIDATED
          shiny::incProgress(1)
        })
      }
    })


    ########################################################################### -
    #
    #--------------------------------DESIGN-------------------------------------
    #
    ########################################################################### -
    output$Design <- renderUI({
      MagellanNTK::process_layout_process(session,
        ns = NS(id),
        sidebar = tagList(),
        content = tagList(
          tags$style(HTML(".Design_content img { margin: 5px; }")),
          div(
            class = "Design_content",
            uiOutput(ns("Design_designEx_ui")),
            uiOutput(ns("dl_ui"))
          )
        )
      )
    })

    #### _content -----
    observe({
      req(rv$steps.enabled["Design"])

      rv.custom$design <- Prostar2::mod_buildDesign_server(
        "designEx",
        quantCols = reactive({
          rv.widgets$ExpandFeatData_quantCols
        }),
        remoteReset = reactive({
          remoteReset()
        }),
        is.enabled = reactive({
          rv$steps.enabled["Design"]
        })
      )
    })

    output$Design_designEx_ui <- renderUI({
      req(rv.widgets$ExpandFeatData_quantCols)
      rv.widgets$ExpandFeatData_quantCols
      remoteReset

      MagellanNTK::toggleWidget(mod_buildDesign_ui(ns("designEx")), rv$steps.enabled["Design"])
    })

    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl("Design", btnEvents()))

      if (is.null(rv.custom$design()$trigger)) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("Design process", id), {
          shiny::incProgress(0.5)

          # DO NOT MODIFY THE NEXT THREE LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status["Design"] <- MagellanNTK::stepStatus$VALIDATED
          shiny::incProgress(1)
        })
      }
    })


    ########################################################################### -
    #
    #-------------------------------------SAVE----------------------------------
    #
    ########################################################################### -
    output$Save <- renderUI({
      MagellanNTK::process_layout_process(session,
        ns = NS(id),
        sidebar = tagList(),
        content = tagList(div(
          style = "margin-left: 10px;",
          uiOutput(ns("dl_ui")),
          br(),
          uiOutput(ns("Save_infos_ui"))
        ))
      )
    })

    #### _content -----
    # Widget - analysis name and description
    output$Save_infos_ui <- renderUI({
      widget <- tagList(
        textInput(ns("Save_analysis"), "Name of the analysis",
          placeholder = "Name of the analysis", width = "400px"
        ),
        textAreaInput(ns("Save_description"), "Description of the analysis",
          placeholder = "Description of the analysis", height = "150px", width = "400px"
        )
      )

      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Save"])
    })

    # Download (ui) (after saving)
    output$dl_ui <- renderUI({
      # req(config@mode == 'process')
      req(rv$steps.status["Save"] == MagellanNTK::stepStatus$VALIDATED)

      MagellanNTK::download_dataset_ui(ns(paste0(id, "_createQuickLink")))
    })

    # output$Save_infos_dataset_UI <- renderUI({
    #   req(rv$dataIn)
    #   infos_dataset_server(
    #     id = "Convert_Save_infosdataset",
    #     dataIn = reactive({rv$dataIn})
    #   )
    #
    #   infos_dataset_ui(id = ns("Convert_Save_infosdataset"))
    # })

    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl("Save", btnEvents()))

      shiny::withProgress(message = paste0("Save process", id), {
        shiny::incProgress(0.5)

        # Add analysis name if none
        if ((rv.widgets$Save_analysis == "") || is.null(rv.widgets$Save_analysis)) {
          rv.widgets$Save_analysis <- "myDataset"
        }

        # Reorder if needed
        .indexForMetacell <- NULL
        if (!is.null(rv.custom$inputGroup)) {
          .indexForMetacell <- rv.custom$inputGroup[rv.custom$design()$order]
        }
        .indQData <- rv.widgets$ExpandFeatData_quantCols[rv.custom$design()$order]

        # Create QFeatures dataset file
        rv$dataIn <- DaparToolshed::createQFeatures(
          file = rv.widgets$SelectFile_file$name,
          data = rv.custom$tab,
          sample = as.data.frame(rv.custom$design()$design),
          indQData = .indQData,
          keyId = rv.widgets$DataId_datasetId,
          analysis = rv.widgets$Save_analysis,
          description = rv.widgets$Save_description,
          logData = rv.widgets$SelectFile_checkDataLogged == "no",
          indexForMetacell = .indexForMetacell,
          typeDataset = rv.widgets$SelectFile_typeOfData,
          parentProtId = rv.widgets$DataId_parentProteinID,
          force.na = rv.widgets$SelectFile_replaceAllZeros,
          software = rv.widgets$SelectFile_software,
          name.pipeline = "PipelinePeptide"
        )
        shiny::incProgress(1)
      })

      S4Vectors::metadata(rv$dataIn)$name.pipeline <- "PipelinePeptide"

      # DO NOT MODIFY THE NEXT FOUR LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      dataOut$name <- rv.widgets$Save_analysis
      rv$steps.status["Save"] <- MagellanNTK::stepStatus$VALIDATED

      # Download (server)
      Prostar2::download_dataset_server(paste0(id, "_createQuickLink"),
        dataIn = reactive({
          rv$dataIn
        }),
        filename = rv.widgets$Save_analysis
      )
    })

    ####### _END_ -----

    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  })
}
