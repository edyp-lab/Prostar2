

#' @title xxx
#' @description xxx
#' @name mod_convert
#' @author Samuel Wieczorek, Manon Gaudin
#' @examples
#' if (interactive()){
#' data("Exp1_R25_prot", package = "DaparToolshedData")
#' path <- system.file('workflow/PipelineConvert', package = 'Prostar2')
#' shiny::runApp(workflowApp("PipelineConvert_Convert", path, dataIn = Exp1_R25_prot))
#' }
#' 
NULL

redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- 'info'

optionsBtnClass <- "info"
options(shiny.fullstacktrace = TRUE,
  shiny.maxRequestSize=3000*1024^2)

#' @rdname mod_convert
#' @export
#' 
PipelineConvert_Convert_conf <- function(){
  # This list contains the basic configuration of the process
  MagellanNTK::Config(
    fullname = 'PipelineConvert_Convert',
    # Define the type of module
    mode = 'process',
    # List of all steps of the process
    steps = c('Select File', 'Data Id', 'Exp and Feat Data', 'Design'),
    # A vector of boolean indicating if the steps are mandatory or not.
    mandatory = c(TRUE, TRUE, TRUE, TRUE)
    
  )
}



#' @title   mod_choose_pipeline_ui and mod_choose_pipeline_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_convert
#'
#' @keywords internal
#' @export
#'
#' @importFrom shiny NS tagList
#' @import sos
#'
#' @return NA
#'
PipelineConvert_Convert_ui <- function(id) {
  ns <- NS(id)
}




#' Convert Server Function
#'
#' @param id xxx
#' @param dataIn xxx
#' @param steps.enabled xxx
#' @param remoteReset xxx
#'
#' @import QFeatures
#' @importFrom shinyjs disabled
#' @importFom stats setNames
#' @importFrom utils read.csv
#' @importFrom spsComps addLoader
#'
#' @export
#'
#' @rdname mod_convert
#'
#' @return NA
#'
PipelineConvert_Convert_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1})
) {
  
  requireNamespace(c("openxlsx", "shinyalert"))
  
  widgets.default.values <- list(
    SelectFile_software = '',
    SelectFile_file = NULL,
    SelectFile_typeOfData = "peptide",
    SelectFile_checkDataLogged = "no",
    SelectFile_replaceAllZeros = TRUE,
    SelectFile_XLSsheets = NULL,
    
    DataId_datasetId = NULL,
    DataId_parentProteinID = NULL,
    DataId_show_previewdatasetID = FALSE,
    DataId_show_previewProteinID = FALSE,
    
    ExpandFeatData_idMethod = FALSE,
    ExpandFeatData_quantCols = NULL,
    ExpandFeatData_inputGroup = NULL,
    
    Save_analysis = NULL,
    Save_description = NULL
  )
  
  rv.custom.default.values <- list(
    tab = NULL,
    previewtab = NULL,
    design = NULL,
    name = NULL
  )
  
  ### -------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ### -------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    core.code <- MagellanNTK::Get_Workflow_Core_Code(
      mode = 'process',
      name = id,
      w.names = names(widgets.default.values),
      rv.custom.names = names(rv.custom.default.values)
    )
    eval(str2expression(core.code))
    
    
    timeline_process_server(
      id = 'Description_timeline',
      config = PipelineConvert_Convert_conf(),
      status = reactive({steps.status()}),
      position = reactive({current.pos()}),
      enabled = reactive({steps.enabled()})
    )
    
    
    
    timeline_process_server(
      id = 'SelectFile_timeline',
      config = PipelineConvert_Convert_conf(),
      status = reactive({steps.status()}),
      position = reactive({current.pos()}),
      enabled = reactive({steps.enabled()})
    )
    
    timeline_process_server(
      id = 'DataId_timeline',
      config = PipelineConvert_Convert_conf(),
      status = reactive({steps.status()}),
      position = reactive({current.pos()}),
      enabled = reactive({steps.enabled()})
    )
    
    
    timeline_process_server(
      id = 'ExpandFeatData_timeline',
      config = PipelineConvert_Convert_conf(),
      status = reactive({steps.status()}),
      position = reactive({current.pos()}),
      enabled = reactive({steps.enabled()})
    )
    
    timeline_process_server(
      id = 'Design_timeline',
      config = PipelineConvert_Convert_conf(),
      status = reactive({steps.status()}),
      position = reactive({current.pos()}),
      enabled = reactive({steps.enabled()})
    )
    
    
    timeline_process_server(
      id = 'Save_timeline',
      config = PipelineConvert_Convert_conf(),
      status = reactive({steps.status()}),
      position = reactive({current.pos()}),
      enabled = reactive({steps.enabled()})
    )
    
    
    observeEvent(input$Description_Sidebar, ignoreNULL = TRUE, {
      dataOut$sidebarState <- input$Description_Sidebar
    })
    
    observeEvent(input$SelectFile_Sidebar, ignoreNULL = TRUE, {
      dataOut$sidebarState <- input$SelectFile_Sidebar
    })
    
    observeEvent(input$DataId_Sidebar, ignoreNULL = TRUE, {
      dataOut$sidebarState <- input$DataId_Sidebar
    })
    
    observeEvent(input$ExpandFeatData_Sidebar, ignoreNULL = TRUE, {
      dataOut$sidebarState <- input$ExpandFeatData_Sidebar
    })
    observeEvent(input$Design_Sidebar, ignoreNULL = TRUE, {
      dataOut$sidebarState <- input$Design_Sidebar
    })
    
    observeEvent(input$Save_Sidebar, ignoreNULL = TRUE, {
      dataOut$sidebarState <- input$Save_Sidebar
    })
    
    # >>>
    # >>> START ------------- Code for Description UI---------------
    
    output$Description <- renderUI({
      file <- normalizePath(file.path(session$userData$workflow.path, 
        'md', paste0(id, '.md')))
      
      
      
      bslib::layout_sidebar(
        tags$head(tags$style(".sidebar-content {background-color: lightblue; width: 300px;}"),
          tags$style(".shiny-input-panel {background-color: lightblue;}")
        ),
        sidebar = bslib::sidebar(
          id = ns("Description_Sidebar"),  # Add an explicit ID
          tags$style(".shiny-input-panel {background-color: lightblue;}"),
          
          timeline_process_ui(ns('Description_timeline')),
          
          inputPanel(
            uiOutput(ns('Description_btn_validate_ui'))
          ),
          position = "left",
          padding = c(100, 0) # 1ere valeur : padding vertical, 2eme : horizontal
          #style = "p1"
        ),
        if (file.exists(file))
          includeMarkdown(file)
        else
          p('No Description available'),
        
        
        # Used to show some information about the dataset which is loaded
        # This function must be provided by the package of the process module
        uiOutput(ns('datasetDescription_ui'))
      )
    })
    
    output$datasetDescription_ui <- renderUI({
      # Insert your own code to visualize some information
      # about your dataset. It will appear once the 'Start' button
      # has been clicked
      
    })
    
    output$Description_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Description_btn_validate"), "Start",
        class = "btn-success")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Description'])
    })
    
    
    observeEvent(input$Description_btn_validate, {
      rv$dataIn <- dataIn()
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- stepStatus$VALIDATED
    })
    
    # <<< END ------------- Code for Description UI---------------
    
    
    # >>> START ------------- Code for step 1 UI---------------
    
    output$SelectFile <- renderUI({
      
      
      
      bslib::layout_sidebar(
        tags$head(tags$style(".sidebar-content {background-color: lightblue; width: 300px; height: 400px;}"),
          tags$style(".shiny-input-panel {background-color: lightblue; }")
          #tags$head(tags$style(HTML("#sidebar_id { overflow: hidden;}"))),
        ),
        sidebar = bslib::sidebar(
          id = ns('SelectFile_Sidebar'),
          timeline_process_ui(ns('SelectFile_timeline')),
          hr(style = "border-top: 3px solid #000000;"),
          uiOutput(ns('SelectFile_btn_validate_ui')),
          inputPanel(
            uiOutput(ns('SelectFile_software_ui'), style = "margin-right : 30px;"),
            uiOutput(ns('SelectFile_file_ui'), style = "margin-right : 30px;"),
            uiOutput(ns('SelectFile_ManageXlsFiles_ui')),
            uiOutput(ns('SelectFile_typeOfData_ui'), style = "margin-right : 30px;"),
            uiOutput(ns('SelectFile_checkDataLogged_ui'), style = "margin-right : 30px;"),
            uiOutput(ns('SelectFile_replaceAllZeros_ui'))
          ),
          position = "left",
          padding = c(100, 0),
          style = "z-index: 0;"
        ),
        
        uiOutput(ns('SelectFile_btn_previewfile_ui')),
        uiOutput(ns('SelectFile_previewfile_ui'), style = "width: 500px, overflow: auto;"),
        
      )
      
      
    })
    
    
    # >>> START: Definition of the widgets
    
    ## File selection -----
    MagellanNTK::mod_popover_for_help_server(
      "help_filesoftware",
      title = "Data source",
      content = "Which software the data comes from."
    )
    
    output$SelectFile_software_ui <- renderUI({
      widget <- radioButtons(ns("SelectFile_software"), 
        MagellanNTK::mod_popover_for_help_ui(ns("help_filesoftware")),
        choices = setNames(nm = c("DIA_NN", "maxquant", "proline")),
        selected = rv.widgets$SelectFile_software)
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['SelectFile'] )
    })
    
    MagellanNTK::mod_popover_for_help_server(
      "help_chooseFile",
      title = "Data file",
      content = "Select one (.txt, .csv, .tsv, .xls, .xlsx) file."
    )
    
    output$SelectFile_file_ui <- renderUI({
      req(rv.widgets$SelectFile_software)
      widget <- fileInput(
        ns("SelectFile_file"), MagellanNTK::mod_popover_for_help_ui(ns("help_chooseFile")),
        multiple = FALSE,
        accept = c(".txt", ".tsv", ".csv", ".xls", ".xlsx"),
        width = "400px"
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['SelectFile'])
    })
    
    fileExt.ok <- reactive({
      req(rv.widgets$SelectFile_file$name)
      authorizedExts <- c("txt", "csv", "tsv", "xls", "xlsx")
      ext <- GetExtension(rv.widgets$SelectFile_file$name)
      !is.na(match(ext, authorizedExts))
    })
    
    MagellanNTK::mod_popover_for_help_server(
      "help_sheets",
      title = "Select sheet",
      content = "Select the sheet containing the data to process."
    )
    
    output$SelectFile_ManageXlsFiles_ui <- renderUI({
      req(rv.widgets$SelectFile_software)
      req(rv.widgets$SelectFile_file)
      req(GetExtension(rv.widgets$SelectFile_file$name) %in% c("xls", "xlsx"))
      
      # tryCatch({   
      sheets <- c('', DaparToolshed::listSheets(rv.widgets$SelectFile_file$datapath))
      widget <- selectInput(ns("SelectFile_XLSsheets"), 
        MagellanNTK::mod_popover_for_help_ui(ns("help_sheets")), 
        choices = sheets, 
        width = "200px",
        selected = rv.widgets$SelectFile_XLSsheets)
      # },
      # warning = function(w) {
      #   shinyjs::info(conditionMessage(w))
      #   return(NULL)
      # },
      # error = function(e) {
      #   shinyjs::info(conditionMessage(e))
      #   return(NULL)
      # },
      # finally = {
      #   # cleanup-code
      # }
      # )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['SelectFile'])
    })
    
    ## File preview -----
    MagellanNTK::mod_popover_for_help_server(
      "help_previewfile",
      title = "",
      content = "Note that it may take some time, especially for large files."
    )
    
    output$SelectFile_btn_previewfile_ui <- renderUI({
      req(rv.widgets$SelectFile_file)
      widget <-  actionButton(ns("SelectFile_btn_previewfile"), "Preview file",
        class = 'info')
      
      div(style="display: flex; flex-wrap: wrap;",
        MagellanNTK::toggleWidget(widget, rv$steps.enabled['SelectFile'] ),
        MagellanNTK::mod_popover_for_help_ui(ns("help_previewfile")))
    })
    
    observeEvent(input$SelectFile_btn_previewfile, {
      req(rv.widgets$SelectFile_file)
      
      ext <- GetExtension(rv.widgets$SelectFile_file$name)
      rv.custom$name <- unlist(strsplit(rv.widgets$SelectFile_file$name, 
        split='.', fixed = TRUE))[1]
      if ((ext %in% c("xls", "xlsx")) && (
        is.null(rv.widgets$SelectFile_XLSsheets) ||
          nchar(rv.widgets$SelectFile_XLSsheets) == 0))
        return(NULL)
      
      if (!fileExt.ok()) {
        shinyjs::info("Warning : this file is not a text nor an Excel file !
                      Accepted extensions are .txt, .csv, .tsv, .xls and .xlsx.")
      } else {
        tryCatch({
          f.path <- rv.widgets$SelectFile_file$datapath
          rv.custom$previewtab <- switch(ext,
            txt = read.csv(f.path, header = TRUE, sep = "\t", as.is = T),
            csv = read.csv(f.path, header = TRUE, sep = ";", as.is = T),
            tsv = read.csv(f.path, header = TRUE, sep = "\t", as.is = T),
            xls = DaparToolshed::readExcel(f.path, sheet = rv.widgets$SelectFile_XLSsheets),
            xlsx = DaparToolshed::readExcel(f.path, sheet = rv.widgets$SelectFile_XLSsheets)
          )
          
          colnames(rv.custom$previewtab) <- gsub(".", "_", colnames(rv.custom$previewtab), fixed = TRUE)
          colnames(rv.custom$previewtab) <- gsub(" ", "_", colnames(rv.custom$previewtab), fixed = TRUE)
        },
          warning = function(w) {
            shinyjs::info(conditionMessage(w))
            return(NULL)
          },
          error = function(e) {
            shinyjs::info(conditionMessage(e))
            return(NULL)
          },
          finally = {
            # cleanup-code
          })
      }
    })
    
    MagellanNTK::format_DT_server('DT_previewfile',
      reactive({rv.custom$previewtab[1:3,]}))
    
    output$SelectFile_previewfile_ui <- renderUI({
      req(rv.widgets$SelectFile_file)
      req(rv.custom$previewtab)
      
      tagList(
        p(style = "color: black; font-weight: bold;", "Preview :"),
        MagellanNTK::format_DT_ui(ns('DT_previewfile'))
      )
    })
    
    
    ## File options -----
    MagellanNTK::mod_popover_for_help_server(
      "help_typeOfData",
      title = "Type of dataset",
      content = "Peptide or protein dataset."
    )
    
    output$SelectFile_typeOfData_ui <- renderUI({
      widget <- radioButtons(ns("SelectFile_typeOfData"), 
        MagellanNTK::mod_popover_for_help_ui(ns("help_typeOfData")),
        choices = c("peptide dataset" = "peptide",
          "protein dataset" = "protein"
        ),
        selected = rv.widgets$SelectFile_typeOfData)
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['SelectFile'] )
    })
    
    MagellanNTK::mod_popover_for_help_server(
      "help_checkDataLogged",
      title = "Data already log-transformed",
      content = "If not, a log2 transformation will be applied to the data automatically."
    )
    
    output$SelectFile_checkDataLogged_ui <- renderUI({
      widget <- radioButtons(ns("SelectFile_checkDataLogged"), 
        MagellanNTK::mod_popover_for_help_ui(ns("help_checkDataLogged")),
        choices = c("Yes" = "yes",
          "No" = "no"),
        selected = rv.widgets$SelectFile_checkDataLogged
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['SelectFile'] )
    })
    
    output$SelectFile_replaceAllZeros_ui <- renderUI({
      widget <- checkboxInput(ns("SelectFile_replaceAllZeros"), 
        "Replace all 0 and NaN by NA",
        value = rv.widgets$SelectFile_replaceAllZeros
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['SelectFile'] )
    })
    
    
    ## Validation button -----
    loader_inline_SelectFile <- spsComps::addLoader$new("SelectFile_btn_validate", color = "blue", method = "inline", type = "spinner")
    
    output$SelectFile_btn_validate_ui <- renderUI({
      widget <-  actionButton(ns("SelectFile_btn_validate"), "Perform",
        class = "btn-success")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['SelectFile'] )
    })
    # >>> END: Definition of the widgets
    
    observeEvent(input$SelectFile_btn_validate, {
      # Do some stuff
      req(rv.widgets$SelectFile_file)
      loader_inline_SelectFile$show()
      
      ext <- GetExtension(rv.widgets$SelectFile_file$name)
      rv.custom$name <- unlist(strsplit(rv.widgets$SelectFile_file$name, 
        split='.', fixed = TRUE))[1]
      if ((ext %in% c("xls", "xlsx")) && (
        is.null(rv.widgets$SelectFile_XLSsheets) ||
          nchar(rv.widgets$SelectFile_XLSsheets) == 0))
        return(NULL)
      
      if (!fileExt.ok()) {
        shinyjs::info("Warning : this file is not a text nor an Excel file !
     Accepted extensions are .txt, .csv, .tsv, .xls and .xlsx.")
      } else {
        tryCatch({
          
          #shinyjs::disable("SelectFile_file")
          f.path <- rv.widgets$SelectFile_file$datapath
          rv.custom$tab <- switch(ext,
            txt = read.csv(f.path, header = TRUE, sep = "\t", as.is = T),
            csv = read.csv(f.path, header = TRUE, sep = ";", as.is = T),
            tsv = read.csv(f.path, header = TRUE, sep = "\t", as.is = T),
            xls = DaparToolshed::readExcel(f.path, sheet = rv.widgets$SelectFile_XLSsheets),
            xlsx = DaparToolshed::readExcel(f.path, sheet = rv.widgets$SelectFile_XLSsheets)
          )
          
          colnames(rv.custom$tab) <- gsub(".", "_", colnames(rv.custom$tab), fixed = TRUE)
          colnames(rv.custom$tab) <- gsub(" ", "_", colnames(rv.custom$tab), fixed = TRUE)
        },
          warning = function(w) {
            shinyjs::info(conditionMessage(w))
            return(NULL)
          },
          error = function(e) {
            shinyjs::info(conditionMessage(e))
            return(NULL)
          },
          finally = {
            # cleanup-code
          })
        
        
        # DO NOT MODIFY THE THREE FOLLOWINF LINES
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status['SelectFile'] <- stepStatus$VALIDATED
      }
      loader_inline_SelectFile$hide()
    })
    
    # <<< END ------------- Code for step 1 UI---------------
    
    
    # >>> START ------------- Code for step 2 UI---------------
    
    output$DataId <- renderUI({
      bslib::layout_sidebar(
        tags$head(tags$style(".sidebar-content {background-color: lightblue; width: 300px; height: 400px;}"),
          tags$style(".shiny-input-panel {background-color: lightblue; }")
        ),
        sidebar = bslib::sidebar(
          id = ns('DataId_Sidebar'),
          timeline_process_ui(ns('DataId_timeline')),
          hr(style = "border-top: 3px solid #000000;"),
          uiOutput(ns("DataId_btn_validate_ui")),
          inputPanel(
            uiOutput(ns('DataId_datasetId_ui')),
            uiOutput(ns("DataId_parentProteinID_ui"))
          ),
          position = "left",
          padding = c(100, 0),
          style = "z-index: 0;"
        ),
        uiOutput(ns('helpTextDataID')),
        uiOutput(ns('DataId_warningNonUniqueID_ui')),
        uiOutput(ns("DataId_show_previewdatasetID_ui")),
        uiOutput(ns("DataId_previewdatasetID_ui")),
        uiOutput(ns("DataId_show_previewProteinID_ui"))
      )
    })
    
    
    ## ID definition -----
    output$helpTextDataID <- renderUI({
      req(rv.widgets$SelectFile_typeOfData)
      
      t <- switch(rv.widgets$SelectFile_typeOfData,
        protein = "proteins",
        peptide = "peptides",
        default = "")
      txt <- paste("Please select among the columns of your data the one that
                corresponds to a unique ID of the ", t, ".", sep = "")
      
      helpText(txt)
    })
    
    MagellanNTK::mod_popover_for_help_server("help_convertIdType",
      title = "ID definition",
      content = "If AutoID is choosen, Prostar will build an index.")
    
    output$DataId_datasetId_ui <- renderUI({
      req(rv.custom$tab)
      
      #.choices <- setNames(nm = c("AutoID", colnames(rv.custom$tab)))
      #names(.choices) <- c("Auto ID", colnames(rv.custom$tab))
      
      widget <- selectInput(ns("DataId_datasetId"), 
        label = MagellanNTK::mod_popover_for_help_ui(ns("help_convertIdType")), 
        choices = setNames(nm = c("", "AutoID", colnames(rv.custom$tab))),
        selected = rv.widgets$DataId_datasetId,
        width = '300px'
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['DataId'] )
    })
    
    datasetID_Ok <- reactive({
      req(rv.widgets$DataId_datasetId)
      req(rv.custom$tab)
      if (rv.widgets$DataId_datasetId == "AutoID") {
        t <- TRUE
      } else {
        t <- (length(as.data.frame(rv.custom$tab)[, rv.widgets$DataId_datasetId])
          == length(unique(as.data.frame(rv.custom$tab)[, rv.widgets$DataId_datasetId])))
      }
      t
    })
    
    previewdatasetID <- reactive({
      req(rv.widgets$DataId_datasetId)
      
      if (rv.widgets$DataId_datasetId == "AutoID"){data.frame(AutoID = 1:6)} else {
        head(rv.custom$tab[, rv.widgets$DataId_datasetId, drop = FALSE])}
    })
    
    output$DataId_show_previewdatasetID_ui <- renderUI({
      req(rv.widgets$DataId_datasetId)
      widget <- checkboxInput(ns("DataId_show_previewdatasetID"), "Show preview", value = rv.widgets$DataId_show_previewdatasetID)
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['DataId'])
    })
    
    MagellanNTK::format_DT_server('DT_previewdatasetID',
      reactive({previewdatasetID()}))
    
    output$DataId_previewdatasetID_ui <- renderUI({
      req(rv.widgets$DataId_datasetId)
      req(rv.widgets$DataId_show_previewdatasetID)
      
      MagellanNTK::format_DT_ui(ns('DT_previewdatasetID'))
    })
    
    output$DataId_warningNonUniqueID_ui <- renderUI({
      # req(rv.widgets$DataId_datasetId != "AutoID")
      # req(rv.custom$tab)
      # 
      # df <- as.data.frame(rv.custom$tab)
      # t <- (length(df[, rv.widgets$DataId_datasetId]) == length(unique(df[, rv.widgets$DataId_datasetId])))
      # 
      if (!datasetID_Ok()) {
        text <- "<img src=\"images/Problem.png\" height=\"24\"></img><font color=\"red\">
        Warning ! Your ID contains duplicate data. Please choose another one.</font>"
      } else {
        text <- "<img src=\"images/Ok.png\" height=\"24\"></img>"
      }
      HTML(text)
    })
    
    
    ## Protein ID definition -----
    MagellanNTK::mod_popover_for_help_server("help_ProteinId",
      title = "Select protein IDs",
      content = "Select the column containing the parent protein IDs."
    )
    
    output$DataId_parentProteinID_ui <- renderUI({
      req(rv.custom$tab)
      req(rv.widgets$SelectFile_typeOfData != "protein")
      
      widget <- selectInput(ns("DataId_parentProteinID"), 
        MagellanNTK::mod_popover_for_help_ui(ns("help_ProteinId")), 
        choices = setNames(nm = c("", colnames(rv.custom$tab))),
        selected = rv.widgets$DataId_parentProteinID,
        width = '300px'
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['DataId'] )
    })
    
    previewProtID <- reactive({
      req(rv.widgets$DataId_parentProteinID)
      
      head(rv.custom$tab[, rv.widgets$DataId_parentProteinID, drop = FALSE])
    })
    
    output$DataId_show_previewProteinID_ui <- renderUI({
      req(rv.widgets$DataId_parentProteinID)
      widget <- checkboxInput(ns("DataId_show_previewProteinID"), "Show preview", value = rv.widgets$DataId_show_previewProteinID)
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['DataId'])
    })
    
    MagellanNTK::format_DT_server('DT_previewProtID',
      reactive({previewProtID()}))
    
    output$DataId_previewProteinID_ui <- renderUI({
      req(rv.widgets$DataId_parentProteinID)
      req(rv.widgets$DataId_show_previewProteinID)
      
      MagellanNTK::format_DT_ui(ns('DT_previewProtID'))
    })
    
    
    ## Validation button -----
    loader_inline_DataId <- spsComps::addLoader$new("DataId_btn_validate", color = "blue", method = "inline", type = "spinner")
    
    output$DataId_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("DataId_btn_validate"), "Perform",
        class = "btn-success")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['DataId'] && datasetID_Ok())
    })
    
    observeEvent(input$DataId_btn_validate, {
      # Do some stuff
      # new.dataset <- 10*rv$dataIn[[length(rv$dataIn)]]
      # rv$dataIn <- Add_Datasets_to_Object(object = rv$dataIn,
      #                                     dataset = new.dataset,
      #                                     name = paste0('temp_',id))
      req(rv.widgets$DataId_datasetId)
      if(rv.widgets$SelectFile_typeOfData != "protein"){
        req(rv.widgets$DataId_parentProteinID)}
      loader_inline_DataId$show()
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status['DataId'] <- stepStatus$VALIDATED
      loader_inline_DataId$hide()
    })
    
    # <<< END ------------- Code for step 2 UI---------------
    
    
    # >>> START ------------- Code for step 3 UI---------------
    
    output$ExpandFeatData <- renderUI({
      
      
      bslib::layout_sidebar(
        tags$head(tags$style(".sidebar-content {background-color: lightblue; width: 300px; height: 400px;}"),
          tags$style(".shiny-input-panel {background-color: lightblue; }")
          #tags$head(tags$style(HTML("#sidebar_id { overflow: hidden;}"))),
        ),
        sidebar = bslib::sidebar(
          id = ns('ExpandFeatData_Sidebar'),
          timeline_process_ui(ns('ExpandFeatData_timeline')),
          hr(style = "border-top: 3px solid #000000;"),
          uiOutput(ns("ExpandFeatData_btn_validate_ui")),
          inputPanel(
            uiOutput(ns("ExpandFeatData_quantCols_ui"), style = "margin-right: 30px;"),
            uiOutput(ns('ExpandFeatData_idMethod_ui'), style = "margin-right: 30px;"),
            uiOutput(ns("ExpandFeatData_inputGroup_ui"))
          ),
          position = "left",
          padding = c(100, 0),
          style = "z-index: 0;"
        ),
        uiOutput(ns("ExpandFeatData_warningNegValues_ui")),
        uiOutput(ns("ExpandFeatData_warningNonNum_ui"))
      )
    })
    
    
    ## Select data columns -----
    MagellanNTK::mod_popover_for_help_server("help_ExpandFeatData_quantCols",
      title = "Select quantification columns",
      content = HTML(
        paste0(
          "Select the columns that are quantitation values by clicking in the field below.", 
          "<br>",
          "Hold MAJ or CTRL while clicking to select multiple columns."
        ))
    )
    
    output$ExpandFeatData_quantCols_ui <- renderUI({
      req(rv.custom$tab)
      widget <- selectInput(ns("ExpandFeatData_quantCols"),
        MagellanNTK::mod_popover_for_help_ui(ns("help_ExpandFeatData_quantCols")),
        choices = setNames(nm=colnames(rv.custom$tab)),
        multiple = TRUE, selectize = FALSE ,
        width = "300px", size = 20,
        selected = rv.widgets$ExpandFeatData_quantCols
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['ExpandFeatData'] )
    })
    
    output$ExpandFeatData_warningNegValues_ui <- renderUI({
      req(rv.widgets$SelectFile_checkDataLogged == "no")
      req(rv.widgets$ExpandFeatData_quantCols)
      req(length(which(rv.custom$tab[, rv.widgets$ExpandFeatData_quantCols] < 0)) > 0)
      
      HTML("<img src=\"images/Problem.png\" height=\"24\"></img><font color=\"red\">
            Warning ! Your original dataset may contain negative values, which cannot be log-transformed. <br>
            Please check your dataset or the log option in the first tab.</font>")
    })
    
    output$ExpandFeatData_warningNonNum_ui <- renderUI({
      req(rv.custom$tab)
      req(rv.widgets$ExpandFeatData_quantCols)
      req(!all(sapply(rv.custom$tab[, rv.widgets$ExpandFeatData_quantCols, drop = FALSE],
        is.numeric)))
      
      HTML("<img src=\"images/Problem.png\" height=\"24\"></img><font color=\"red\">
            Warning ! At least one of the selected column contains non-numerical data.</font>")
    })
    
    ## Identification method -----
    MagellanNTK::mod_popover_for_help_server("help_ExpandFeatData_idMethod",
      title = "Provide identification method",
      content = "...")
    
    output$ExpandFeatData_idMethod_ui <- renderUI({
      req(rv.widgets$ExpandFeatData_quantCols)
      
      widget <- radioButtons(ns("ExpandFeatData_idMethod"), 
        MagellanNTK::mod_popover_for_help_ui(ns("help_ExpandFeatData_idMethod")),
        choices = list(
          "No (default values will be computed)" = FALSE,
          "Yes" = TRUE),
        selected = rv.widgets$ExpandFeatData_idMethod)
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['ExpandFeatData'])
    })
    
    
    observe({
      rv.widgets$ExpandFeatData_inputGroup <- Prostar2::mod_inputGroup_server('inputGroup',
        df = reactive({rv.custom$tab}),
        quantCols = reactive({rv.widgets$ExpandFeatData_quantCols}),
        is.enabled = reactive({rv$steps.enabled['ExpandFeatData']}))
    })
    
    output$ExpandFeatData_inputGroup_ui <- renderUI({
      req(rv.widgets$ExpandFeatData_quantCols)
      req(as.logical(rv.widgets$ExpandFeatData_idMethod))
      
      rv.widgets$ExpandFeatData_quantCols
      mod_inputGroup_ui(ns('inputGroup'))
    })
    
    
    ## Validation button -----
    loader_inline_ExpandFeatData <- spsComps::addLoader$new("ExpandFeatData_btn_validate", color = "blue", method = "inline", type = "spinner")
    
    output$ExpandFeatData_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("ExpandFeatData_btn_validate"), "Perform",
        class = "btn-success")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['ExpandFeatData'])
    })
    
    observeEvent(input$ExpandFeatData_btn_validate, {
      req(rv.widgets$ExpandFeatData_quantCols)
      req(all(sapply(rv.custom$tab[, rv.widgets$ExpandFeatData_quantCols, drop = FALSE],
        is.numeric)))
      if (as.logical(rv.widgets$ExpandFeatData_idMethod)){req(rv.widgets$ExpandFeatData_inputGroup())}
      loader_inline_ExpandFeatData$show()
      # Do some stuff
      # new.dataset <- 10*rv$dataIn[[length(rv$dataIn)]]
      # rv$dataIn <- Add_Datasets_to_Object(object = rv$dataIn,
      #                                     dataset = new.dataset,
      #                                     name = paste0('temp_',id))
      # 
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status['ExpandFeatData'] <- stepStatus$VALIDATED
      loader_inline_ExpandFeatData$hide()
    })
    
    # <<< END ------------- Code for for step 3 UI---------------
    
    
    # >>> START ------------- Code for step 4 UI---------------
    
    output$Design <- renderUI({
      
      
      bslib::layout_sidebar(
        tags$head(tags$style(".sidebar-content {background-color: lightblue; width: 300px; height: 400px;}"),
          tags$style(".shiny-input-panel {background-color: lightblue; }")
          #tags$head(tags$style(HTML("#sidebar_id { overflow: hidden;}"))),
        ),
        sidebar = bslib::sidebar(
          id = ns('Design_Sidebar'),
          timeline_process_ui(ns('Design_timeline')),
          hr(style = "border-top: 3px solid #000000;"),
          uiOutput(ns("Design_btn_validate_ui")),
          inputPanel(
            uiOutput(ns("Design_designEx_ui"))
          ),
          position = "left",
          padding = c(100, 0),
          style = "z-index: 0;"
        ),
        uiOutput(ns("Design_designEx_ui"))
      )
    })
    
    
    ## Experiment design -----
    output$Design_designEx_ui <- renderUI({
      req(rv.widgets$ExpandFeatData_quantCols)
      rv.widgets$ExpandFeatData_quantCols
      
      rv.custom$design <- Prostar2::mod_buildDesign_server(
        "designEx", 
        quantCols = rv.widgets$ExpandFeatData_quantCols,
        is.enabled = reactive({rv$steps.enabled['Design']})
      )
      
      mod_buildDesign_ui(ns("designEx"))
    })
    
    
    ## Validation button -----
    loader_inline_Design <- spsComps::addLoader$new("Design_btn_validate", color = "blue", method = "inline", type = "spinner")
    
    output$Design_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Design_btn_validate"), "Validate design",
        class = "btn-success")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Design'])
    })
    
    observeEvent(input$Design_btn_validate, {
      req(rv.custom$design()$trigger)
      loader_inline_Design$show()
      # Do some stuff
      # new.dataset <- 10*rv$dataIn[[length(rv$dataIn)]]
      # rv$dataIn <- Add_Datasets_to_Object(object = rv$dataIn,
      #                                     dataset = new.dataset,
      #                                     name = paste0('temp_',id))
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status['Design'] <- stepStatus$VALIDATED
      loader_inline_Design$hide()
    })
    
    # <<< END ------------- Code for for step 4 UI---------------
    
    
    # >>> START ------------- Code for Save UI---------------
    
    output$Save <- renderUI({
      bslib::layout_sidebar(
        tags$head(tags$style(".sidebar-content {background-color: lightblue; width: 300px;}"),
          tags$style(".shiny-input-panel {background-color: lightblue;}")
        ),
        sidebar = bslib::sidebar(
          id = ns('Save_Sidebar'),
          timeline_process_ui(ns('Save_timeline')),
          tags$style(".shiny-input-panel {background-color: lightblue;}"),
          hr(style = "border-top: 3px solid #000000;"),
          uiOutput(ns('Save_btn_validate_ui')),
          inputPanel(
            
            uiOutput(ns('Save_infos_ui'))
          ),
          position = "left",
          padding = c(100, 0),
          style = "z-index: 0;"
        ),
         uiOutput(ns('Save_mod_dl_ui'))
      )
    })
    
    ## Name and description -----
    output$Save_infos_ui <- renderUI({
      MagellanNTK::toggleWidget(
        tagList(
          textInput(ns('Save_analysis'), 'Name of the analysis', 
            placeholder = 'Name of the analysis', width = "400px"),
          textAreaInput(ns('Save_description'), 'Description of the analysis', 
            placeholder = 'Description of the analysis', height = '150px', width = "400px")
        ),
        rv$steps.enabled['Save']
      )
    })
    
    ## Save dataset -----
    output$Save_mod_dl_ui <- renderUI({
      req(config@mode == 'process')
      req(rv$steps.status['Save'] == stepStatus$VALIDATED)
      
      fluidPage(
        wellPanel(
          MagellanNTK::download_dataset_ui(ns('createQuickLink'))
        )
      )
    })    
    
    ## Validation button -----
    loader_inline_Save <- spsComps::addLoader$new("Save_btn_validate", color = "blue", method = "inline", type = "spinner")
    
    output$Save_btn_validate_ui <- renderUI({
      MagellanNTK::toggleWidget(actionButton(ns("Save_btn_validate"), 
        "Create QFeatures dataset", 
        class = "btn-success"),
        rv$steps.enabled['Save']
      )
    })
    
    
    observeEvent(input$Save_btn_validate, {
      
      loader_inline_Save$show()
      # Check if the conditions have been reordered or not.
      # If it is the case, the metacells must also be reordered
      # in the same way.
      #rv.custom$design
      
      # Reorder columns before creating QFeatures object
      # print(rv.custom$design()$design)
      # print(rv.custom$design()$order)
      # as.data.frame(rv.custom$design()$design)
      
      if (input$Save_analysis != ""){
        analysis_name <- rv.widgets$Save_analysis
      }else{
        analysis_name <- "myDataset"
      }
      
      .indexForMetacell <- rv.widgets$ExpandFeatData_inputGroup()[rv.custom$design()$order]
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
        logData = rv.widgets$SelectFile_checkDataLogged == 'no',
        indexForMetacell = .indexForMetacell,
        typeDataset = rv.widgets$SelectFile_typeOfData,
        parentProtId = rv.widgets$DataId_parentProteinID,
        force.na = rv.widgets$SelectFile_replaceAllZeros,
        software = rv.widgets$SelectFile_software)
      #browser()
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- list(data = rv$dataIn, name = rv.custom$name)
      rv$steps.status['Save'] <- stepStatus$VALIDATED
      
      Prostar2::download_dataset_server('createQuickLink', 
        dataIn = reactive({rv$dataIn}),
        filename = analysis_name)
      
      loader_inline_Save$hide()
      print("fini")
    })
    
    # <<< END ------------- Code for Save UI---------------
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  })
}
