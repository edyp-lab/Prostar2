

#' @title xxx
#' @description xxx
#' @name mod_convert
#' @author Samuel Wieczorek 
#' @examples
#' \dontrun{
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
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs disabled
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
  
  requireNamespace("openxlsx")
  
  
  
  widgets.default.values <- list(
    SelectFile_software = '',
    SelectFile_file = NULL,
    SelectFile_typeOfData = NULL,
    SelectFile_checkDataLogged = "no",
    SelectFile_replaceAllZeros = TRUE,
    SelectFile_XLSsheets = NULL,
    
    DataId_datasetId = NULL,
    DataId_parentProteinID = NULL,
    
    ExpandFeatData_idMethod = NULL,
    ExpandFeatData_quantCols = NULL,
    ExpandFeatData_inputGroup = NULL,
    
    Save_analysis = NULL,
    Save_description = NULL
  )
  
  rv.custom.default.values <- list(
    tab = NULL,
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
    
    # #Define local reactive variables
    rv.convert <- reactiveValues(
      tab = NULL,
      design = NULL,
      inputGroup = NULL
    )
    
    # >>>
    # >>> START ------------- Code for Description UI---------------
    # >>> 
    
    
    output$Description <- renderUI({
      file <- normalizePath(file.path(session$userData$workflow.path, 
        'md', paste0(id, '.md')))
      
      
      tagList(
        # In this example, the md file is found in the extdata/module_examples directory
        # but with a real app, it should be provided by the package which
        # contains the UI for the different steps of the process module.
        uiOutput(ns('Description_btn_validate_ui')),
        
        # Used to show some information about the dataset which is loaded
        # This function must be provided by the package of the process module
        uiOutput(ns('datasetDescription_ui')),
        
        if (file.exists(file))
          includeMarkdown(file)
        else
          p('No Description available')
      )
    })
    
    output$datasetDescription_ui <- renderUI({
      # Insert your own code to visualize some information
      # about your dataset. It will appear once the 'Start' button
      # has been clicked
      
    })
    
    output$Description_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Description_btn_validate"), "Start",
        class = 'info')
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Description'])
    })
    
    
    observeEvent(input$Description_btn_validate, {
      rv$dataIn <- dataIn()
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- stepStatus$VALIDATED
    })
    
    
    # >>>
    # >>> START ------------- Code for step 1 UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI ------------------------------------
    output$SelectFile <- renderUI({
      fluidPage(
        wellPanel(
          # uiOutput for all widgets in this UI
          # This part is mandatory
          # The renderUI() function of each widget is managed by MagellanNTK
          # The dev only have to define a reactive() function for each
          # widget he want to insert
          # Be aware of the naming convention for ids in uiOutput()
          # For more details, please refer to the dev document.
          div(style="display: flex; flex-wrap: wrap;",
              uiOutput(ns('SelectFile_software_ui'), style = "margin-right : 30px;"),
              uiOutput(ns('SelectFile_file_ui'), style = "margin-right : 30px;"),
              uiOutput(ns('SelectFile_ManageXlsFiles_ui'))),
          tags$hr(),
          div(style="display: flex; flex-wrap: wrap;",
              uiOutput(ns('SelectFile_typeOfData_ui'), style = "margin-right : 30px;"),
              uiOutput(ns('SelectFile_checkDataLogged_ui'), style = "margin-right : 30px;"),
              uiOutput(ns('SelectFile_replaceAllZeros_ui'))),
          
          # Insert validation button
          uiOutput(ns('SelectFile_btn_validate_ui')),
        )
      )
    })
    
    
    # >>> START: Definition of the widgets
    
    ## File selection
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
    
    ############ Read text file to be imported ######################
    # observeEvent(req(rv.widgets$SelectFile_file), {
    #   rv.widgets$SelectFile_XLSsheets
    #   
    #   ext <- GetExtension(rv.widgets$SelectFile_file$name)
    #   rv.custom$name <- unlist(strsplit(rv.widgets$SelectFile_file$name, 
    #     split='.', fixed = TRUE))[1]
    #   if (((ext %in% c("xls", "xlsx"))) && 
    #       is.null(rv.widgets$SelectFile_XLSsheets))
    #     return(NULL)
    #   
    #   
    #   authorizedExts <- c("txt", "csv", "tsv", "xls", "xlsx")
    #   
    #   if (!fileExt.ok()) {
    #     shinyjs::info("Warning : this file is not a text nor an Excel file !
    #  Please choose another one.")
    #   } else {
    #     tryCatch({
    # 
    #       shinyjs::disable("SelectFile_file")
    #       f.path <- rv.widgets$SelectFile_file$datapath
    #       rv.convert$tab <- switch(ext,
    #                                txt = read.csv(f.path, header = TRUE, sep = "\t", as.is = T),
    #                                csv = read.csv(f.path, header = TRUE, sep = ";", as.is = T),
    #                                tsv = read.csv(f.path, header = TRUE, sep = "\t", as.is = T),
    #                                xls = DaparToolshed::readExcel(f.path, sheet = rv.widgets$SelectFile_XLSsheets),
    #                                xlsx = DaparToolshed::readExcel(f.path, sheet = rv.widgets$SelectFile_XLSsheets)
    #       )
    #       
    #       colnames(rv.convert$tab) <- gsub(".", "_", colnames(rv.convert$tab), fixed = TRUE)
    #       colnames(rv.convert$tab) <- gsub(" ", "_", colnames(rv.convert$tab), fixed = TRUE)
    #     },
    #     warning = function(w) {
    #       shinyjs::info(conditionMessage(w))
    #       return(NULL)
    #     },
    #     error = function(e) {
    #       shinyjs::info(conditionMessage(e))
    #       return(NULL)
    #     },
    #     finally = {
    #       # cleanup-code
    #     })
    #   }
    # })
    
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
    
    
    ## File options
    MagellanNTK::mod_popover_for_help_server(
      "help_typeOfData",
      title = "Type of dataset",
      content = "Is it a peptide or protein dataset ?"
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
    
    output$SelectFile_btn_validate_ui <- renderUI({
      widget <-  actionButton(ns("SelectFile_btn_validate"), "Perform",
        class = 'info')
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['SelectFile'] )
      
    })
    # >>> END: Definition of the widgets
    
    
    observeEvent(input$SelectFile_btn_validate, {
      # Do some stuff
      
      req(rv.widgets$SelectFile_file)
      
      ext <- GetExtension(rv.widgets$SelectFile_file$name)
      rv.custom$name <- unlist(strsplit(rv.widgets$SelectFile_file$name, 
        split='.', fixed = TRUE))[1]
      if (((ext %in% c("xls", "xlsx"))) && (
        is.null(rv.widgets$SelectFile_XLSsheets) ||
          nchar(rv.widgets$SelectFile_XLSsheets) == 0))
        return(NULL)
      
      authorizedExts <- c("txt", "csv", "tsv", "xls", "xlsx")
      
      if (!fileExt.ok()) {
        shinyjs::info("Warning : this file is not a text nor an Excel file !
     Please choose another one.")
      } else {
        tryCatch({
          
          shinyjs::disable("SelectFile_file")
          f.path <- rv.widgets$SelectFile_file$datapath
          rv.convert$tab <- switch(ext,
            txt = read.csv(f.path, header = TRUE, sep = "\t", as.is = T),
            csv = read.csv(f.path, header = TRUE, sep = ";", as.is = T),
            tsv = read.csv(f.path, header = TRUE, sep = "\t", as.is = T),
            xls = DaparToolshed::readExcel(f.path, sheet = rv.widgets$SelectFile_XLSsheets),
            xlsx = DaparToolshed::readExcel(f.path, sheet = rv.widgets$SelectFile_XLSsheets)
          )
          
          colnames(rv.convert$tab) <- gsub(".", "_", colnames(rv.convert$tab), fixed = TRUE)
          colnames(rv.convert$tab) <- gsub(" ", "_", colnames(rv.convert$tab), fixed = TRUE)
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
    })
    
    # <<< END ------------- Code for step 1 UI---------------
    
    
    
    
    # >>> START ------------- Code for step 2 UI---------------
    
    output$DataId <- renderUI({
      fluidPage(
        wellPanel(
          uiOutput(ns('helpTextDataID')),
          # Definition of which ID to use
          uiOutput(ns('DataId_datasetId_ui')),
          uiOutput(ns('DataId_warningNonUniqueID_ui')),
          tags$hr(),
          # Definition of Protein ID if peptide dataset
          div(style="display: flex; flex-wrap: wrap;",
              uiOutput(ns("DataId_parentProteinId_ui"), style = "margin-right: 20px;"),
              uiOutput(ns("DataId_previewProteinID_ui"))),
          # Insert validation button
          # This line is necessary. DO NOT MODIFY
          uiOutput(ns('DataId_btn_validate_ui'))
        )
      )
    })
    
    
    ## Definition of which ID to use
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
      req(rv.convert$tab)
      
      #.choices <- setNames(nm = c("AutoID", colnames(rv.convert$tab)))
      #names(.choices) <- c("Auto ID", colnames(rv.convert$tab))
      
      widget <- selectInput(ns("DataId_datasetId"), 
        label = MagellanNTK::mod_popover_for_help_ui(ns("help_convertIdType")), 
        choices = setNames(nm = c("", "AutoID", colnames(rv.convert$tab))),
        selected = rv.widgets$DataId_datasetId,
        width = '300px'
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['DataId'] )
    })
    
    datasetID_Ok <- reactive({
      req(rv.widgets$DataId_datasetId)
      req(rv.convert$tab)
      if (rv.widgets$DataId_datasetId == "AutoID") {
        t <- TRUE
      } else {
        t <- (length(as.data.frame(rv.convert$tab)[, rv.widgets$DataId_datasetId])
          == length(unique(as.data.frame(rv.convert$tab)[, rv.widgets$DataId_datasetId])))
      }
      t
    })
    
    output$DataId_warningNonUniqueID_ui <- renderUI({
      # req(rv.widgets$DataId_datasetId != "AutoID")
      # req(rv.convert$tab)
      # 
      # df <- as.data.frame(rv.convert$tab)
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
    
    
    ## Definition of Protein ID if peptide dataset
    MagellanNTK::mod_popover_for_help_server("help_ProteinId",
        title = "Select protein IDs",
        content = "Select the column containing the parent protein IDs."
      )
    
    output$DataId_parentProteinId_ui <- renderUI({
      req(rv.convert$tab)
      req(rv.widgets$SelectFile_typeOfData != "protein")

      widget <- selectInput(ns("DataId_parentProteinId"), 
                            MagellanNTK::mod_popover_for_help_ui(ns("help_ProteinId")), 
                            choices = setNames(nm = c("", colnames(rv.convert$tab))),
                            selected = rv.widgets$DataId_parentProteinId,
                            width = '300px'
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['DataId'] )
    })
    
    observeEvent(input$DataId_parentProteinId, {
      rv.widgets$DataId_parentProteinId <- input$DataId_parentProteinId
    })
    
    previewProtID <- reactive({
      req(rv.widgets$DataId_parentProteinId)
      
      head(rv.convert$tab[, rv.widgets$DataId_parentProteinId, drop = FALSE])
    })
    
    MagellanNTK::format_DT_server('DT_previewProtID',
                                  reactive({previewProtID()}))
    
    output$DataId_previewProteinID_ui <- renderUI({
      req(rv.widgets$DataId_parentProteinId)
      
      tagList(
        p(style = "color: black; font-weight: bold;", "Preview :"),
        MagellanNTK::format_DT_ui(ns('DT_previewProtID')))
    })
    
    
    output$DataId_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("DataId_btn_validate"), "Perform",
        class = 'info')
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
        req(rv.widgets$DataId_parentProteinId)}
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status['DataId'] <- stepStatus$VALIDATED
    })
    
    # <<< END ------------- Code for step 2 UI---------------
    
    
    # >>> START ------------- Code for step 3 UI---------------
    
    output$ExpandFeatData <- renderUI({
      wellPanel(
        shinyjs::useShinyjs(),
        fluidRow(
          column(width = 4, uiOutput(ns('ExpandFeatData_idMethod_ui'))),
          #column(width = 4, uiOutput(ns("ExpandFeatData_checkIdTab_ui")),
          column(width = 4, shinyjs::hidden(
            div(id = "warning_neg_values",
              p("Warning : Your original dataset may contain negative values",
                "so that they cannot be logged. Please check back the dataset or",
                "the log option in the first tab.")
            )
          ))
        ),
        fluidRow(
          column(width = 4, uiOutput(ns("ExpandFeatData_quantCols_ui"), width = "400px")),
          column(width = 8, shinyjs::hidden(
            uiOutput(ns("ExpandFeatData_inputGroup_ui")
              #, width = "600px"
            )
          ))
        ),
        
        # Insert validation button
        # This line is necessary. DO NOT MODIFY
        uiOutput(ns('ExpandFeatData_btn_validate_ui'))
      )
    })
    
    
    output$ExpandFeatData_idMethod_ui <- renderUI({
      widget <- radioButtons(ns("ExpandFeatData_idMethod"), 
        "Provide identification method",
        choices = list(
          "No (default values will be computed)" = FALSE,
          "Yes" = TRUE),
        selected = rv.widgets$ExpandFeatData_idMethod)
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['ExpandFeatData'] )
    })
    
    
    observe({
    rv.widgets$ExpandFeatData_inputGroup <- Prostar2::mod_inputGroup_server('inputGroup',
      df = reactive({rv.convert$tab}),
      quantCols = reactive({rv.widgets$ExpandFeatData_quantCols})
        )
  })
  
    output$ExpandFeatData_inputGroup_ui <- renderUI({
      req(as.logical(rv.widgets$ExpandFeatData_idMethod))
      rv.widgets$ExpandFeatData_quantCols
      
      
      mod_inputGroup_ui(ns('inputGroup'))
    })
    
    output$ExpandFeatData_quantCols_ui <- renderUI({
      req(rv.convert$tab)
      
      MagellanNTK::mod_popover_for_help_server("help_ExpandFeatData_quantCols",
        title = "Quantitative data",
        content = "Select the columns that are quantitation values
            by clicking in the field below.")
      
      tagList(
        MagellanNTK::mod_popover_for_help_ui(ns("help_ExpandFeatData_quantCols")),
        widget <- selectInput(ns("ExpandFeatData_quantCols"),
          label = "Select columns of quantification",
          choices = setNames(nm=colnames(rv.convert$tab)),
          multiple = TRUE, selectize = FALSE ,
          width = "200px", size = 20,
          selected = rv.widgets$ExpandFeatData_quantCols
        )
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['ExpandFeatData'] )
    })
    
    
    
    
    observe({
      shinyjs::toggle("warning_neg_values",
        condition = !is.null(rv.widgets$ExpandFeatData_quantCols) &&
          length(which(rv.convert$tab[, rv.widgets$ExpandFeatData_quantCols] < 0)) > 0
      )
      shinyjs::toggle("ExpandFeatData_idMethod", condition = !is.null(rv.widgets$ExpandFeatData_quantCols))
      shinyjs::toggle("ExpandFeatData_inputGroup_ui", condition = as.logical(rv.widgets$idMethod) == TRUE)
    })
    
    
    
    output$ExpandFeatData_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("ExpandFeatData_btn_validate"),
        "Perform",
        class = 'info')
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['ExpandFeatData'] )
    })
    
    observeEvent(input$ExpandFeatData_btn_validate, {
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
    })
    
    # <<< END ------------- Code for Design UI---------------
    
    
    ############# STEP 4 ######################
    output$Design <- renderUI({
      
      rv.convert$design <- Prostar2::mod_buildDesign_server(
        "designEx", 
        quantCols = rv.widgets$ExpandFeatData_quantCols,
        is.enabled = reactive({rv$steps.enabled['Design']})
        )
      
      wellPanel(
        mod_buildDesign_ui(ns("designEx")),
        uiOutput(ns('Design_btn_validate_ui'))
        
      )
    })
    
    
    output$Design_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Design_btn_validate"), "Validate design",
        class = 'info')
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Design'] )
    })
    
    observeEvent(input$Design_btn_validate, {
      
      req(rv.convert$design)
      # Do some stuff
      # new.dataset <- 10*rv$dataIn[[length(rv$dataIn)]]
      # rv$dataIn <- Add_Datasets_to_Object(object = rv$dataIn,
      #                                     dataset = new.dataset,
      #                                     name = paste0('temp_',id))
      # 
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status['Design'] <- stepStatus$VALIDATED
    })
    
    # >>> START ------------- Code for Save UI---------------
    output$Save <- renderUI({
      tagList(
        # Insert validation button
        # This line is necessary. DO NOT MODIFY
        uiOutput(ns('Save_infos_ui')),
        uiOutput(ns('Save_btn_validate_ui')),
        uiOutput(ns('mod_dl_ui'))
      )
    })
    
    output$mod_dl_ui <- renderUI({
      req(config@mode == 'process')
      req(rv$steps.status['Save'] == stepStatus$VALIDATED)
      MagellanNTK::download_dataset_ui(ns('createQuickLink'))
    })
    
    
    output$Save_infos_ui <- renderUI({
      MagellanNTK::toggleWidget(
        tagList(
          textInput(ns('Save_analysis'), '', placeholder = 'Name of the analysis'),
          textAreaInput(ns('Save_description'), '', placeholder = 'Description of the analysis', height = '150px')
        ),
        rv$steps.enabled['Save']
      )
    })
    
    
    output$Save_btn_validate_ui <- renderUI({
      MagellanNTK::toggleWidget(actionButton(ns("Save_btn_validate"), 
        "Create QFeatures dataset", 
        class = 'info'),
        rv$steps.enabled['Save']
      )
    })
    
    
    observeEvent(input$Save_btn_validate, {
      
      
      # Check if the conditions have been reordered or not.
      # If it is the case, the metacells must also be reordered
      # in the same way.
      #rv.convert$design

      # Reorder colums before creatinf QFetatures object
      print(rv.convert$design()$design)
      print(rv.convert$design()$order)
      as.data.frame(rv.convert$design()$design)
      

      .indexForMetacell <- rv.widgets$ExpandFeatData_inputGroup()[rv.convert$design()$order]
      .indQData <- rv.widgets$ExpandFeatData_quantCols[rv.convert$design()$order]
      
      
      # Create QFeatures dataset file
      rv$dataIn <- DaparToolshed::createQFeatures(
        file = rv.widgets$SelectFile_file$name,
        data = rv.convert$tab, 
        sample = as.data.frame(rv.convert$design()$design),
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
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- list(data = rv$dataIn, name = rv.custom$name)
      rv$steps.status['Save'] <- stepStatus$VALIDATED
      
      Prostar2::download_dataset_server('createQuickLink', 
        dataIn = reactive({rv$dataIn}))
      
    })
    # <<< END ------------- Code for step 3 UI---------------
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  })
}

