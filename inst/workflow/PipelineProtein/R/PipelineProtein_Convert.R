

#' @title xxx
#' @description xxx
#' @name mod_convert
#' @author Samuel Wieczorek, Manon Gaudin
#' @examples
#' if (interactive()){
#' data("Exp1_R25_prot", package = "DaparToolshedData")
#' path <- system.file('workflow/PipelineProtein', package = 'Prostar2')
#' shiny::runApp(workflowApp("PipelineProtein_Convert", path, dataIn = Exp1_R25_prot))
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
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets source_shinyApp_files nav_process_ui nav_process_server source_wf_files Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut format_DT_ui format_DT_server Timestamp toggleWidget mod_popover_for_help_server mod_popover_for_help_ui
#' 
PipelineProtein_Convert_conf <- function(){
  # This list contains the basic configuration of the process
  MagellanNTK::Config(
    fullname = 'PipelineProtein_Convert',
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
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#'
#' @return NA
#'
PipelineProtein_Convert_ui <- function(id) {
  ns <- NS(id)
}




#' Convert Server Function
#'
#' @param id xxx
#' @param dataIn xxx
#' @param steps.enabled xxx
#' @param remoteReset A `logical(1)` which acts as a remote command to reset
#' the module to its default values. Default is FALSE.
#'
#' @importFrom shinyjs disabled info
#' @importFrom stats setNames
#' @importFrom utils read.csv
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#'
#' @export
#'
#' @rdname mod_convert
#'
#' @return NA
#'
PipelineProtein_Convert_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({TRUE}),
  remoteReset = reactive({NULL}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  btnEvents = reactive({NULL})
) {
  
  requireNamespace(c("openxlsx", "shinyalert"))
  
  widgets.default.values <- list(
    SelectFile_software = "DIA_NN",
    SelectFile_file = NULL,
    SelectFile_typeOfData = "protein",
    SelectFile_checkDataLogged = "no",
    SelectFile_replaceAllZeros = TRUE,
    SelectFile_XLSsheets = NULL,
    
    DataId_datasetId = "",
    DataId_parentProteinID = NULL,
    DataId_show_previewdatasetID = FALSE,
    DataId_show_previewProteinID = FALSE,
    
    ExpandFeatData_idMethod = FALSE,
    ExpandFeatData_quantCols = NULL,
    ExpandFeatData_inputGroup = reactive({NULL}),
    
    Save_analysis = "",
    Save_description = NULL
  )
  
  rv.custom.default.values <- list(
    tab = NULL,
    previewtab = NULL,
    design = NULL,
    name = NULL,
    history = MagellanNTK::InitializeHistory()
  )
  
  
  ### -------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ### -------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  
    core.code <- paste0(
      MagellanNTK::Get_Code_Declare_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_ObserveEvent_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_rv_reactiveValues(),
      MagellanNTK::Get_Code_Declare_rv_custom(names(rv.custom.default.values)),
      #MagellanNTK::Get_Code_for_Initialize_History(widgets.default.values),
      MagellanNTK::Get_Code_for_dataOut(),
      MagellanNTK::Get_Code_for_remoteReset(
        widgets = TRUE,
        custom = TRUE,
        dataIn = "NULL"
      ),
      sep = "\n"
    )
    
    
    core.code <- MagellanNTK::Get_Workflow_Core_Code(
      mode = 'process',
      name = id,
      w.names = names(widgets.default.values),
      rv.custom.names = names(rv.custom.default.values)
    )
    
    eval(str2expression(core.code))
    add_resourcePath()
    
    ###########################################################################-
    #
    #-----------------------------DESCRIPTION-----------------------------------
    #
    ###########################################################################-
    output$Description <- renderUI({
      file <- normalizePath(file.path(
        system.file('workflow', package = 'Prostar2'),
        unlist(strsplit(id, '_'))[1], 
        'md', 
        paste0(id, '.Rmd')))
      
      MagellanNTK::process_layout_process(session,
        ns = NS(id),
        sidebar = div(id = ns('div_process_layout_sidebar')),
        content = div(id = ns('div_content'),
          if (file.exists(file))
            includeMarkdown(file)
          else
            p('No Description available')
        )
      )
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Description', btnEvents()))
      #req(dataIn())
      
      shiny::withProgress(message = paste0("Description process", id), {
        shiny::incProgress(0.5)
        
        rv$dataIn <- dataIn()
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- rv$dataIn
        rv$steps.status['Description'] <- MagellanNTK::stepStatus$VALIDATED
      })
    })
    
    
    ###########################################################################-
    #
    #------------------------------SELECT FILE----------------------------------
    #
    ###########################################################################-
    output$SelectFile <- renderUI({
      shinyjs::useShinyjs()
      
      MagellanNTK::process_layout_process(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns('SelectFile_software_ui')),
          uiOutput(ns('SelectFile_typeOfData_ui')),
          uiOutput(ns('SelectFile_checkDataLogged_ui')),
          uiOutput(ns('SelectFile_replaceAllZeros_ui'))
        ),
        content = tagList(
          div(style = "margin-left: 10px; display: flex; gap: 20px;",
            uiOutput(ns('SelectFile_file_ui')),
            uiOutput(ns('SelectFile_ManageXlsFiles_ui'))),
          div(style = "margin-bottom: 5px;",
              uiOutput(ns('SelectFile_btn_previewfile_ui'))),
          uiOutput(ns('SelectFile_previewfile_ui'))
        )
      )
    })
    
    
    #### _sidebar -----
    output$SelectFile_software_ui <- renderUI({
      #req(rv.widgets$SelectFile_file$name)
      widget <- radioButtons(ns("SelectFile_software"), 
        "Data source",
        choices = setNames(nm = c("DIA_NN", "maxquant", "proline")),
        selected = rv.widgets$SelectFile_software)
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['SelectFile'] )
    })
    
    output$SelectFile_typeOfData_ui <- renderUI({
      widget <- radioButtons(ns("SelectFile_typeOfData"), 
                             "Type of dataset",
                             choices = c(#"peptide dataset" = "peptide",
                               "protein dataset" = "protein"
                             ),
                             selected = rv.widgets$SelectFile_typeOfData)
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['SelectFile'] )
    })
    
    output$SelectFile_checkDataLogged_ui <- renderUI({
      widget <- radioButtons(ns("SelectFile_checkDataLogged"), 
                             "Data already log-transformed",
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
    
    #### _content -----
    output$SelectFile_file_ui <- renderUI({
      #req(rv.widgets$SelectFile_software)
      widget <- fileInput(
        ns("SelectFile_file"), "Data file",
        multiple = FALSE,
        accept = c(".txt", ".tsv", ".csv", ".xls", ".xlsx"),
        width = "400px"
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['SelectFile'])
    })
    
    fileExt.ok <- reactive({
      req(rv.widgets$SelectFile_file$name)
      authorizedExts <- c("txt", "csv", "tsv", "xls", "xlsx")
      ext <- MagellanNTK::GetExtension(rv.widgets$SelectFile_file$name)
      !is.na(match(ext, authorizedExts))
    })
    
    output$SelectFile_ManageXlsFiles_ui <- renderUI({
      req(rv.widgets$SelectFile_file)
      req(MagellanNTK::GetExtension(rv.widgets$SelectFile_file$name) %in% c("xls", "xlsx"))
      
      sheets <- c(DaparToolshed::listSheets(rv.widgets$SelectFile_file$datapath))
      widget <- selectInput(ns("SelectFile_XLSsheets"), 
                            "Select sheet", 
                            choices = sheets, 
                            width = "200px",
                            selected = rv.widgets$SelectFile_XLSsheets)
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['SelectFile'])
    })
    
    output$SelectFile_btn_previewfile_ui <- renderUI({
      req(rv.widgets$SelectFile_file)
      widget <-  actionButton(ns("SelectFile_btn_previewfile"), "Preview file",
        class = 'info')
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['SelectFile'] )
    })
    
    observeEvent(rv.widgets$SelectFile_file, {
      rv.custom$previewtab <- NULL
    })
    
    observeEvent(input$SelectFile_btn_previewfile, {
      req(rv.widgets$SelectFile_file)
      
      ext <- MagellanNTK::GetExtension(rv.widgets$SelectFile_file$name)
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
      reactive({rv.custom$previewtab[seq_len(3),]}))
    
    output$SelectFile_previewfile_ui <- renderUI({
      req(rv.widgets$SelectFile_file)
      req(rv.custom$previewtab)
      
      #tagList(
        #p(style = "color: black; font-weight: bold;", "Preview :"),
        MagellanNTK::format_DT_ui(ns('DT_previewfile'))
      #)
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('SelectFile', btnEvents()))
      
      rv.widgets$SelectFile_XLSsheets <- input$SelectFile_XLSsheets
      if (is.null(rv.widgets$SelectFile_file) || is.null(rv.widgets$SelectFile_software) || 
          is.null(rv.widgets$SelectFile_typeOfData) || is.null(rv.widgets$SelectFile_checkDataLogged) ||
          is.null(rv.widgets$SelectFile_replaceAllZeros) || 
          (MagellanNTK::GetExtension(rv.widgets$SelectFile_file$name) %in% c("xls", "xlsx") && is.null(rv.widgets$SelectFile_XLSsheets))) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("SelectFile process", id), {
          shiny::incProgress(0.5)
          # Do some stuff
          req(rv.widgets$SelectFile_file)
          
          
          ext <- MagellanNTK::GetExtension(rv.widgets$SelectFile_file$name)
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
            
            
            # DO NOT MODIFY THE THREE FOLLOWING LINES
            dataOut$trigger <- MagellanNTK::Timestamp()
            dataOut$value <- NULL
            rv$steps.status['SelectFile'] <- MagellanNTK::stepStatus$VALIDATED
          }
        })
      }
    })
    
    
    ###########################################################################-
    #
    #--------------------------------DATA ID------------------------------------
    #
    ###########################################################################-
    output$DataId <- renderUI({
      shinyjs::useShinyjs()
      
      MagellanNTK::process_layout_process(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns('helpTextDataID'))
        ),
        content = tagList(
          div(style = "display: flex; margin-left: 10px;",
            uiOutput(ns('DataId_datasetId_ui')),
            uiOutput(ns('DataId_warningNonUniqueID_img_ui'))),
          uiOutput(ns('DataId_warningNonUniqueID_txt_ui')),
          
          uiOutput(ns("DataId_show_previewdatasetID_ui")),
          uiOutput(ns("DataId_previewdatasetID_ui")),
          
          uiOutput(ns("DataId_parentProteinID_ui")),
          uiOutput(ns("DataId_show_previewProteinID_ui"))
        )
      )
    })
    
    #### _sidebar -----
    output$helpTextDataID <- renderUI({
      req(rv.widgets$SelectFile_typeOfData)
      
      t <- switch(rv.widgets$SelectFile_typeOfData,
        protein = "proteins",
        peptide = "peptides",
        default = "")
      txt <- paste0("Please select the column in your dataset that contains the unique ", t, " IDs.")
      
      helpText(txt)
    })
    
    #### _content -----
    output$DataId_datasetId_ui <- renderUI({
      req(rv.widgets$SelectFile_typeOfData)
      req(rv.custom$tab)
      
      #.choices <- setNames(nm = c("AutoID", colnames(rv.custom$tab)))
      #names(.choices) <- c("Auto ID", colnames(rv.custom$tab))
      title <- switch(rv.widgets$SelectFile_typeOfData,
                      protein = "Proteins IDs",
                      peptide = "Peptides IDs",
                      default = "ID definition")
      
      widget <- selectInput(ns("DataId_datasetId"), 
                            label = title, 
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
    
    output$DataId_warningNonUniqueID_img_ui <- renderUI({
      # req(rv.widgets$DataId_datasetId != "AutoID")
      # req(rv.custom$tab)
      # 
      # df <- as.data.frame(rv.custom$tab)
      # t <- (length(df[, rv.widgets$DataId_datasetId]) == length(unique(df[, rv.widgets$DataId_datasetId])))
      # 
      if (!datasetID_Ok()) {
        img <- "images/Problem.png"
      } else {
        img <- "images/Ok.png"
      }
      div(style = "margin-top: 13px;", img(src = img, height = 25))
    })
    
    output$DataId_warningNonUniqueID_txt_ui <- renderUI({
      # req(rv.widgets$DataId_datasetId != "AutoID")
      # req(rv.custom$tab)
      # 
      # df <- as.data.frame(rv.custom$tab)
      # t <- (length(df[, rv.widgets$DataId_datasetId]) == length(unique(df[, rv.widgets$DataId_datasetId])))
      # 
      if (!datasetID_Ok()) {
        text <- "Warning: Your ID contains duplicate data. Please choose another one."
      } else {
        text <- NULL
      }
      p(style = "color: red;", text)
    })
    
    output$DataId_show_previewdatasetID_ui <- renderUI({
      req(rv.widgets$DataId_datasetId)
      widget <- checkboxInput(ns("DataId_show_previewdatasetID"), "Show preview", value = rv.widgets$DataId_show_previewdatasetID)
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['DataId'])
    })    
    
    previewdatasetID <- reactive({
      req(rv.widgets$DataId_datasetId)
      
      if (rv.widgets$DataId_datasetId == "AutoID"){
        data.frame(AutoID = seq_len(6))
      } else {
        head(rv.custom$tab[, rv.widgets$DataId_datasetId, drop = FALSE])
      }
    })

    MagellanNTK::format_DT_server('DT_previewdatasetID',
      reactive({previewdatasetID()}))
    
    output$DataId_previewdatasetID_ui <- renderUI({
      req(rv.widgets$DataId_datasetId)
      req(rv.widgets$DataId_show_previewdatasetID)
      
      MagellanNTK::format_DT_ui(ns('DT_previewdatasetID'))
    })
    
    
    output$DataId_parentProteinID_ui <- renderUI({
      req(rv.custom$tab)
      req(rv.widgets$SelectFile_typeOfData != "protein")
      
      widget <- selectInput(ns("DataId_parentProteinID"), 
        "Select protein IDs", 
        choices = setNames(nm = c("", colnames(rv.custom$tab))),
        selected = rv.widgets$DataId_parentProteinID,
        width = '300px'
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['DataId'] )
    })
    
    output$DataId_show_previewProteinID_ui <- renderUI({
      req(rv.widgets$DataId_parentProteinID)
      widget <- checkboxInput(ns("DataId_show_previewProteinID"), "Show preview", value = rv.widgets$DataId_show_previewProteinID)
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['DataId'])
    })    
    
    previewProtID <- reactive({
      req(rv.widgets$DataId_parentProteinID)
      
      head(rv.custom$tab[, rv.widgets$DataId_parentProteinID, drop = FALSE])
    })

    MagellanNTK::format_DT_server('DT_previewProtID',
      reactive({previewProtID()}))
    
    output$DataId_previewProteinID_ui <- renderUI({
      req(rv.widgets$DataId_parentProteinID)
      req(rv.widgets$DataId_show_previewProteinID)
      
      MagellanNTK::format_DT_ui(ns('DT_previewProtID'))
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('DataId', btnEvents()))
     
      if (is.null(rv.widgets$DataId_datasetId) || rv.widgets$DataId_datasetId == "" || !datasetID_Ok() || 
          (rv.widgets$SelectFile_typeOfData == "peptide" && (is.null(rv.widgets$DataId_parentProteinID) 
                                                             || rv.widgets$DataId_parentProteinID == "" )) ) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("DataId process", id), {
          shiny::incProgress(0.5)
          
          req(rv.widgets$DataId_datasetId)
          
          if(rv.widgets$SelectFile_typeOfData != "protein"){
            req(rv.widgets$DataId_parentProteinID)}
          
          # DO NOT MODIFY THE THREE FOLLOWING LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status['DataId'] <- MagellanNTK::stepStatus$VALIDATED
        })
      }
    })
    
    
    ###########################################################################-
    #
    #--------------------------EXP AND FEAT DATA--------------------------------
    #
    ###########################################################################-
    output$ExpandFeatData <- renderUI({
      MagellanNTK::process_layout_process(session,
        ns = NS(id),
        sidebar = tagList(),
        content = tagList(
          tags$style(HTML(".ExpandFeatData_content img { margin: 5px; }")),
          div(class = "ExpandFeatData_content",
            div(style = "margin-bottom: 10px; margin-top: -15px;",
              uiOutput(ns("ExpandFeatData_warningNegValues_ui")),
              uiOutput(ns("ExpandFeatData_warningNonNum_ui"))),
            div(style = "display:flex; gap: 25px;", 
              uiOutput(ns("ExpandFeatData_quantCols_ui"), style = "margin-left: 15px;"),
              div(
                uiOutput(ns('ExpandFeatData_idMethod_ui')),
                uiOutput(ns("ExpandFeatData_inputGroup_ui"))
               )
            )
          )
        )
      )
    })
    
    #### _sidebar -----
    output$ExpandFeatData_idMethod_ui <- renderUI({
      widget <- radioButtons(ns("ExpandFeatData_idMethod"), 
                             "Provide identification method",
                             choices = list(
                               "No (default values will be computed)" = FALSE,
                               "Yes" = TRUE),
                             selected = rv.widgets$ExpandFeatData_idMethod)
      
      cond <- rv$steps.enabled['ExpandFeatData'] && !is.null(rv.widgets$ExpandFeatData_quantCols)
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['ExpandFeatData'])
    })
    
    #### _content -----
    output$ExpandFeatData_quantCols_ui <- renderUI({
      req(rv.custom$tab)
      widget <- selectInput(ns("ExpandFeatData_quantCols"),
        "Select quantification columns",
        choices = setNames(nm=colnames(rv.custom$tab)),
        multiple = TRUE, selectize = FALSE ,
        width = "300px", size = 20,
        selected = rv.widgets$ExpandFeatData_quantCols
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['ExpandFeatData'])
    })
    
    output$ExpandFeatData_warningNegValues_ui <- renderUI({
      req(rv.widgets$SelectFile_checkDataLogged == "no")
      req(rv.widgets$ExpandFeatData_quantCols)
      req(length(which(rv.custom$tab[, rv.widgets$ExpandFeatData_quantCols] < 0)) > 0)
      
      div(style = "display: flex;", 
          img(src = "images/Problem.png", height = 25),
          p(style = "color: red;", 
          "Warning: Your original dataset may contain negative values, which cannot be log-transformed. \n
          Please check your dataset or review the log-transformation option in the first tab.")
      )
    })
    
    output$ExpandFeatData_warningNonNum_ui <- renderUI({
      req(rv.custom$tab)
      req(rv.widgets$ExpandFeatData_quantCols)
      req(!all(sapply(rv.custom$tab[, rv.widgets$ExpandFeatData_quantCols, drop = FALSE],
        is.numeric)))
      
      div(style = "display: flex; margin-top: -15px;", 
          img(src = "images/Problem.png", height = 25),
          p(style = "color: red; margin-top: 6px;", 
            "Warning: At least one of the selected columns contains non-numerical data.")
      )
    })
    
    output$ExpandFeatData_inputGroup_ui <- renderUI({
      req(as.logical(rv.widgets$ExpandFeatData_idMethod))
      
      rv.widgets$ExpandFeatData_inputGroup <- Prostar2::mod_inputGroup_server('inputGroup',
        df = reactive({rv.custom$tab}),
        quantCols = reactive({rv.widgets$ExpandFeatData_quantCols}),
        is.enabled = reactive({rv$steps.enabled['ExpandFeatData']}))
      
      rv.widgets$ExpandFeatData_quantCols
      mod_inputGroup_ui(ns('inputGroup'))
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('ExpandFeatData', btnEvents()))
      
      if (is.null(rv.widgets$ExpandFeatData_quantCols) || !all(sapply(rv.custom$tab[, rv.widgets$ExpandFeatData_quantCols, drop = FALSE], is.numeric)) || 
          (as.logical(rv.widgets$ExpandFeatData_idMethod) && is.null(rv.widgets$ExpandFeatData_inputGroup()))) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("ExpandFeatData process", id), {
          shiny::incProgress(0.5)
          
          req(rv.widgets$ExpandFeatData_quantCols)
          req(all(sapply(rv.custom$tab[, rv.widgets$ExpandFeatData_quantCols, drop = FALSE],
            is.numeric)))
          
          if (as.logical(rv.widgets$ExpandFeatData_idMethod)){
            req(rv.widgets$ExpandFeatData_inputGroup())
          }
          
          # new.dataset <- 10*rv$dataIn[[length(rv$dataIn)]]
          # rv$dataIn <- Add_Datasets_to_Object(object = rv$dataIn,
          #                                     dataset = new.dataset,
          #                                     name = paste0('temp_',id))
          
          # DO NOT MODIFY THE THREE FOLLOWING LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status['ExpandFeatData'] <- MagellanNTK::stepStatus$VALIDATED
        })
      }
    })
    
    
    ###########################################################################-
    #
    #--------------------------------DESIGN-------------------------------------
    #
    ###########################################################################-
    output$Design <- renderUI({
      MagellanNTK::process_layout_process(session,
        ns = NS(id),
        sidebar = tagList(),
        content = tagList(
          tags$style(HTML(".Design_content img { margin: 5px; }")),
          div(class = "Design_content",
            uiOutput(ns("Design_designEx_ui")),
            uiOutput(ns('dl_ui'))
          )
        )
      )
    })
    
    #### _sidebar -----
    #### _content -----
    observe({
      req(rv$steps.enabled["Design"])
      # rv.widgets$ExpandFeatData_quantCols
      # remoteReset
      # print("obs")
      # print(remoteReset())

      rv.custom$design <- Prostar2::mod_buildDesign_server(
        "designEx", 
        quantCols = reactive({rv.widgets$ExpandFeatData_quantCols}),
        remoteReset = reactive({remoteReset()}),
        is.enabled = reactive({rv$steps.enabled['Design']})
      )
   })
    
    output$Design_designEx_ui <- renderUI({
      req(rv.widgets$ExpandFeatData_quantCols)
      rv.widgets$ExpandFeatData_quantCols
      remoteReset
      
      MagellanNTK::toggleWidget(mod_buildDesign_ui(ns("designEx")), rv$steps.enabled['Design'])
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Design', btnEvents()))
      
      if (is.null(rv.custom$design()$trigger)) {
        shinyjs::info(btnVentsMasg)
      } else {
        shiny::withProgress(message = paste0("Design process", id), {
          shiny::incProgress(0.5)
          
          # new.dataset <- 10*rv$dataIn[[length(rv$dataIn)]]
          # rv$dataIn <- Add_Datasets_to_Object(object = rv$dataIn,
          #                                     dataset = new.dataset,
          #                                     name = paste0('temp_',id))
          
          # DO NOT MODIFY THE THREE FOLLOWING LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- NULL
          rv$steps.status['Design'] <- MagellanNTK::stepStatus$VALIDATED
        })
      }
    })
    
    
    ###########################################################################-
    #
    #-------------------------------------SAVE----------------------------------
    #
    ###########################################################################-
    output$Save <- renderUI({
      MagellanNTK::process_layout_process(session,
        ns = NS(id),
        sidebar = tagList(),
        content = tagList(div(style = "margin-left: 10px;",
          uiOutput(ns('dl_ui')),
          br(),
          uiOutput(ns('Save_infos_ui')))
        )
      )
    })
    
    #### _content -----
    output$Save_infos_ui <- renderUI({
      MagellanNTK::toggleWidget(
        tagList(
          textInput(ns('Save_analysis'), 'Name of the analysis', 
            placeholder = 'Name of the analysis', width = "400px"),
          textAreaInput(ns('Save_description'), 'Description of the analysis', 
            placeholder = 'Description of the analysis', height = '150px', width = "400px")
        ), rv$steps.enabled['Save']
      )
    })
    
    output$dl_ui <- renderUI({
      # req(config@mode == 'process')
      req(rv$steps.status['Save'] == MagellanNTK::stepStatus$VALIDATED)
      
      MagellanNTK::download_dataset_ui(ns(paste0(id, '_createQuickLink')))
      
    })    
    
    output$Save_infos_dataset_UI <- renderUI({
      req(rv$dataIn)
      infos_dataset_server(
        id = "Convert_Save_infosdataset",
        dataIn = reactive({rv$dataIn})
      )
      
      infos_dataset_ui(id = ns("Convert_Save_infosdataset"))
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Save', btnEvents()))
      
      shiny::withProgress(message = paste0("Save process", id), {
        shiny::incProgress(0.5)
        
        # Check if the conditions have been reordered or not.
        # If it is the case, the metacells must also be reordered
        # in the same way.
        #rv.custom$design
        
        # Reorder columns before creating QFeatures object
        # print(rv.custom$design()$design)
        # print(rv.custom$design()$order)
        # as.data.frame(rv.custom$design()$design)
        
        if ((rv.widgets$Save_analysis == "") || is.null(rv.widgets$Save_analysis)){
          rv.widgets$Save_analysis <- "myDataset"
        }
        
        .indexForMetacell <- NULL
        if (!is.null(rv.widgets$ExpandFeatData_inputGroup)){
          print("_________if !is.null(rv.widgets$ExpandFeatData_inputGroup)")
          print("-- order :")
          print(rv.custom$design()$order)
          print("--rv.wid$expand_inputgrp :")
          print(rv.widgets$ExpandFeatData_inputGroup())
          print("--rv.wid$expand_quantcol :")
          print(rv.widgets$ExpandFeatData_quantCols)
          .indexForMetacell <- rv.widgets$ExpandFeatData_inputGroup()[rv.custom$design()$order]
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
          logData = rv.widgets$SelectFile_checkDataLogged == 'no',
          indexForMetacell = .indexForMetacell,
          typeDataset = rv.widgets$SelectFile_typeOfData,
          parentProtId = rv.widgets$DataId_parentProteinID,
          force.na = rv.widgets$SelectFile_replaceAllZeros,
          software = rv.widgets$SelectFile_software,
          name.pipeline = "PipelineProtein"
        )
      })
      
      S4Vectors::metadata(rv$dataIn)$name.pipeline <- 'PipelineProtein'
      # DO NOT MODIFY THE THREE FOLLOWING LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      dataOut$name = rv.custom$name
      rv$steps.status['Save'] <- MagellanNTK::stepStatus$VALIDATED
      
      Prostar2::download_dataset_server(paste0(id, '_createQuickLink'), dataIn = reactive({rv$dataIn}),
                                        filename = rv.widgets$Save_analysis)
    })
    
    ####### _END_ -----
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  }
  )
}
