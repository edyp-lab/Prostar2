#' @title PipelineProtein Imputation module
#'
#' @description
#' This module contains the imputation step of the protein pipeline.
#' 
#' @param id A `character(1)` which is the 'id' of the module.
#'
#' @param dataIn An instance of the class `MultiAssayExperiment`
#'
#' @param steps.enabled A vector of boolean which has the same length of the steps
#' of the pipeline. This information is used to enable/disable the widgets. It is not
#' a communication variable between the caller and this module, thus there is no
#' corresponding output variable
#'
#' @param remoteReset It is a remote command to reset the module. An `integer()` that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#'
#' @param steps.status A vector of `character()` which indicates the status of each step
#' which can be either 'validated', 'undone' or 'skipped'. Enabled or disabled in the UI.
#' 
#' @param current.pos A `integer(1)` which acts as a remote command to make
#'  a step active in the timeline. Default is 1.
#'  
#' @param path A `character()` which is the path to the directory which 
#' contains the files and directories of the pipeline.
#' 
#' @examples
#' if (interactive()){
#'   Prostar2("PipelineProtein_Imputation")
#' }
#' 
#' @name PipelineProtein_Imputation
#' 
#' @importFrom stats setNames rnorm
#' @importFrom shinyjs useShinyjs
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' 
#' @return An instance of the class `MultiAssayExperiment`
#' 
NULL


#' @rdname PipelineProtein_Imputation
#' @export
#' 
PipelineProtein_Imputation_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelineProtein_Imputation',
    mode = 'process',
    steps = c('POV Imputation', 'MEC Imputation'),
    mandatory = c(FALSE, FALSE)
  )
}


#' @rdname PipelineProtein_Normalization
#' @export
#' 
PipelineProtein_Imputation_ui <- function(id){
  ns <- NS(id)
}


#' @rdname PipelineProtein_Normalization
#' @export
#' 
PipelineProtein_Imputation_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  btnEvents = reactive({NULL})
){
  
  pkgs_require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list(
    POVImputation_algorithm = NULL,
    POVImputation_KNN_n = 10,
    POVImputation_detQuant_quantile = 2.5,
    POVImputation_detQuant_factor = 1,
    MECImputation_algorithm = NULL,
    MECImputation_KNN_n = 10,
    MECImputation_detQuant_quantile = 2.5,
    MECImputation_detQuant_factor = 1,
    MECImputation_fixedValue = 0
  )
  
  rv.custom.default.values <- list(
    dataIn1 = NULL,
    dataIn2 = NULL,
    tmp.mec = reactive({NULL}),
    tmp.pov = reactive({NULL}),
    history = MagellanNTK::InitializeHistory(),
    mv.present = FALSE,
    POVImputation_SummaryDT = data.frame(
      Operation = "-",
      nbImputed = "0",
      TotalMissingValues = '0',
      stringsAsFactors = FALSE
    ),
    MECImputation_SummaryDT = data.frame(
      Operation = "-",
      nbImputed = "0",
      TotalMissingValues = '0',
      stringsAsFactors = FALSE
    ),
    result_open_dataset = reactive({NULL})
  )
  
  imputationAlgorithmsProteins_MEC <- list(
    "None" = "None",
    "Det quantile" = "detQuantile",
    "Fixed value" = "fixedValue"
  )
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    core.code <- MagellanNTK::Get_Workflow_Core_Code(
      mode = 'process',
      name = id,
      w.names = names(widgets.default.values),
      rv.custom.names = names(rv.custom.default.values)
    )
    
    eval(str2expression(core.code))
    add_resourcePath()
    
    mv.present <- reactive({
      #Useful for the MEC imputation
      qdata <- SummarizedExperiment::assay(dataIn()[[length(dataIn())]])
      rv.custom$mv.present <- sum(is.na(qdata)) > 0
      rv.custom$mv.present
    })
    
    
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
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns('open_dataset_UI'))
        ),
        content = div(id = ns('div_content'),
          if (file.exists(file))
            includeMarkdown(file)
          else
            p('No Description available')
          #uiOutput(ns('Description_infos_dataset_UI'))
        )
      )
      
    })
    
    #### _sidebar -----
    output$open_dataset_UI <- renderUI({
      req(session$userData$runmode == 'process')
      req(is.null(dataIn()))
      req(NULL)
      rv.custom$result_open_dataset <- MagellanNTK::open_dataset_server(
        id = "open_dataset",
        class = 'QFeatures',
        extension = "qf",
        remoteReset = reactive({remoteReset()})
      )
      
      MagellanNTK::open_dataset_ui(id = ns("open_dataset"))
    })
    
    #### _content -----
    # output$Description_infos_dataset_UI <- renderUI({
    #   req(rv$dataIn)
    #   
    #   infos_dataset_server(
    #     id = "Description_infosdataset",
    #     dataIn = reactive({rv$dataIn})
    #   )
    #   
    #   infos_dataset_ui(id = ns("Description_infosdataset"))
    # })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Description', btnEvents()))
      #rv.custom$result_open_dataset()$dataset
      req(dataIn())
      
      rv$dataIn <- dataIn()
      
      if(!is.null(rv.custom$result_open_dataset()$dataset))
        rv$dataIn <- rv.custom$result_open_dataset()$dataset
      
      rv.custom$dataIn1 <- rv$dataIn
      rv.custom$dataIn2 <- rv$dataIn
      
      dtImput <- data.frame(
        Operation = "-",
        nbImputed = "0",
        TotalMissingValues = QFeatures::nNA(rv$dataIn[[length(rv$dataIn)]])$nNA[, "nNA"],
        stringsAsFactors = FALSE
      )
      #colnames(dtImput) <- c('Operation', 'Nb imputed', 'Total missing values')
      
      rv.custom$POVImputation_SummaryDT <- dtImput
      rv.custom$MECImputation_SummaryDT <- dtImput
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status['Description'] <- MagellanNTK::stepStatus$VALIDATED
    })
    
    
    ###########################################################################-
    #
    #----------------------------POV IMPUTATION---------------------------------
    #
    ###########################################################################-
    output$POVImputation <- renderUI({
      shinyjs::useShinyjs()
      path <- file.path(system.file('www/css', package = 'MagellanNTK'),'MagellanNTK.css')
      includeCSS(path)
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns("POVImputation_algorithm_UI")),
          uiOutput(ns("POVImputation_KNN_nbNeighbors_UI")),
          uiOutput(ns("POVImputation_detQuant_UI"))
        ),
        content = div(
          tags$style(HTML(".mv-container img {margin: 0 !important;}")),
          uiOutput(ns('POVImputation_DT_UI')),
          uiOutput(ns("POVImputation_showDetQuantValues")),
          div(class = "mv-container", style = "display: flex; margin-top: 20px;",
              uiOutput(ns("mvplots_ui"))
          )
        )
      )
    })
    
    #### _sidebar -----
    output$POVImputation_algorithm_UI <- renderUI({
      
      widget <- selectInput(ns("POVImputation_algorithm"), 
                            "Algorithm for POV",
                            choices = list(
                              "None" = "None",
                              "slsa" = "slsa",
                              "Det quantile" = "detQuantile",
                              "KNN" = "KNN"
                            ),
                            selected = rv.widgets$POVImputation_algorithm,
                            width = "150px",
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["POVImputation"])
    })
    
    output$POVImputation_KNN_nbNeighbors_UI <- renderUI({
      req(rv.widgets$POVImputation_algorithm == 'KNN')
      
      widget <- shinyWidgets::autonumericInput(
        ns("POVImputation_KNN_nbNeighbors"),
        label = "Neighbors",
        value = isolate(rv.widgets$POVImputation_KNN_n), 
        width = "100px",
        minimumValue = 1,
        maximumValue = max(nrow(rv.custom$dataIn1), widgets.default.values$POVImputation_KNN_n),
        decimalCharacter = ".",
        decimalPlaces = 0,
        modifyValueOnWheel = TRUE,
        align = "left"
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["POVImputation"])
    })
    
    
    output$POVImputation_detQuant_UI <- renderUI({
      req(rv.widgets$POVImputation_algorithm == 'detQuantile')
      
      widget <- div(
        style = "display: flex; gap: 10px;",
        shinyWidgets::autonumericInput(
          ns("POVImputation_detQuant_quantile"),
          label = "Quantile",
          value = isolate(rv.widgets$POVImputation_detQuant_quantile), 
          width = "100px",
          minimumValue = 0,
          maximumValue = 100,
          decimalCharacter = ".",
          currencySymbol = " %",
          decimalPlaces = 1,
          modifyValueOnWheel = TRUE,
          currencySymbolPlacement = "s",
          align = "left"
        ),
        shinyWidgets::autonumericInput(
          ns("POVImputation_detQuant_factor"),
          label = "Factor",
          value = isolate(rv.widgets$POVImputation_detQuant_factor),  
          width = "100px",
          minimumValue = 0,
          maximumValue = 10,
          decimalCharacter = ".",
          decimalPlaces = 1,
          modifyValueOnWheel = TRUE,
          align = "left"
        )
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["POVImputation"])
    })
    
    #### _content -----
    MagellanNTK::format_DT_server("POV_dt", 
      dataIn = reactive({rv.custom$POVImputation_SummaryDT}))
    
    output$POVImputation_DT_UI <- renderUI({
      req(rv.custom$POVImputation_SummaryDT)
      MagellanNTK::format_DT_ui(ns("POV_dt"))
    })
    
    output$mvplots_ui <- renderUI({
      widget <- mod_mv_plots_ui(ns("POVImputation_mvplots"))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["POVImputation"])
    })
    
    observe({
      req(rv.custom$dataIn1)
      
      pal <- DaparToolshed::GetColorsForConditions(unique(DaparToolshed::design_qf(rv.custom$dataIn1)$Condition), 
                                            DaparToolshed::ExtendPalette(length(unique(DaparToolshed::design_qf(rv.custom$dataIn1)$Condition))))
      
      mod_mv_plots_server("POVImputation_mvplots",
        data = reactive({rv.custom$dataIn1[[length(rv.custom$dataIn1)]]}),
        grp = reactive({omXplore::get_group(rv.custom$dataIn1)}),
        mytitle = reactive({"POV imputation"}),
        pal = pal,
        pattern = reactive({c("Missing", "Missing POV", "Missing MEC")})
      )
    })
    
    output$POVImputation_showDetQuantValues <- renderUI({
      req(rv.widgets$POVImputation_algorithm == "detQuantile")
      
      mod_DetQuantImpValues_server(
        id = "POVImputation_DetQuantValues_DT",
        dataIn = reactive({rv.custom$dataIn1[[length(rv.custom$dataIn1)]]}),
        quant = reactive({rv.widgets$POVImputation_detQuant_quantile}),
        factor = reactive({rv.widgets$POVImputation_detQuant_factor})
      )
      
      tagList(
        #h5("The POV will be imputed by the following values :"),
        mod_DetQuantImpValues_ui(ns("POVImputation_DetQuantValues_DT"))
      )
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('POVImputation', btnEvents()))
      req(rv.custom$dataIn1)
      if (is.null(rv.custom$dataIn1) || 
          rv.widgets$POVImputation_algorithm == "None")
        shinyjs::info(btnVentsMasg)
      
      else {
        req(rv.widgets$POVImputation_algorithm != "None")
        m <- DaparToolshed::matchMetacell(
          DaparToolshed::qMetacell(rv.custom$dataIn1[[length(rv.custom$dataIn1)]]),
          pattern = "Missing POV",
          level = DaparToolshed::typeDataset(rv.custom$dataIn1[[length(rv.custom$dataIn1)]])
        )
        nbPOVBefore <- length(which(m))
        
        withProgress(message = "", detail = "", value = 0, {
          incProgress(0.5, detail = "Imputing POV")
          
          .tmp <- NULL
          .param <- list()
          
          try({
            switch(rv.widgets$POVImputation_algorithm,
              slsa = {
                incProgress(0.5, detail = "slsa Imputation")
                
                .tmp <- DaparToolshed::wrapperImputeSLSA(
                  obj = rv.custom$dataIn1[[length(rv.custom$dataIn1)]],
                  design = DaparToolshed::design_qf(rv.custom$dataIn1))
                
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'POVImputation', 'algorithm', rv.widgets$POVImputation_algorithm)
              },
              
              detQuantile = {
                incProgress(0.5, detail = "det quantile Imputation")
                
                .tmp <- DaparToolshed::wrapperImputeDetQuant(
                  obj = rv.custom$dataIn1[[length(rv.custom$dataIn1)]],
                  qval = rv.widgets$POVImputation_detQuant_quantile / 100,
                  factor = rv.widgets$POVImputation_detQuant_factor,
                  na.type = 'Missing POV')
                
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'POVImputation', 'algorithm', rv.widgets$POVImputation_algorithm)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'POVImputation', 'quantile', rv.widgets$POVImputation_detQuant_quantile)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'POVImputation', 'factor', rv.widgets$POVImputation_detQuant_factor)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'POVImputation', 'na.type', 'Missing POV')
              },
              
              KNN = {
                incProgress(0.5, detail = "KNN Imputation")
                
                .tmp <- DaparToolshed::wrapperImputeKNN(
                  obj = rv.custom$dataIn1[[length(rv.custom$dataIn1)]],
                  grp = DaparToolshed::design_qf(rv.custom$dataIn1)$Condition,
                  K = rv.widgets$POVImputation_KNN_n)
                
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'POVImputation', 'algorithm', rv.widgets$POVImputation_algorithm)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'POVImputation', 'K', rv.widgets$POVImputation_KNN_n)
              }
            )
          })
          
          if(inherits(.tmp, "try-error") || inherits(.tmp, "try-warning")) {
            MagellanNTK::mod_SweetAlert_server(id = 'sweetalert_perform_POVimputation_button',
              text = .tmp,
              type = 'error' )
          } else {
            incProgress(1, detail = "Finalize POV imputation")
            
            m <- DaparToolshed::matchMetacell(DaparToolshed::qMetacell(.tmp),
              pattern = "Missing POV",
              level = DaparToolshed::typeDataset(.tmp)
            )
            nbPOVAfter <- length(which(m))
            rv$nbPOVimputed <- nbPOVBefore - nbPOVAfter
            
            rv.custom$dataIn1 <- Prostar2::addDatasets(
              rv.custom$dataIn1,
              .tmp,
              'POVImputation')
            
            # Add infos
            nBefore <- QFeatures::nNA(rv.custom$dataIn1[[length(rv.custom$dataIn1) - 1]])$nNA[, "nNA"]
            nAfter <- QFeatures::nNA(rv.custom$dataIn1[[length(rv.custom$dataIn1)]])$nNA[, "nNA"]
            
            rv.custom$POVImputation_SummaryDT <- rbind(
              rv.custom$POVImputation_SummaryDT ,
              c("POV Imputation", nBefore - nAfter, nAfter)
            )
            
            rv.custom$dataIn2 <- rv.custom$dataIn1
            
            rv.custom$MECImputation_SummaryDT <- rv.custom$POVImputation_SummaryDT
            
            # DO NOT MODIFY THE THREE FOLLOWING LINES
            dataOut$trigger <- MagellanNTK::Timestamp()
            dataOut$value <- NULL
            rv$steps.status['POVImputation'] <- MagellanNTK::stepStatus$VALIDATED
          }
        })
      }
    })
    
    
    ###########################################################################-
    #
    #----------------------------MEC IMPUTATION---------------------------------
    #
    ###########################################################################-
    output$MECImputation <- renderUI({
      shinyjs::useShinyjs()
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns("MECImputation_chooseImputationMethod_ui")),
          uiOutput(ns("MECImputation_Params_ui"))
        ),
        content = tagList(
          tags$style(HTML(".mv-container img {margin: 0 !important;}")),
          uiOutput(ns("MECImputation_DT_UI")),
          uiOutput(ns("warningMECImputation")),
          uiOutput(ns("MECImputation_showDetQuantValues_ui")),
          tags$hr(),
          withProgress(message = "", detail = "", value = 0, {
            incProgress(0.5, detail = "Building plots...")
            uiOutput(ns('MECImputation_mvplots_ui'))
          })
        )
      )
    })
    
    #### _sidebar -----
    output$MECImputation_chooseImputationMethod_ui <- renderUI({
      req(mv.present())
      
      widget <- selectInput(ns("MECImputation_algorithm"), "Algorithm for MEC",
                            choices = imputationAlgorithmsProteins_MEC,
                            selected = rv.widgets$MECImputation_algorithm, 
                            width = "150px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["MECImputation"])
    })
    
    output$MECImputation_Params_ui <- renderUI({
      req(mv.present())
      req(rv.widgets$MECImputation_algorithm != "None")
      
      widget <- switch(rv.widgets$MECImputation_algorithm,
                       detQuantile = {
                         widget <- div(
                           style = "display: flex; gap: 10px;",
                           shinyWidgets::autonumericInput(
                             ns("POVImputation_detQuant_quantile"),
                             label = "Quantile",
                             value = isolate(rv.widgets$POVImputation_detQuant_quantile),  
                             width = "100px",
                             minimumValue = 0,
                             maximumValue = 100,
                             decimalCharacter = ".",
                             currencySymbol = " %",
                             decimalPlaces = 1,
                             modifyValueOnWheel = TRUE,
                             currencySymbolPlacement = "s",
                             align = "left"
                           ),
                           shinyWidgets::autonumericInput(
                             ns("POVImputation_detQuant_factor"),
                             label = "Factor",
                             value = isolate(rv.widgets$POVImputation_detQuant_factor), 
                             width = "100px",
                             minimumValue = 0,
                             maximumValue = 10,
                             decimalCharacter = ".",
                             decimalPlaces = 1,
                             modifyValueOnWheel = TRUE,
                             align = "left"
                           )
                         )},
                       
                       fixedValue = {
                         shinyWidgets::autonumericInput(
                           ns("MECImputation_fixedValue"),
                           label = "Factor",
                           value = isolate(rv.widgets$MECImputation_fixedValue), 
                           width = "100px",
                           minimumValue = 0,
                           maximumValue = 100,
                           decimalCharacter = ".",
                           decimalPlaces = 1,
                           modifyValueOnWheel = TRUE,
                           align = "left"
                         )
                       }
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["MECImputation"])
    })
    
    #### _content -----
    MagellanNTK::format_DT_server("MEC_dt", 
      dataIn = reactive({rv.custom$MECImputation_SummaryDT}))
    
    output$MECImputation_DT_UI <- renderUI({
      req(rv.custom$MECImputation_SummaryDT)
      MagellanNTK::format_DT_ui(ns("MEC_dt"))
    })
    
    output$MECImputation_mvplots_ui <- renderUI({
      widget <- mod_mv_plots_ui(ns("MECImputation_mvplots"))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["MECImputation"])
    })
    
    observe({
      req(rv.custom$dataIn2)
      
      pal <- DaparToolshed::GetColorsForConditions(unique(DaparToolshed::design_qf(rv.custom$dataIn2)$Condition), 
                                                   DaparToolshed::ExtendPalette(length(unique(DaparToolshed::design_qf(rv.custom$dataIn2)$Condition))))
      
      mod_mv_plots_server("MECImputation_mvplots",
        data = reactive({rv.custom$dataIn2[[length(rv.custom$dataIn2)]]}),
        grp = reactive({omXplore::get_group(rv.custom$dataIn2)}),
        mytitle = reactive({"MEC imputation"}),
        pal = pal,
        pattern = reactive({c("Missing", "Missing POV", "Missing MEC")})
      )
    })
    
    output$MECImputation_showDetQuantValues_ui <- renderUI({
      req(rv.widgets$MECImputation_algorithm == "detQuantile")
      
      mod_DetQuantImpValues_server(
        id = "MECImputation_DetQuantValues_DT",
        dataIn = reactive({rv.custom$dataIn2[[length(rv.custom$dataIn2)]]}),
        quant = reactive({rv.widgets$MECImputation_detQuant_quantile}),
        factor = reactive({rv.widgets$MECImputation_detQuant_factor})
      )
      
      tagList(
        #h5("The MEC will be imputed by the following values :"),
        mod_DetQuantImpValues_ui(ns("MECImputation_DetQuantValues_DT"))
      )
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('MECImputation', btnEvents()))
      
      if ( is.null(rv.custom$dataIn2) || 
          rv.widgets$MECImputation_algorithm == "None")
        shinyjs::info(btnVentsMasg)
      
      else {
        req(rv.custom$dataIn2)
        req(rv.widgets$MECImputation_algorithm != "None")
        withProgress(message = "", detail = "", value = 0, {
          incProgress(0.5, detail = "Imputing MEC")
          
          m <- DaparToolshed::matchMetacell(
            DaparToolshed::qMetacell(rv.custom$dataIn2[[length(rv.custom$dataIn2)]]),
            pattern = "Missing MEC",
            level = DaparToolshed::typeDataset(rv.custom$dataIn2[[length(rv.custom$dataIn2)]])
          )
          nbMECBefore <- length(which(m))
          incProgress(0.5, detail = "MEC Imputation")
          
          .tmp <- NULL
          .param <- list()
          try({
            switch(rv.widgets$MECImputation_algorithm,
              detQuantile = {
                incProgress(0.5, detail = "det quantile Imputation")
                .tmp <- DaparToolshed::wrapperImputeDetQuant(
                  obj = rv.custom$dataIn2[[length(rv.custom$dataIn2)]],
                  qval = rv.widgets$MECImputation_detQuant_quantile / 100,
                  factor = rv.widgets$MECImputation_detQuant_factor,
                  na.type = 'Missing MEC')
                
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'MECImputation', 'algorithm', rv.widgets$MECImputation_algorithm)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'MECImputation', 'quantile', rv.widgets$MECImputation_detQuant_quantile)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'MECImputation', 'factor', rv.widgets$MECImputation_detQuant_factor)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'MECImputation', 'na.type', 'Missing MEC')
              },
              
              fixedValue = {
                .tmp <- DaparToolshed::wrapperImputeFixedValue(
                  obj = rv.custom$dataIn2[[length(rv.custom$dataIn2)]],
                  fixVal = rv.widgets$MECImputation_fixedValue,
                  na.type = "Missing MEC"
                )
                
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'MECImputation', 'algorithm', rv.widgets$MECImputation_algorithm)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'MECImputation', 'fixVal', rv.widgets$MECImputation_fixedValue)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Imputation', 'MECImputation', 'na.type', 'Missing MEC')
              }
            )
          })
          
          if(inherits(.tmp, "try-error")) {
            MagellanNTK::mod_SweetAlert_server(id = 'sweetalert_perform_MECimputation_button',
              text = .tmp,
              type = 'error' )
          } else {
            incProgress(1, detail = "Finalize MEC imputation")
            
            m <- DaparToolshed::matchMetacell(DaparToolshed::qMetacell(.tmp),
                                               pattern = "Missing MEC",
                                               level = DaparToolshed::typeDataset(.tmp)
            )
            nbMECAfter <- length(which(m))
            rv$nbMECimputed <- nbMECBefore - nbMECAfter
            
            rv.custom$dataIn2 <- Prostar2::addDatasets(
              rv.custom$dataIn2,
              .tmp,
              'MECImputation')
            
            # Add infos
            nBefore <- QFeatures::nNA(rv.custom$dataIn2[[length(rv.custom$dataIn2) - 1]])$nNA[, "nNA"]
            nAfter <- QFeatures::nNA(rv.custom$dataIn2[[length(rv.custom$dataIn2)]])$nNA[, "nNA"]
            
            rv.custom$MECImputation_SummaryDT <- rbind(
              rv.custom$MECImputation_SummaryDT ,
              c("MEC Imputation",
                nBefore - nAfter,
                nAfter)
            )
            
            # DO NOT MODIFY THE THREE FOLLOWING LINES
            dataOut$trigger <- MagellanNTK::Timestamp()
            dataOut$value <- NULL
            rv$steps.status['MECImputation'] <- MagellanNTK::stepStatus$VALIDATED
          }
        })
      }
    })
    
    
    ###########################################################################-
    #
    #-------------------------------------SAVE----------------------------------
    #
    ###########################################################################-
    output$Save <- renderUI({
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(),
        content = tagList(
          uiOutput(ns('save_txt')),
          uiOutput(ns('dl_ui'))
          )
      )
    })
    
    #### _content -----
    output$save_txt <- renderUI({
      req(rv$steps.status['Save'] != MagellanNTK::stepStatus$VALIDATED)
      req(config@mode == 'process')
      
      div(
        style = "margin: 25px;",
        p(HTML("Click <b>'Run'</b> to validate this step.<br>
                If you need to make changes, click <b>'Reset'</b>."),
          style = "font-size: 17px;
                   line-height: 1.6;
                   margin: 0;
                   padding: 12px 16px;
                   background-color: #EAEAEA;
                   border-radius: 4px;"
        )
      )
    })
    
    output$dl_ui <- renderUI({
      req(rv$steps.status['Save'] == MagellanNTK::stepStatus$VALIDATED)
      req(config@mode == 'process')
      
      Prostar2::download_dataset_ui(ns(paste0(id, '_createQuickLink')))
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Save', btnEvents()))
      
      shiny::withProgress(message = paste0("Save process", id), {
        shiny::incProgress(0.5)
        
        if (isTRUE(all.equal(SummarizedExperiment::assays(rv$dataIn),
          SummarizedExperiment::assays(rv.custom$dataIn2)))){
          shinyjs::info(btnVentsMasg)
        
        } else {
          len_start <- length(rv$dataIn)
          len_end <- length(rv.custom$dataIn2)
          len_diff <- len_end - len_start
          
          req(len_diff > 0)
          
          if (len_diff == 2)
            rv.custom$dataIn2 <- QFeatures::removeAssay(rv.custom$dataIn2, 
              length(rv.custom$dataIn2) - 1)
          
          # Rename the new dataset with the name of the process
          i <- length(rv.custom$dataIn2)
          names(rv.custom$dataIn2)[i] <- 'Imputation'
          S4Vectors::metadata(rv.custom$dataIn2)$name.pipeline <- 'PipelineProtein'
          
          DaparToolshed::paramshistory(rv.custom$dataIn2[[i]]) <- rbind(DaparToolshed::paramshistory(rv.custom$dataIn2[[i]]),
                                                                   rv.custom$history)
          
          # DO NOT MODIFY THE THREE FOLLOWING LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- rv.custom$dataIn2
          rv$steps.status['Save'] <- MagellanNTK::stepStatus$VALIDATED
          
          Prostar2::download_dataset_server(paste0(id, '_createQuickLink'), dataIn = reactive({dataOut$value}))
        }
      })
    })
    
    ####### _END_ -----
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
    }
  )
}
