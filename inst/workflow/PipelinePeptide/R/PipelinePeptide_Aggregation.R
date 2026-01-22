#' @title Shiny example process module.
#'
#' @description
#' This module contains the configuration information for the corresponding pipeline.
#' It is called by the nav_pipeline module of the package MagellanNTK
#' 
#' The name of the server and ui functions are formatted with keywords separated by '_', as follows:
#' * first string `mod`: indicates that it is a Shiny module
#' * `pipeline name` is the name of the pipeline to which the process belongs
#' * `process name` is the name of the process itself
#' 
#' This convention is important because MagellanNTK call the different
#' server and ui functions by building dynamically their name.
#' 
#' In this example, `PipelinePeptide_Aggregation_ui()` and `PipelinePeptide_Aggregation_server()` define
#' the code for the process `ProcessPeptide` which is part of the pipeline called `PipelinePeptide`.
#' 
#' @examples
#' \dontrun{
#' library(MagellanNTK)
#' library(DaparToolshed)
#' library(omXplore)
#' library(shiny)
#' library(waiter)
#' library(shinyjs)
#' library(shinyBS)
#' library(shinydashboard)
#' library(shinydashboardPlus)
#' library(highcharter)
#' library(Prostar2)
#' data(Exp1_R25_pept, package = "DaparToolshedData")
#' obj <- Exp1_R25_pept
#' # Simulate imputation of missing values
#' obj <- NAIsZero(obj, 1)
#' path <- system.file('workflow/PipelinePeptide', package = 'Prostar2')
#' proc_workflowApp("PipelinePeptide_Aggregation", path, dataIn = obj)
#' }
#' 
#' @rdname PipelinePeptide
#' @export
#' 
PipelinePeptide_Aggregation_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelinePeptide_Aggregation',
    mode = 'process',
    steps = c('Aggregation'),
    mandatory = c(TRUE)
  )
}


#' @param id xxx
#' 
#' @rdname PipelinePeptide
#' 
#' @author Samuel Wieczorek, Manon Gaudin
#' 
#' @export
#'
PipelinePeptide_Aggregation_ui <- function(id){
  ns <- NS(id)
}


#' @param id xxx
#'
#' @param dataIn An instance of the class 
#'
#' @param steps.enabled A vector of boolean which has the same length of the steps
#' of the pipeline. This information is used to enable/disable the widgets. It is not
#' a communication variable between the caller and this module, thus there is no
#' corresponding output variable
#'
#' @param remoteReset It is a remote command to reset the module. A boolean that
#' indicates if the pipeline has been reset by a program of higher level
#' Basically, it is the program which has called this module
#' 
#' @param steps.status xxx
#' 
#' @param current.pos xxx
#'
#' @rdname PipelinePeptide
#' 
#' @import foreach
#' 
#' @export
#' @importFrom DaparToolshed typeDataset
#' 
PipelinePeptide_Aggregation_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  btnEvents = reactive({NULL})
){
  
  pkgs.require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))
  
  widgets.default.values <- list(
    Aggregation_includeSharedPeptides = "Yes_Iterative_Redistribution",
    Aggregation_ponderation = "Global",
    Aggregation_operator = "Mean",
    Aggregation_considerPeptides = "allPeptides",
    Aggregation_proteinId = "None",
    Aggregation_topN = 3,
    Aggregation_addRowData = NULL,
    Addmetadata_columnsForProteinDataset = NULL,
    Aggregation_maxiter = 500
  )
  
  rv.custom.default.values <- list(
    result_open_dataset = reactive({NULL}),
    
    nbEmptyLines = NULL,
    temp.aggregate = NULL,
    AggregProtStatsPept = NULL,
    AggregProtStatsProt = NULL
  )
  
  ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    requireNamespace('DaparToolshed')
    
    core.code <- MagellanNTK::Get_Workflow_Core_Code(
      mode = 'process',
      name = id,
      w.names = names(widgets.default.values),
      rv.custom.names = names(rv.custom.default.values)
    )
    
    eval(str2expression(core.code))
    add.resourcePath()
    
    # >>>
    # >>> START ------------- Code for Description UI---------------
    # >>> 
    
    
    output$Description <- renderUI({
      file <- normalizePath(file.path(
        system.file('workflow', package = 'Prostar2'),
        unlist(strsplit(id, '_'))[1], 
        'md', 
        paste0(id, '.Rmd')))
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(),
        content = tagList(
          if (file.exists(file))
            includeMarkdown(file)
          else
            p('No Description available')
        )
      )
    })
    
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Description', btnEvents()))
      req(inherits(dataIn(), 'QFeatures'))
      
      rv$dataIn <- dataIn()
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- MagellanNTK::stepStatus$VALIDATED
    })
    
    
    # >>>
    # >>> START ------------- Code for Aggregation UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI ------------------------------------
    output$Aggregation <- renderUI({
      shinyjs::useShinyjs()
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns('Aggregation_chooseProteinId_ui')),
          uiOutput(ns('Aggregation_includeSharedPeptides_ui')),
          tags$hr(),
          uiOutput(ns('Aggregation_considerPeptides_ui')),
          tags$hr(),
          uiOutput(ns('Aggregation_operator_ui')),
          tags$hr(),
          uiOutput(ns('Aggregation_addRowData_ui'))
        ),
        content = tagList(
          uiOutput(ns('Aggregation_emptyrow_ui')),
          uiOutput(ns('Aggregation_warning_ui')),
          uiOutput(ns('Aggregation_AggregationDone_ui')),
          uiOutput(ns('Aggregation_aggregationStats_ui'))
        )
      )
    })
    
    
    output$Aggregation_warning_ui <- renderUI({
      req(rv$dataIn)
      
      .data <- last_assay(rv$dataIn)
      m <- DaparToolshed::match.metacell(
        DaparToolshed::qMetacell(.data),
        pattern = c("Missing", "Missing POV", "Missing MEC"),
        level = DaparToolshed::typeDataset(.data)
      )
      NA.count <- length(which(m))
      
      if (NA.count > 0) {
        tags$p(style = "color: red;",
        tags$b("Warning:"), " Your dataset contains missing values. 
        For better results, you should impute them first"
        )
      }
    })
    
    output$Aggregation_emptyrow_ui <- renderUI({
      req(rv$dataIn)
      
      .data <- SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])
      rv.custom$nbEmptyLines <- getNumberOfEmptyLines(.data)
      if (rv.custom$nbEmptyLines > 0) {
        tags$p(style = "color: red;",
               tags$b("Warning:"), "Your dataset contains empty lines (fully filled with missing values). 
               Please remove them using the filtering step."
        )
      }
    })
    
    output$Aggregation_chooseProteinId_ui <- renderUI({
      
      if (!is.null(DaparToolshed::parentProtId(last_assay(rv$dataIn)))) {
        return(NULL)
      }
      
      .choices <- colnames(SummarizedExperiment::rowData(last_assay(rv$dataIn)))
      widget <- selectInput(ns("Aggregation_proteinId"),
        "Choose the protein ID",
        choices = c("None", .choices),
        selected = rv.widgets$Aggregation_proteinId
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Aggregation'] )
    })
    
    
    ## Selection of how to handle shared peptides
    MagellanNTK::mod_popover_for_help_server("modulePopover_includeShared",
                                title = "Include shared peptides",
                                content = HTML(
                                  paste0(
                                    "<strong>• No :</strong>",
                                    " only protein-specific peptides ", "<br>",
                                    "<strong>• Yes (as protein specific) :</strong>",
                                    " shared peptides processed as protein specific", "<br>",
                                    "<strong>• Yes (simple redistribution) :</strong>",
                                    " single proportional redistribution of shared peptides", "<br>",
                                    "<strong>• Yes (iterative redistribution) :</strong>",
                                    " iterative proportional redistribution of shared peptides"
                                  )
                                )
    )
    MagellanNTK::mod_popover_for_help_server("modulePopover_ponderation",
                                title = "Ponderation",
                                content = HTML(
                                  paste0(
                                    "Modify how is calculated the coefficient for shared peptides redistribution.", "<br>",
                                    "<strong>• Global :</strong>",
                                    " calculated using all sample from all conditions", "<br>",
                                    "<strong>• Condition-wise :</strong>",
                                    " calculated using samples from each condition independantly", "<br>",
                                    "<strong>• Sample-wise :</strong>",
                                    " calculated using each sample independantly"
                                  )
                                )
    )
    
    MagellanNTK::mod_popover_for_help_server("modulePopover_maxiter",
                                title = "Max iteration",
                                content = HTML(
                                  paste0(
                                    "Maximum number of iteration."
                                  )
                                )
    )
    
    output$Aggregation_includeSharedPeptides_ui <- renderUI({
      widget <- radioButtons(ns("Aggregation_includeSharedPeptides"), 
                             MagellanNTK::mod_popover_for_help_ui(ns("modulePopover_includeShared")),
                             choices = c("No" = "No",
                                         "Yes (as protein specific)" = "Yes_As_Specific",
                                         "Yes (simple redistribution)" = "Yes_Simple_Redistribution",
                                         "Yes (iterative redistribution)" = "Yes_Iterative_Redistribution"),
                             selected = rv.widgets$Aggregation_includeSharedPeptides
      )
      widget2 <- radioButtons(ns("Aggregation_ponderation"), 
                              MagellanNTK::mod_popover_for_help_ui(ns("modulePopover_ponderation")),
                              choices = c("Global" = "Global",
                                          "Condition" = "Condition",
                                          "Sample" = "Sample"),
                              selected = rv.widgets$Aggregation_ponderation
      )
      widget3 <- numericInput(ns("Aggregation_maxiter"),
                              MagellanNTK::mod_popover_for_help_ui(ns("modulePopover_maxiter")),
                              value = rv.widgets$Aggregation_maxiter,
                              min = 1,
                              step = 1,
                              width = "100px"
      )
        
      
      tagList(div(style = "display: inline-block; margin-right: 35px;", 
                  MagellanNTK::toggleWidget(widget, rv$steps.enabled['Aggregation'])),
              div(style = "display: inline-block; vertical-align: top;",
                  if(rv.widgets$Aggregation_includeSharedPeptides == "Yes_Simple_Redistribution"){
                      MagellanNTK::toggleWidget(widget2, rv$steps.enabled['Aggregation'])
                  }else if(rv.widgets$Aggregation_includeSharedPeptides == "Yes_Iterative_Redistribution"){
                    tagList(
                      MagellanNTK::toggleWidget(widget2, rv$steps.enabled['Aggregation']),
                      MagellanNTK::toggleWidget(widget3, rv$steps.enabled['Aggregation'])
                    )
                  }
              ))
    })
    
    
    
    ## Selection of peptides to considers
    MagellanNTK::mod_popover_for_help_server("modulePopover_considerPeptides",
                                title = "Consider",
                                content = HTML(
                                  paste0(
                                    "<strong>• All peptides :</strong>",
                                    " considers all peptides ", "<br>",
                                    "<strong>• N most abundant :</strong>",
                                    " considers only the N top peptides for each protein"
                                  )
                                )
    )
    MagellanNTK::mod_popover_for_help_server("modulePopover_topN",
                                title = "N ",
                                content = HTML(
                                  paste0(
                                    "Number of N top peptides to consider for each protein"
                                  )
                                )
    )
    
    
    output$Aggregation_considerPeptides_ui <- renderUI({
      widget <- radioButtons(ns("Aggregation_considerPeptides"), 
                             MagellanNTK::mod_popover_for_help_ui(ns("modulePopover_considerPeptides")),
                             choices = c("All peptides" = "allPeptides",
                                         "N most abundant" = "topN"),
                             selected = rv.widgets$Aggregation_considerPeptides
      )
      widget2 <- numericInput(ns("Aggregation_topN"),
                              MagellanNTK::mod_popover_for_help_ui(ns("modulePopover_topN")),
                              value = rv.widgets$Aggregation_topN,
                              min = 0,
                              step = 1,
                              width = "100px"
      )

      tagList(div(style = "display: inline-block; margin-right: 35px;", 
        MagellanNTK::toggleWidget(widget, rv$steps.enabled['Aggregation'])),
        div(style = "display: inline-block; vertical-align: top;",
            if(rv.widgets$Aggregation_considerPeptides == "topN"){
                 MagellanNTK::toggleWidget(widget2, rv$steps.enabled['Aggregation'])
              }
      ))
    })
    
    ## Selection of aggregation function
    MagellanNTK::mod_popover_for_help_server("modulePopover_operator",
                                title = "Function",
                                content = HTML(
                                  paste0(
                                    "Function to use for quantitative data aggregation"
                                  )
                                )
    )
    output$Aggregation_operator_ui <- renderUI({
      widget <- radioButtons(ns("Aggregation_operator"), 
                             MagellanNTK::mod_popover_for_help_ui(ns("modulePopover_operator")),
                       choices = c("Sum" = "Sum", 
                  "Mean" = "Mean", 
                  "Median" =  "Median", 
                  "medianPolish" = "medianPolish", 
                  "robustSummary" = "robustSummary"),
                       selected = rv.widgets$Aggregation_operator
        )
        
        
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Aggregation'])
    })
    
    ## Selection of rowData columns
    MagellanNTK::mod_popover_for_help_server("modulePopover_addRowData",
                                title = "RowData columns to aggregate",
                                content = HTML(
                                  paste0(
                                    "Selection of column from rowData to be aggregated"
                                  )
                                )
    )
    output$Aggregation_addRowData_ui <- renderUI({
      widget <- selectInput(ns("Aggregation_addRowData"), 
                            MagellanNTK::mod_popover_for_help_ui(ns("modulePopover_addRowData")),
                            colnames(SummarizedExperiment::rowData(last_assay(rv$dataIn))),
                            selected = rv.widgets$Aggregation_addRowData,
                            multiple = TRUE
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Aggregation'])
    })
    
      
    ### Aggregation done message
    output$Aggregation_AggregationDone_ui <- renderUI({
      req(rv.custom$temp.aggregate)
      
      if (!is.null(rv.custom$temp.aggregate$issues) &&
          length(rv.custom$temp.aggregate$issues) > 0) {
        .style <- "color: red;"
        txt <- "The aggregation process did not succeed because some sets of peptides contains missing values and quantitative values at the same time."
      } else {
        txt <- "Aggregation done"
        .style <- ""
      }
      tags$h3(style = .style, txt)
    })
    
    
    ### Table with informations
    ## About peptides
    aggregationStatsPept <- reactive({
      req(rv$dataIn)
      
      for (k in 1:length(rv$dataIn)){
        if (DaparToolshed::typeDataset(rv$dataIn[[k]]) != "protein")
          i_datapept <- k
      }
      res <- DaparToolshed::getProteinsStats(SummarizedExperiment::rowData(rv$dataIn[[i_datapept]])[['adjacencyMatrix']])
      
      rv.custom$AggregProtStatsPept$desc <- c(
        "Total number of peptides",
        "Number of specific peptides",
        "Number of shared peptides"
      )
      rv.custom$AggregProtStatsPept$nb <- c(
        res$nbPeptides,
        res$nbSpecificPeptides,
        res$nbSharedPeptides
      )
      rv.custom$AggregProtStatsPept$percent <- round(rv.custom$AggregProtStatsPept$nb*100/rv.custom$AggregProtStatsPept$nb[1], 2)
      
      df <- as.data.frame(rv.custom$AggregProtStatsPept)
      names(df) <- c("Description", 
                     "Count",
                     "Percentage")
      df
    })
    
    ## About proteins
    aggregationStatsProt <- reactive({
      req(rv$dataIn)
      
      for (k in 1:length(rv$dataIn)){
        if (DaparToolshed::typeDataset(rv$dataIn[[k]]) != "protein")
          i_datapept <- k
      }
      res <- DaparToolshed::getProteinsStats(SummarizedExperiment::rowData(rv$dataIn[[i_datapept]])[['adjacencyMatrix']])
      
      rv.custom$AggregProtStatsProt$desc <- c(
        "Total number of proteins",
        "Number of proteins with only specific peptides",
        "Number of proteins with only shared peptides",
        "Number of proteins with both specific and shared peptides"
      )
      rv.custom$AggregProtStatsProt$nb <- c(
        res$nbProt,
        length(res$protOnlyUniquePep),
        length(res$protOnlySharedPep),
        length(res$protMixPep)
      )
      rv.custom$AggregProtStatsProt$percent <- round(rv.custom$AggregProtStatsProt$nb*100/rv.custom$AggregProtStatsProt$nb[1], 2)
      
      df <- as.data.frame(rv.custom$AggregProtStatsProt)
      names(df) <- c("Description", 
                     "Count",
                     "Percentage")
      df
    })
    
    MagellanNTK::format_DT_server('dtaggregationStatsPept',
                                  reactive({aggregationStatsPept()})
    )
    MagellanNTK::format_DT_server('dtaggregationStatsProt',
                                  reactive({aggregationStatsProt()})
    )
    
    ## Table UI
    output$Aggregation_aggregationStats_ui <- renderUI({
      tagList(
        MagellanNTK::format_DT_ui(ns('dtaggregationStatsPept')),
        MagellanNTK::format_DT_ui(ns('dtaggregationStatsProt'))
      )
    })
    
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Aggregation', btnEvents()))
      req(dataIn())
      req(rv.custom$nbEmptyLines == 0)
      
      withProgress(message = "", detail = "", value = 0, {
        incProgress(0.5, detail = "Aggregation processing")
      # Do some stuff
      rv.custom$temp.aggregate <- DaparToolshed::RunAggregation(
        qf = rv$dataIn,
        includeSharedPeptides = rv.widgets$Aggregation_includeSharedPeptides,
        operator = rv.widgets$Aggregation_operator,
        considerPeptides = rv.widgets$Aggregation_considerPeptides,
        adjMatrix = 'adjacencyMatrix',
        ponderation = rv.widgets$Aggregation_ponderation,
        n = rv.widgets$Aggregation_topN,
        aggregated_col = rv.widgets$Aggregation_addRowData,
        max_iter = rv.widgets$Aggregation_maxiter
        )
     
      })
      
      if(is.null(rv.custom$temp.aggregate$issues)){
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status['Aggregation'] <- MagellanNTK::stepStatus$VALIDATED
      } else {
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status['Aggregation'] <- MagellanNTK::stepStatus$VALIDATED
      }
    })
    
    
    # <<< END ------------- Code for step 1 UI---------------

    # >>> START ------------- Code for step 2 UI---------------
    output$Save <- renderUI({
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(),
        content = uiOutput(ns('dl_ui'))
      )
    })
    
    output$dl_ui <- renderUI({
      req(rv$steps.status['Save'] == MagellanNTK::stepStatus$VALIDATED)
      req(config@mode == 'process')
      
      MagellanNTK::download_dataset_ui(ns('createQuickLink'))
    })
    
    # output$Save_btn_validate_ui <- renderUI({
    #   tagList(
    #     MagellanNTK::toggleWidget( 
    #       actionButton(ns("Save_btn_validate"), "Validate step",
    #         class = "btn-success"),
    #       rv$steps.enabled['Save']
    #     ),
    #     if (config@mode == 'process' && 
    #         rv$steps.status['Save'] == MagellanNTK::stepStatus$VALIDATED) {
    #       download_dataset_ui(ns('createQuickLink'))
    #     }
    #   )
    #   
    # })
    
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Save', btnEvents()))
      
      req(rv.custom$temp.aggregate)
      
      rv.custom$history[['Aggregation_includeSharedPeptides']] <- rv.widgets$Aggregation_includeSharedPeptides
      rv.custom$history[['Aggregation_operator']] <- rv.widgets$Aggregation_operator
      rv.custom$history[['Aggregation_considerPeptides']] <- rv.widgets$Aggregation_considerPeptides
      rv.custom$history[['Aggregation_proteinId']] <- rv.widgets$Aggregation_proteinId
      rv.custom$history[['Aggregation_topN']] <- as.numeric(rv.widgets$Aggregation_topN)
      rv.custom$history[['Aggregation_addRowData']] <- rv.widgets$Aggregation_addRowData
      
      paramshistory(rv.custom$temp.aggregate[[length(rv.custom$temp.aggregate)]]) <- rv.custom$history
      
      rv$dataIn <- rv.custom$temp.aggregate 
      names(rv$dataIn)[length(rv$dataIn)] <- 'Aggregation'
      
      ###TEMPORARY !!!!!!!!!
      rv$dataIn <- QFeatures::zeroIsNA(rv$dataIn, length(rv$dataIn))
      rv$dataIn <- QFeatures::filterNA(rv$dataIn, pNA = 0.99, length(rv$dataIn))
      SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])[which(is.na(SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])))] <- 0
      ###!!!!!!!!!
      
      # DO NOT MODIFY THE THREE FOLLOWING LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Save'] <- MagellanNTK::stepStatus$VALIDATED
      
      Prostar2::download_dataset_server('createQuickLink', 
        dataIn = reactive({rv$dataIn}))
      
    })
    # <<< END ------------- Code for step 2 UI---------------

    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  }
  )
}