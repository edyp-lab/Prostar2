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
#' library(bs4Dash)
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
#' 
PipelinePeptide_Aggregation_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1})
){
  
  requireNamespace('DaparToolshed')
  
  
  # Define default selected values for widgets
  # This is only for simple workflows
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
    
    
    # Add a new observer to remoteReset to complete the default behaviour
    observeEvent(remoteReset(), {
      
    })
    
    # >>>
    # >>> START ------------- Code for Description UI---------------
    # >>> 
    
    
    output$Description <- renderUI({
      # file <- normalizePath(file.path(session$userData$workflow.path, 
      #   'md', paste0(id, '.md')))
      
      file <- normalizePath(file.path(
        system.file('workflow', package = 'Prostar2'),
        unlist(strsplit(id, '_'))[1], 
        'md', 
        paste0(id, '.md')))
      
      
      tagList(
        # In this example, the md file is found in the extdata/module_examples directory
        # but with a real app, it should be provided by the package which
        # contains the UI for the different steps of the process module.
        # Insert validation button
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
      widget <- actionButton(ns("Description_btn_validate"),
        "Start",
        class = "btn-success")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Description'])
    })
    
    
    observeEvent(input$Description_btn_validate, {
      rv$dataIn <- dataIn()
      
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- stepStatus$VALIDATED
    })
    
    
    
    # >>>
    # >>> START ------------- Code for Aggregation UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI ------------------------------------
    output$Aggregation <- renderUI({
      shinyjs::useShinyjs()
      
      .data <- last_assay(rv$dataIn)
      m <- DaparToolshed::match.metacell(
        DaparToolshed::qMetacell(.data),
        pattern = c("Missing", "Missing POV", "Missing MEC"),
        level = DaparToolshed::typeDataset(.data)
      )
      NA.count <- length(which(m))
      
      # uiOutput for all widgets in this UI
      # This part is mandatory
      # The renderUlength(rv$dataIn) function of each widget is managed by MagellanNTK
      # The dev only have to define a reactive() function for each
      # widget he want to insert
      # Be aware of the naming convention for ids in uiOutput()
      # For more details, please refer to the dev document.
      fluidRow(
        column(4,
               wellPanel(
               uiOutput(ns('Aggregation_chooseProteinId_ui')),
               uiOutput(ns('Aggregation_includeSharedPeptides_ui')),
               tags$hr(),
               uiOutput(ns('Aggregation_considerPeptides_ui')),
               tags$hr(),
               uiOutput(ns('Aggregation_operator_ui')),
               tags$hr(),
               uiOutput(ns('Aggregation_addRowData_ui')),
               uiOutput(ns('Aggregation_btn_validate_ui'))
               )
        ), 
        column(8,
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
    # >>> START: Definition of the widgets
    
    
    output$Aggregation_chooseProteinId_ui <- renderUI({
      
      if (!is.null(DaparToolshed::parentProtId(last_assay(rv$dataIn)))) {
        return(NULL)
      }
      
      .choices <- colnames(rowData(last_assay(rv$dataIn)))
      widget <- selectInput(ns("Aggregation_proteinId"),
        "Choose the protein ID",
        choices = c("None", .choices),
        selected = rv.widgets$Aggregation_proteinId
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Aggregation'] )
    })
    
    
    ## Selection of how to handle shared peptides
    mod_popover_for_help_server("modulePopover_includeShared",
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
    mod_popover_for_help_server("modulePopover_ponderation",
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
    
    mod_popover_for_help_server("modulePopover_maxiter",
                                title = "Max iteration",
                                content = HTML(
                                  paste0(
                                    "Maximum number of iteration."
                                  )
                                )
    )
    
    output$Aggregation_includeSharedPeptides_ui <- renderUI({
      widget <- radioButtons(ns("Aggregation_includeSharedPeptides"), 
                             mod_popover_for_help_ui(ns("modulePopover_includeShared")),
                             choices = c("No" = "No",
                                         "Yes (as protein specific)" = "Yes_As_Specific",
                                         "Yes (simple redistribution)" = "Yes_Simple_Redistribution",
                                         "Yes (iterative redistribution)" = "Yes_Iterative_Redistribution"),
                             selected = rv.widgets$Aggregation_includeSharedPeptides
      )
      widget2 <- radioButtons(ns("Aggregation_ponderation"), 
                              mod_popover_for_help_ui(ns("modulePopover_ponderation")),
                              choices = c("Global" = "Global",
                                          "Condition" = "Condition",
                                          "Sample" = "Sample"),
                              selected = rv.widgets$Aggregation_ponderation
      )
      widget3 <- numericInput(ns("Aggregation_maxiter"),
                              mod_popover_for_help_ui(ns("modulePopover_maxiter")),
                              value = rv.widgets$Aggregation_maxiter,
                              min = 1,
                              step = 1,
                              width = "100px"
      )
        
      
      tagList(div(style = "display: inline-block; margin-right: 35px;", 
                  MagellanNTK::toggleWidget(widget, rv$steps.enabled['Aggregation'])),
              div(style = "display: inline-block; vertical-align: top;",
                  if(rv.widgets$Aggregation_includeSharedPeptides %in% c("Yes_Simple_Redistribution", "Yes_Iterative_Redistribution")){
                    tagList(
                      MagellanNTK::toggleWidget(widget2, rv$steps.enabled['Aggregation']),
                      MagellanNTK::toggleWidget(widget3, rv$steps.enabled['Aggregation'])
                    )
                  }
              ))
    })
    
    
    
    ## Selection of peptides to considers
    mod_popover_for_help_server("modulePopover_considerPeptides",
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
    mod_popover_for_help_server("modulePopover_topN",
                                title = "N ",
                                content = HTML(
                                  paste0(
                                    "Number of N top peptides to consider for each protein"
                                  )
                                )
    )
    
    
    output$Aggregation_considerPeptides_ui <- renderUI({
      widget <- radioButtons(ns("Aggregation_considerPeptides"), 
                             mod_popover_for_help_ui(ns("modulePopover_considerPeptides")),
                             choices = c("All peptides" = "allPeptides",
                                         "N most abundant" = "topN"),
                             selected = rv.widgets$Aggregation_considerPeptides
      )
      widget2 <- numericInput(ns("Aggregation_topN"),
                              mod_popover_for_help_ui(ns("modulePopover_topN")),
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
    mod_popover_for_help_server("modulePopover_operator",
                                title = "Function",
                                content = HTML(
                                  paste0(
                                    "Function to use for quantitative data aggregation"
                                  )
                                )
    )
    output$Aggregation_operator_ui <- renderUI({
      widget <- radioButtons(ns("Aggregation_operator"), 
                       mod_popover_for_help_ui(ns("modulePopover_operator")),
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
    mod_popover_for_help_server("modulePopover_addRowData",
                                title = "RowData columns to aggregate",
                                content = HTML(
                                  paste0(
                                    "Selection of column from rowData to be aggregated"
                                  )
                                )
    )
    output$Aggregation_addRowData_ui <- renderUI({
      widget <- selectInput(ns("Aggregation_addRowData"), 
                            mod_popover_for_help_ui(ns("modulePopover_addRowData")),
                            colnames(rowData(last_assay(rv$dataIn))),
                            selected = rv.widgets$Aggregation_addRowData,
                            multiple = TRUE
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Aggregation'])
    })
    
    # output$downloadAggregationIssues <- downloadHandler(
    #   filename = "aggregation_issues.txt",
    #   content = function(file) {
    #     tmp.peptides <- lapply(
    #       rv.custom$temp.aggregate$issues,
    #       function(x) paste0(x, collapse = ",")
    #     )
    #     df <- data.frame(
    #       Proteins = names(rv.custom$temp.aggregate$issues),
    #       Peptides = as.data.frame(do.call(rbind, tmp.peptides))
    #     )
    #     colnames(df) <- c("Proteins", "Peptides")
    #     write.table(df,
    #       file = file,
    #       row.names = FALSE,
    #       quote = FALSE,
    #       sep = "\t"
    #     )
    #   }
    # )
    
    
    # Add_Aggregated_rowData <- reactive({
    #   req('aggregated' %in% names(rv.custom$temp.aggregate))
    #   
    #   # Add aggregated simple columns rowData
    #   total <- 60
    #   
    #   i.agg <- length(rv.custom$temp.aggregate)
    #   i.before.agg <- i.agg - 1
    #   .names <- names(rowData(rv.custom$temp.aggregate[[i.before.agg]]))
    #   .names <- .names[-match(c('qMetacell', 'adjacencyMatrix'), .names)]
    #   
    #   delta <- round(total / length(.names))
    #   cpt <- 10
    #   
    #   withProgress(message = "", detail = "", value = 0, {
    #     incProgress(0.5, detail = "Aggregation in progress")
    #     
    #     for (col.name in .names) {
    #       #browser()
    #       
    #       newCol <- BuildColumnToProteinDataset(
    #         peptideData = rowData((rv.custom$temp.aggregate[[i.agg - 1]])),
    #         matAdj = adjacencyMatrix(rv.custom$temp.aggregate[[i.agg - 1]]),
    #         columnName = col.name,
    #         proteinNames = rownames(rowData((rv.custom$temp.aggregate[[i.agg]])))
    #       )
    #       #browser()
    #       cnames <- colnames(rowData(rv.custom$temp.aggregate[[i.agg]]))
    #       rowData(rv.custom$temp.aggregate[[i.agg]]) <-
    #         data.frame(rowData(rv.custom$temp.aggregate[[i.agg]]), newCol)
    #       
    #       colnames(rowData(rv.custom$temp.aggregate[[i.agg]])) <- c(
    #         cnames,
    #         paste0("agg_", col.name)
    #       )
    #       
    #       cpt <- cpt + delta
    #       incProgress(cpt / 100, detail = paste0("Processing column ", col.name))
    #   }
    #   })
    # })
      
    ### Aggregation done message
    output$Aggregation_AggregationDone_ui <- renderUI({
       req(rv.custom$temp.aggregate)
      print('new test')
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
        if (typeDataset(rv$dataIn[[k]]) != "protein")
          i_datapept <- k
      }
      res <- DaparToolshed::getProteinsStats(rowData(rv$dataIn[[i_datapept]])[['adjacencyMatrix']])
      
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
        if (typeDataset(rv$dataIn[[k]]) != "protein")
          i_datapept <- k
      }
      res <- DaparToolshed::getProteinsStats(rowData(rv$dataIn[[i_datapept]])[['adjacencyMatrix']])
      
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
    
    
    ### Validation button
    output$Aggregation_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Aggregation_btn_validate"),
        "Validate step",
        class = "btn-success")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Aggregation'] )
      
    })
    

    
    observeEvent(input$Aggregation_btn_validate, {

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
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      if(is.null(rv.custom$temp.aggregate$issues)){
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status['Aggregation'] <- stepStatus$VALIDATED
      } else {
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status['Aggregation'] <- stepStatus$VALIDATED
        
        #rv.custom$temp.aggregate <- NULL
      }
})

# <<< END ------------- Code for step 1 UI---------------

    # >>> START ------------- Code for step 3 UI---------------
    output$Save <- renderUI({
      tagList(
        # Insert validation button
        # This line is necessary. DO NOT MODIFY
        uiOutput(ns('Save_btn_validate_ui'))
      )
    })
    
    output$Save_btn_validate_ui <- renderUI({
      tagList(
        MagellanNTK::toggleWidget( 
          actionButton(ns("Save_btn_validate"), "Validate step",
            class = "btn-success"),
          rv$steps.enabled['Save']
        ),
        if (config@mode == 'process' && 
            rv$steps.status['Save'] == stepStatus$VALIDATED) {
          download_dataset_ui(ns('createQuickLink'))
        }
      )
      
    })
    
    
    observeEvent(input$Save_btn_validate, {
      # Do some stuff
      
      req(rv.custom$temp.aggregate)
      
      rv.custom$history[['Aggregation_includeSharedPeptides']] <- as.numeric(rv.widgets$Aggregation_includeSharedPeptides)
      rv.custom$history[['Aggregation_operator']] <- as.numeric(rv.widgets$Aggregation_operator)
      rv.custom$history[['Aggregation_considerPeptides']] <- as.numeric(rv.widgets$Aggregation_considerPeptides)
      rv.custom$history[['Aggregation_proteinId']] <- as.numeric(rv.widgets$Aggregation_proteinId)
      rv.custom$history[['Aggregation_topN']] <- as.numeric(rv.widgets$Aggregation_topN)
      rv.custom$history[['Aggregation_addRowData']] <- rv.widgets$Aggregation_addRowData
      
      paramshistory(rv.custom$temp.aggregate[[length(rv.custom$temp.aggregate)]]) <- rv.custom$history
      
      rv$dataIn <- rv.custom$temp.aggregate 
      
      # DO NOT MODIFY THE THREE FOLLOWING LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Save'] <- stepStatus$VALIDATED
      Prostar2::download_dataset_server('createQuickLink', 
        dataIn = reactive({rv$dataIn}))
      
    })
    # <<< END ------------- Code for step 3 UI---------------

    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  }
  )
}