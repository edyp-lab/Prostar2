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
#' data(Exp1_R25_pept, package = "DaparToolshedData")
#' obj <- Exp1_R25_pept
#' # Simulate imputation of missing values
#' obj <- NAIsZero(obj, 1)
#' path <- system.file('workflow/PipelinePeptide', package = 'Prostar2')
#' shiny::runApp(workflowApp("PipelinePeptide_Aggregation", path, dataIn = obj))
#' }
#' 
#' @rdname PipelinePeptide
#' @export
#' 
PipelinePeptide_Aggregation_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelinePeptide_Aggregation',
    mode = 'process',
    steps = c('Aggregation', 'Add metadata'),
    mandatory = c(FALSE, FALSE)
  )
}


#' @param id xxx
#' 
#' @rdname PipelinePeptide
#' 
#' @author Samuel Wieczorek
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
    Aggregation_includeSharedPeptides = "Yes2",
    Aggregation_operator = "Mean",
    Aggregation_considerPeptides = "allPeptides",
    Aggregation_proteinId = "None",
    Aggregation_topN = 3,
    Aggregation_filterProtAfterAgregation = NULL,
    Addmetadata_columnsForProteinDataset = NULL
  )
  
  rv.custom.default.values <- list(
    temp.aggregate = NULL,
    AggregProtStats = NULL,
    X = NULL,
    X.split = NULL,
    X.shared = NULL,
    X.unique = NULL
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
      
      ## Build the adjacency matrix if they are not present
      .data <- last_assay(rv$dataIn)
      if (!("adjacencyMatrix" %in% names(rowData(.data)))){
        QFeatures::adjacencyMatrix(rv$dataIn[[length(rv$dataIn)]]) <- BuildAdjacencyMatrix(.data, DaparToolshed::parentProtId(.data), FALSE)
      }
      rv.custom$X <- QFeatures::adjacencyMatrix(rv$dataIn[[length(rv$dataIn)]])
      rv.custom$X.split <- DaparToolshed::splitAdjacencyMat(rv.custom$X)
      rv.custom$X.shared <- rv.custom$X.split$Xshared
      rv.custom$X.unique <- rv.custom$X.split$Xspec

      print("Initialization finished")
      
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
      
      .style <- "display:inline-block; vertical-align: middle; padding-right: 20px;"
      .data <- last_assay(rv$dataIn)
      m <- DaparToolshed::match.metacell(
        DaparToolshed::qMetacell(.data),
        pattern = c("Missing", "Missing POV", "Missing MEC"),
        level = DaparToolshed::typeDataset(.data)
      )
      NA.count <- length(which(m))
      
      wellPanel(
        # uiOutput for all widgets in this UI
        # This part is mandatory
        # The renderUlength(rv$dataIn) function of each widget is managed by MagellanNTK
        # The dev only have to define a reactive() function for each
        # widget he want to insert
        # Be aware of the naming convention for ids in uiOutput()
        # For more details, please refer to the dev document.
        tagList(
          div(
              div(style = .style,
                uiOutput(ns('Aggregation_warning_ui'))
              ),
              div(style = .style,
                uiOutput(ns('Aggregation_chooseProteinId_ui'))
              ),
              
              div(style = .style,
                uiOutput(ns('Aggregation_includeSharedPeptides_ui'))
              ),
              
              div(style = .style,
                uiOutput(ns('Aggregation_considerPeptides_ui'))
              ),
              
              div(style = .style,
                uiOutput(ns('Aggregation_operator_ui'))
              )
            ),
            div(style = .style,
              uiOutput(ns("Aggregation_btn_validate_ui"))
            ),
          div(style = .style,
            uiOutput(ns("Aggregation_AggregationDone_ui"))
          ),
          div(style = .style,
            DT::dataTableOutput(ns("Aggregation_aggregationStats_ui"))
          )
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
    
    
    
    output$Aggregation_includeSharedPeptides_ui <- renderUI({
      
      #popover_for_help_ui("modulePopover_includeShared")
      
      widget <- radioButtons(ns("Aggregation_includeSharedPeptides"), NULL,
        choices = c("No" = "No",
          "Yes (as protein specific)" = "Yes1",
          "Yes (redistribution)" = "Yes2"
        ),
        selected = rv.widgets$Aggregation_includeSharedPeptides
      )
      
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Aggregation'])
    })
    
    
    
    output$Aggregation_considerPeptides_ui <- renderUI({
      widget <- radioButtons(ns("Aggregation_considerPeptides"), "Consider",
        choices = c("all peptides" = "allPeptides",
          "N most abundant" = "onlyN"),
        selected = rv.widgets$Aggregation_considerPeptides
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Aggregation'])
    })
    
    
    
    output$Aggregation_operator_ui <- renderUI({
      
      .style <- "display:inline-block; vertical-align: middle; padding-right: 20px;"
      
      choice <- if (rv.widgets$Aggregation_includeSharedPeptides %in% c("No", "Yes1")) {
        c("Mean" = "Mean", "Sum" = "Sum")
      } else if (rv.widgets$Aggregation_includeSharedPeptides == "Yes2"){
        c("Mean" = "Mean")
      }
      
      widget <- tagList(
        div(
          div(style = .style, 
            radioButtons(ns("Aggregation_operator"), "Operator",
            choices = choice,
            selected = rv.widgets$Aggregation_operator
          )),
          div(style = .style, 
            if(rv.widgets$Aggregation_considerPeptides == "onlyN")
              
              numericInput(ns("Aggregation_topN"),
                "N",
                value = rv.widgets$Aggregation_topN,
                min = 0,
                step = 1,
                width = "100px"
              )
          ))
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
    

    ########################################################
    RunAggregation <- reactive({
      ll.agg <- NULL
      
      req(QFeatures::adjacencyMatrix(last_assay(rv$dataIn)))
      rv.widgets$Aggregation_includeSharedPeptides
      rv.widgets$Aggregation_operator
      rv.widgets$Aggregation_considerPeptides
      rv.widgets$Aggregation_topN
      
      
      withProgress(message = "", detail = "", value = 0, {
        incProgress(0.5, detail = "Aggregation in progress")
        
        # Update the adjacency matrix to use
        X <- NULL
        if (rv.widgets$Aggregation_includeSharedPeptides %in% c("Yes2", "Yes1"))
          X = rv.custom$X.shared
        else
          X = rv.custom$X.unique
        
        i.last <- length(rv$dataIn)
        #adjacencyMatrix(rv$dataIn[[i.last]]) <- X
        
        feat1 <- aggregateFeatures4Prostar(
          object = rv$dataIn,
          i = i.last,
          name = 'aggregated',
          fcol = 'adjacencyMatrix',
          fun = 'colSumsMat')
        
        ll.agg <- NULL
        
      })
      
      return(ll.agg)
    })
    
    
    output$Aggregation_AggregationDone_ui <- renderUI({
       req(rv.custom$temp.aggregate)
      
      if (!is.null(rv.custom$temp.aggregate$issues) &&
          length(rv.custom$temp.aggregate$issues) > 0) {
        .style <- "color: red;"
        txt <- "The aggregation process did not succeed because some sets of
    peptides contains missing values and quantitative
       values at the same time."
      } else {
        txt <- "Aggregation done"
        .style <- ""
      }
      tags$h3(style = .style, txt)
    })
    

    
    
    output$Aggregation_aggregationStats_ui <- DT::renderDataTable(server = TRUE, {
      req(rv.custom$X)
      req(rv.widgets$Aggregation_proteinId != "None")
      
      res <- DaparToolshed::GetProteinsStats(rv.custom$X.shared)
      
      rv.custom$AggregProtStats$nb <- c(
        res$nbPeptides,
        res$nbSpecificPeptides,
        res$nbSharedPeptides,
        res$nbProt,
        length(res$protOnlyUniquePep),
        length(res$protOnlySharedPep),
        length(res$protMixPep)
      )
      
      df <- as.data.frame(rv.custom$AggregProtStats)
      names(df) <- c("Description", "Value")
      
      DT::datatable(df,
        escape = FALSE,
        rownames = FALSE,
        extensions = c("Scroller"),
        option = list(
          initComplete = initComplete(),
          dom = "rt",
          autoWidth = TRUE,
          ordering = F,
          columnDefs = list(
            list(width = "150px", targets = 0),
            list(width = "100px", targets = 1)
          )
        )
      )
      
    })
    
    
    
    
    
    output$Aggregation_btn_validate_ui <- renderUI({
      widget <- actionButton(ns("Aggregation_btn_validate"),
        "Validate step",
        class = "btn-success")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Aggregation'] )
      
    })
    
    observeEvent(input$Aggregation_btn_validate, {
      # Do some stuff
      rv.custom$temp.aggregate <- RunAggregation()
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status['Aggregation'] <- is.null(rv.custom$temp.aggregate$issues)
      #rv$steps.status['Aggregation'] <- stepStatus$VALIDATED
    })
    
    # <<< END ------------- Code for step 1 UI---------------
    
    
    
    
    
    # >>>
    # >>> START ------------- Code for Aggregation UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI -----------------
    output$Addmetadata <- renderUI({
      shinyjs::useShinyjs()
      
      wellPanel(
        # uiOutput for all widgets in this UI
        # This part is mandatory
        # The renderUlength(rv$dataIn) function of each widget is managed by MagellanNTK
        # The dev only have to define a reactive() function for each
        # widget he want to insert
        # Be aware of the naming convention for ids in uiOutput()
        # For more details, please refer to the dev document.
        
        uiOutput("Addmetadata_displayNbPeptides_ui"),
        #popover_for_help_ui("modulePopover_colsForAggreg"),
        uiOutput("Addmetadata_columnsForProteinDataset_ui")
        )
    })
    

    output$Addmetadata_displayNbPeptides_ui <- renderUI({
      
      
    })
    

    
    output$Addmetadata_columnsForProteinDataset_ui <- renderUI({
      req(rv$dataIn)
        
      .data <- last_assay(rv$dataIn)
        ind <- match(
          colnames(qMetacell(.data)), .colnames)
        .colnames <- colnames(rowData(.data))
        choices <- setNames(nm = .colnames[-ind])
        
        widget <- selectInput(ns("Addmetadata_columnsForProteinDataset"),
                label = "",
                choices = choices,
                multiple = TRUE,
                width = "200px",
                selectize = TRUE
              )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Addmetadata'] )
    })
    
    
    


output$Addmetadata_btn_validate_ui <- renderUI({
  widget <- actionButton(ns("Addmetadata_btn_validate"),
    "Validate step",
    class = "btn-success")
  MagellanNTK::toggleWidget(widget, rv$steps.enabled['Addmetadata'] )
  
})

observeEvent(input$Addmetadata_btn_validate, {
  # Do some stuff
  rv.custom$temp.aggregate <- RunAggregation()
  
  # DO NOT MODIFY THE THREE FOLLOWINF LINES
  dataOut$trigger <- MagellanNTK::Timestamp()
  dataOut$value <- NULL
  rv$steps.status['Aggregation'] <- stepStatus$VALIDATED
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
      
      # req(rv.custom$temp.aggregate$obj.prot)
      # req(is.null(rv.custom$temp.aggregate$issues))
      # 
      # .data <- last_assay(rv$dataIn)
      # req(rv.custom$X)
      # 
      # 
      # isolate({
      #   withProgress(message = "", detail = "", value = 0, {
      #     X <- NULL
      #     if (rv.widgets$Aggregation_includeSharedPeptides %in% c("Yes2", "Yes1")) {
      #       X <- rv.custom$X.shared
      #     } else {
      #       X <- rv.custom$X.unique
      #     }
      #     
      #     total <- 60
      #     
      #     delta <- round(total / length(rv.widgets$Addmetadata_columnsForProteinDataset))
      #     cpt <- 10
      #     
      #     for (c in rv.widgets$Addmetadata_columnsForProteinDataset) {
      #       newCol <- BuildColumnToProteinDataset(
      #         peptideData = rowData(.data),
      #         matAdj = rv.custom$X,
      #         columnName = c,
      #         proteinNames = rownames(rowData((rv.custom$temp.aggregate$obj.prot)))
      #       )
      #       cnames <- colnames(rowData(rv.custom$temp.aggregate$obj.prot))
      #       rowData(rv.custom$temp.aggregate$obj.prot) <-
      #         data.frame(rowData(rv.custom$temp.aggregate$obj.prot), newCol)
      #       
      #       colnames(rowData(rv.custom$temp.aggregate$obj.prot)) <- c(
      #         cnames,
      #         paste0("agg_", c)
      #       )
      #       
      #       cpt <- cpt + delta
      #       incProgress(cpt / 100, detail = paste0("Processing column ", c))
      #     }
      # 
      # rv.custom$history[['Aggregation_includeSharedPeptides']] <- as.numeric(rv.widgets$Aggregation_includeSharedPeptides)
      # rv.custom$history[['Aggregation_operator']] <- as.numeric(rv.widgets$Aggregation_operator)
      # rv.custom$history[['Aggregation_considerPeptides']] <- as.numeric(rv.widgets$Aggregation_considerPeptides)
      # rv.custom$history[['Aggregation_proteinId']] <- as.numeric(rv.widgets$Aggregation_proteinId)
      # rv.custom$history[['Aggregation_topN']] <- as.numeric(rv.widgets$Aggregation_topN)
      # rv.custom$history[['Aggregation_filterProtAfterAgregation']] <- as.numeric(rv.widgets$Aggregation_filterProtAfterAgregation)
      # rv.custom$history[['Addmetadata_columnsForProteinDataset']] <- as.numeric(rv.widgets$Addmetadata_columnsForProteinDataset)
      # 
      # paramshistory(new.dataset) <- rv.custom$history
      # 
      # new.dataset <- rv$temp.aggregate$obj.prot
      # 
      # new.name <- paste0("Aggregated", ".", TypeOfDataset(new.dataset))
      #   
      # rv$dataIn <- QFeatures::addAssay(rv$dataIn, new.dataset, new.name)
      
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