#' @title PipelineProtein Normalization module
#'
#' @description
#' This module contains the normalization step of the protein pipeline.
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
#'   Prostar2("PipelineProtein_Normalization")
#' }
#' 
#' @name PipelineProtein_Normalization
#' 
#' @importFrom stats setNames rnorm
#' @import omXplore
#' @importFrom shinyjs hidden useShinyjs toggle
#' @importFrom shinyFeedback showFeedbackWarning hideFeedback
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' 
#' @return An instance of the class `MultiAssayExperiment`
#' 
NULL


#' @rdname PipelineProtein_Normalization
#' @export
#' 
PipelineProtein_Normalization_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelineProtein_Normalization',
    mode = 'process',
    steps = c('Normalization'),
    mandatory = c(FALSE)
    )
}


#' @rdname PipelineProtein_Normalization
#' @export
#' 
PipelineProtein_Normalization_ui <- function(id){
  ns <- NS(id)
  shinyjs::useShinyjs()
}


#' @rdname PipelineProtein_Normalization
#' @export
#' 
PipelineProtein_Normalization_server <- function(id,
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
    Normalization_method = "None",
    Normalization_type = "overall",
    Normalization_spanLOESS = 0.7,
    Normalization_quantile = 0.15,
    Normalization_varReduction = FALSE,
    Normalization_sync = FALSE
  )
  
  rv.custom.default.values <- list(
    tmp.dataset = NULL,
    history = MagellanNTK::InitializeHistory(),
    selectProt = reactive({NULL}),
    result_open_dataset = reactive({NULL})
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
    # output$open_dataset_UI <- renderUI({
    #   req(session$userData$runmode == 'process')
    #   req(is.null(dataIn()))
    #   req(NULL)
    #  
    #   rv.custom$result_open_dataset <- MagellanNTK::open_dataset_server(
    #     id = "open_dataset",
    #     class = 'QFeatures',
    #     extension = "qf",
    #     remoteReset = reactive({remoteReset()})
    #   )
    #   
    # MagellanNTK::open_dataset_ui(id = ns("open_dataset"))
    # })
    
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
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Description', btnEvents()))
      #rv.custom$result_open_dataset()$dataset
      req(dataIn())
      
      rv$dataIn <- dataIn()

      if(!is.null(rv.custom$result_open_dataset()$dataset))
        rv$dataIn <- rv.custom$result_open_dataset()$dataset

      shiny::withProgress(message = paste0("xxx process", id), {
        shiny::incProgress(0.5)
        
        rv.custom$dataIn <- rv$dataIn
        
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status['Description'] <- MagellanNTK::stepStatus$VALIDATED
      })
    })
    
    
    ###########################################################################-
    #
    #-----------------------------NORMALIZATION---------------------------------
    #
    ###########################################################################-
    output$Normalization <- renderUI({
      shinyjs::useShinyjs()
      .style <- "display:inline-block; vertical-align: middle; 
      padding-right: 20px;"

      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          uiOutput(ns("Normalization_method_ui")),
          shinyjs::hidden(uiOutput(ns('Normalization_type_ui'))),
          shinyjs::hidden(uiOutput(ns('Normalization_spanLOESS_ui'))),
          uiOutput(ns("Normalization_quantile_ui")),
          uiOutput(ns("Normalization_varReduction_ui")),
          uiOutput(ns('tracking')),
          shinyjs::hidden(uiOutput(ns("Normalization_sync_ui")))
          ),
        content = tagList(fluidRow(
          column(6,
            omXplore::omXplore_density_ui(ns("densityPlot_Norm"))),
          column(6,
            omXplore::omXplore_intensity_ui(ns("boxPlot_Norm")))
        ),
        uiOutput(ns('comparisonPlot'))
        )
      )
    })
    
    #### _sidebar -----
    output$Normalization_method_ui <- renderUI({
      widget <- selectInput(
        ns('Normalization_method'),
        "Method",
        choices = setNames(nm = c("None", DaparToolshed::normalizeMethods())),
        selected = rv.widgets$Normalization_method,
        width = '220px'
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Normalization"])
    })
    
    output$Normalization_type_ui <- renderUI({
      widget <- selectInput(ns('Normalization_type'),
                            "Type",
                            choices = stats::setNames(
                              nm = c("overall", "within conditions")),
                            selected = rv.widgets$Normalization_type,
                            width = '180px')
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Normalization"])
    })
    
    output$Normalization_spanLOESS_ui <- renderUI({
      widget <- textInput(
        ns('Normalization_spanLOESS'),
        'Span',
        value = rv.widgets$Normalization_spanLOESS,
        width = '100px')
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Normalization"])
    })
    
    output$Normalization_quantile_ui <- renderUI({
      req(rv.widgets$Normalization_method == "QuantileCentering")
      
      widget <- textInput(
        ns('Normalization_quantile'),
        "Normalization quantile",
        value = rv.widgets$Normalization_quantile,
        width = '100px')
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Normalization"])
    })
    
    output$Normalization_varReduction_ui <- renderUI({
      req(rv.widgets$Normalization_method == "MeanCentering")
      
      widget <- checkboxInput(
        ns('Normalization_varReduction'),
        "Include variance reduction",
        value = rv.widgets$Normalization_varReduction
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Normalization"])
    })
    
    output$Normalization_sync_ui <- renderUI({
      widget <- checkboxInput(
        ns('Normalization_sync'),
        "Synchronise with selection above",
        value = rv.widgets$Normalization_sync
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Normalization"])
    })
    
    output$tracking <- renderUI({
      req(rv.widgets$Normalization_method %in% c('QuantileCentering', 'MeanCentering', 'SumByColumns'))
      widget <-  omXplore::plots_tracking_ui(ns("tracker"))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Normalization"])
    })
    
    selectProt <- omXplore::plots_tracking_server(
      id = "tracker",
      dataIn = reactive({rv.custom$dataIn[[length(rv.custom$dataIn)]]}),
      remoteReset = reactive({remoteReset()})
    )
    
    observeEvent(rv.widgets$Normalization_method, {
      req(rv.widgets$Normalization_method)
      req(rv.custom$dataIn)
      shinyjs::toggle("Normalization_btn_validate",
                      condition = rv.widgets$Normalization_method != "None")
      
      shinyjs::toggle("Normalization_spanLOESS_ui",
                      condition = rv.widgets$Normalization_method == "LOESS")
      
      .choice <- c("QuantileCentering", "MeanCentering", "SumByColumns", 
                   "LOESS", "vsn")
      
      shinyjs::toggle("Normalization_type_ui",
                      condition = (rv.widgets$Normalization_method %in% .choice)
      )
      
      cond <- S4Vectors::metadata(rv.custom$dataIn[[length(rv.custom$dataIn)]])[['typeDataset']] == "protein"
      
      .meths <- DaparToolshed::normalizeMethods('withTracking')
      trackAvailable <- rv.widgets$Normalization_method %in% .meths
      shinyjs::toggle("Normalization_sync_ui",
                      condition = cond && trackAvailable)
    })
    
    #### _content -----
    omXplore::omXplore_intensity_server("boxPlot_Norm",
      dataIn = reactive({rv.custom$dataIn}),
      i = reactive({length(rv.custom$dataIn)}),
      track.indices = reactive({selectProt()$indices}),
      remoteReset = reactive({remoteReset()}),
      is.enabled = reactive({rv$steps.enabled["Normalization"]})
    )
    
    omXplore::omXplore_density_server("densityPlot_Norm", 
      dataIn = reactive({rv.custom$dataIn}),
      i = reactive({length(rv.custom$dataIn)})
    )
    
    output$comparisonPlot <- renderUI({
      req(rv.custom$dataIn)
      norm_idx <- which(names(rv.custom$dataIn) == "Normalization")
      if (length(norm_idx) == 1) {
        plotly::plotlyOutput(ns("viewComparisonNorm_hc"))
      } else {
        tags$i(style = "color: black; margin-top: 10px; font-size: 12px; font-style: italic;",
               "The comparison plot will be available once the data has been normalized.")
      }
    })
    
    output$viewComparisonNorm_hc <- plotly::renderPlotly({
      req(rv.custom$dataIn)
      req(length(rv.custom$dataIn) > 1)
      norm_idx <- which(names(rv.custom$dataIn) == "Normalization")
      req(length(norm_idx) == 1)
      
      obj1 <- rv.custom$dataIn[[norm_idx]]
      obj2 <- rv.custom$dataIn[[norm_idx-1]]
      
      req(obj1)
      req(obj2)
      protId <- DaparToolshed::idcol(rv.custom$dataIn[[norm_idx]])
      
      if (!is.null(selectProt()$indices)) {
        .n <- length(selectProt()$indices)
        .subset <- selectProt()$indices
      } else {
        .n <- floor(0.02 * nrow(obj1))
        .subset <- seq(nrow(obj1))
      }
      
      DaparToolshed::compareNormalizationD_HC(
        qDataBefore = SummarizedExperiment::assay(rv.custom$dataIn, norm_idx),
        qDataAfter = SummarizedExperiment::assay(rv.custom$dataIn, norm_idx-1),
        keyId = SummarizedExperiment::rowData(rv.custom$dataIn[[norm_idx]])[, protId],
        conds = DaparToolshed::design_qf(rv.custom$dataIn)$Condition,
        pal = NULL,
        # Consider only 2% of the entire dataset
        n = .n,
        subset.view = .subset
      )
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Normalization', btnEvents()))
      
      shiny::withProgress(message = paste0("Normalization process", id), {
        shiny::incProgress(0.5)

        if ( is.null(rv.custom$dataIn) ||
          rv.widgets$Normalization_method == widgets.default.values$Normalization_method)
        shinyjs::info(btnVentsMasg)
        
        else {
          req(rv.widgets$Normalization_method)
          req(rv.custom$dataIn)
        
          rv.custom$tmpAssay <- NULL
          .tmp <- NULL
          try({
            .conds <- SummarizedExperiment::colData(rv.custom$dataIn)[, "Condition"]
            qdata <- SummarizedExperiment::assay(rv.custom$dataIn, length(rv.custom$dataIn))
            
            switch(rv.widgets$Normalization_method,
              G_noneStr = {
                .tmp <- rv.custom$dataIn[[length(rv.custom$dataIn)]]
              },
              
              GlobalQuantileAlignment = {
                .tmp <- DaparToolshed::GlobalQuantileAlignment(qdata)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'method', rv.widgets$Normalization_method)
              },
              
              QuantileCentering = {
                quant <- NA
                if (!is.null(rv.widgets$Normalization_quantile)) {
                  quant <- as.numeric(rv.widgets$Normalization_quantile)
                }
                
                .tmp <- DaparToolshed::QuantileCentering(
                  qData = qdata, 
                  conds = .conds, 
                  type = rv.widgets$Normalization_type, 
                  subset.norm = selectProt()$indices, 
                  quantile = quant)
                
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'method', rv.widgets$Normalization_method)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'quantile', quant)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'type', rv.widgets$Normalization_type)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'subset.norm', selectProt()$indices)
              },
              
              MeanCentering = {
                .tmp<- DaparToolshed::MeanCentering(
                  qData = qdata, 
                  conds = .conds,
                  type = rv.widgets$Normalization_type,
                  scaling = rv.widgets$Normalization_varReduction,
                  subset.norm = selectProt()$indices
                )
                
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'method', rv.widgets$Normalization_method)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'varReduction', rv.widgets$Normalization_varReduction)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'type', rv.widgets$Normalization_type)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'subset.norm', selectProt()$indices)
              },
              
              SumByColumns = {
                .tmp <- DaparToolshed::SumByColumns(
                  qData = qdata,
                  conds = .conds,
                  type = rv.widgets$Normalization_type,
                  subset.norm = selectProt()$indices
                )
                
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'method', rv.widgets$Normalization_method)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'type', rv.widgets$Normalization_type)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'subset.norm', selectProt()$indices)
              },
              
              LOESS = {
                .tmp <- DaparToolshed::LOESS(
                  qData = qdata,
                  conds = .conds,
                  type = rv.widgets$Normalization_type,
                  span = as.numeric(rv.widgets$Normalization_spanLOESS)
                )
                
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'method', rv.widgets$Normalization_method)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'type', rv.widgets$Normalization_type)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'spanLOESS', as.numeric(rv.widgets$Normalization_spanLOESS))
              },
              
              vsn = {
                .tmp <- DaparToolshed::vsn(
                  qData = qdata,
                  conds = .conds,
                  type = rv.widgets$Normalization_type
                )
                
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'method', rv.widgets$Normalization_method)
                rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'Normalization', 'Normalization', 'type', rv.widgets$Normalization_type)
              }
            )
          })
        
          if(inherits(.tmp, "try-error") || inherits(.tmp, "try-warning")) {
            MagellanNTK::mod_SweetAlert_server(id = 'sweetalert_perform_normalization',
              text = .tmp[[1]],
              type = 'error' )
          } else {
            new.dataset <- rv.custom$dataIn[[length(rv.custom$dataIn)]]
            SummarizedExperiment::assay(new.dataset) <- .tmp
            
            rv.custom$dataIn <- QFeatures::addAssay(rv.custom$dataIn, new.dataset, 'Normalization')
            
            # DO NOT MODIFY THE THREE FOLLOWING LINES
            dataOut$trigger <- MagellanNTK::Timestamp()
            dataOut$value <- NULL
            rv$steps.status['Normalization'] <- MagellanNTK::stepStatus$VALIDATED
          }
        }
      })
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
          uiOutput(ns('dl_ui'))
        )
      )
    })
    
    #### _content -----
    output$dl_ui <- renderUI({
      req(rv$steps.status['Save'] == MagellanNTK::stepStatus$VALIDATED)
      req(config@mode == 'process')
      
      Prostar2::download_dataset_ui(ns(paste0(id, '_createQuickLink')))
    })
    
    ### btnEvent -----
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Save', btnEvents()))

      shiny::withProgress(message = paste0("Saving process", id), {
        shiny::incProgress(0.5)
        if (isTRUE(all.equal(SummarizedExperiment::assays(dataIn()),
                             SummarizedExperiment::assays(rv.custom$dataIn))))
          shinyjs::info(btnVentsMasg)
        else {
          S4Vectors::metadata(rv.custom$dataIn)$name.pipeline <- 'PipelineProtein'
          
          DaparToolshed::paramshistory(rv.custom$dataIn[[length(rv.custom$dataIn)]]) <- rbind(DaparToolshed::paramshistory(rv.custom$dataIn[[length(rv.custom$dataIn)]]),
                                                                                              rv.custom$history)
        
          # DO NOT MODIFY THE THREE FOLLOWING LINES
          dataOut$trigger <- MagellanNTK::Timestamp()
          dataOut$value <- rv.custom$dataIn
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
