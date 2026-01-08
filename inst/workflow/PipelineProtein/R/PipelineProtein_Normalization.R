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
#' In this example, `PipelineProtein_Normalization_ui()` and `PipelineProtein_Normalization_server()` define
#' the code for the process `ProcessProtein` which is part of the pipeline called `PipelineProtein`.
#' 
#' @example inst/workflow/PipelineProtein/examples/example_PipelineProtein_Normalization.R
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' 
NULL

#' @rdname PipelineProtein
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


#' @param id xxx
#' 
#' @rdname PipelineProtein
#' 
#' @author Samuel Wieczorek
#' 
#' @export
#'
PipelineProtein_Normalization_ui <- function(id){
  ns <- NS(id)
  shinyjs::useShinyjs()
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
#'
#' @rdname PipelineProtein
#' 
#' @importFrom stats setNames rnorm
#' @import omXplore
#' @importFrom shinyjs hidden useShinyjs toggle
#' @importFrom QFeatures addAssay
#' 
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
  
  
  pkgs.require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))
  
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
    history = NULL,
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
    add.resourcePath()

    
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
          div(id = ns("chunk"), style = "width: 100px; height: 100px;" ),
          if (file.exists(file))
            includeMarkdown(file)
          else
            p('No Description available')
          #uiOutput(ns('Description_infos_dataset_UI'))
        )
      )

    })

    
    
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
    
    

    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Description', btnEvents()))
      #rv.custom$result_open_dataset()$dataset
      req(dataIn())
      rv$dataIn <- dataIn()
      
      
      #if (session$userData$wf_mode == 'process'){
      #rv$dataIn <- QFeatures::QFeatures()
      #rv$dataIn <- QFeatures::addAssay(rv$dataIn, SummarizedExperiment::SummarizedExperiment(), name = 'tmp')
      
      if(!is.null(rv.custom$result_open_dataset()$dataset))
        rv$dataIn <- rv.custom$result_open_dataset()$dataset
     #}

      shiny::withProgress(message = paste0("xxx process", id), {
        shiny::incProgress(0.5)
        
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- stepStatus$VALIDATED
    })
    
    })
    
    # >>>
    # >>> START ------------- Code for Normalization UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI ------------------------------------
    output$Normalization <- renderUI({
      shinyjs::useShinyjs()
      #path <- file.path(system.file('www/css', package = 'MagellanNTK'),'MagellanNTK.css')
      #includeCSS(path)
      
      .style <- "display:inline-block; vertical-align: middle; 
      padding-right: 20px;"

      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          #timeline_process_ui(ns('Normalization_timeline')),
          uiOutput(ns("Normalization_method_ui")),
          shinyjs::hidden(uiOutput(ns('Normalization_type_ui'))),
          shinyjs::hidden(uiOutput(ns('Normalization_spanLOESS_ui'))),
          uiOutput(ns("Normalization_quantile_ui")),
          uiOutput(ns("Normalization_varReduction_ui")),
          uiOutput(ns('tracking')),
          shinyjs::hidden(uiOutput(ns("Normalization_sync_ui")))
          ),
        content = tagList(
          fluidRow(
            column(width = 6,
              omXplore::omXplore_density_ui(ns("densityPlot_Norm")),
          omXplore::omXplore_intensity_ui(ns("boxPlot_Norm"))
              ),
            column(width = 6,
          highcharter::highchartOutput(ns("viewComparisonNorm_hc"))
            )
          ))
        )
    })
    
    ###############################################################
    
    selectProt <- omXplore::plots_tracking_server(
      id = "tracker",
      dataIn = reactive({rv$dataIn[[length(rv$dataIn)]]}),
      remoteReset = reactive({remoteReset()})
    )
    
    omXplore::omXplore_intensity_server("boxPlot_Norm",
      dataIn = reactive({rv$dataIn}),
      i = reactive({length(rv$dataIn)}),
      track.indices = reactive({selectProt()$indices}),
      remoteReset = reactive({remoteReset()}),
      is.enabled = reactive({rv$steps.enabled["Normalization"]})
    )
    
    omXplore::omXplore_density_server("densityPlot_Norm", 
      dataIn = reactive({rv$dataIn}),
      i = reactive({length(rv$dataIn)})
    )
    
    
    output$Normalization_method_ui <- renderUI({
      widget <- selectInput(
        ns('Normalization_method'),
        "Normalization method",
        choices = setNames(nm = c("None", DaparToolshed::normalizeMethods())),
        selected = rv.widgets$Normalization_method,
        width = '180px'
        )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["Normalization"])
    })
    
    
    output$Normalization_type_ui <- renderUI({
      widget <- selectInput(ns('Normalization_type'),
        "Normalization type",
        choices = stats::setNames(
          nm = c("overall", "within conditions")),
        selected = rv.widgets$Normalization_type,
        width = '150px')
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
        MagellanNTK::mod_popover_for_help_ui(ns('quantile_help')),
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
    
    
    
    output$viewComparisonNorm_hc <- highcharter::renderHighchart({
      req(rv$dataIn)
      req(length(rv$dataIn) > 1)
      #req(names(rv$dataIn)[length(rv$dataIn)] == 'Normalization')
      obj1 <- rv$dataIn[[length(rv$dataIn)]]
      obj2 <- rv$dataIn[[length(rv$dataIn)-1]]
      
      req(obj1)
      req(obj2)
      protId <- DaparToolshed::idcol(rv$dataIn[[length(rv$dataIn)]])
      
      if (!is.null(selectProt()$indices)) {
        .n <- length(selectProt()$indices)
        .subset <- selectProt()$indices
      } else {
        .n <- floor(0.02 * nrow(obj1))
        .subset <- seq(nrow(obj1))
      }
      
      
      
      DaparToolshed::compareNormalizationD_HC(
        qDataBefore = SummarizedExperiment::assay(rv$dataIn, length(rv$dataIn)),
        qDataAfter = SummarizedExperiment::assay(rv$dataIn, length(rv$dataIn)-1),
        keyId = SummarizedExperiment::rowData(rv$dataIn[[length(rv$dataIn)]])[, protId],
        conds = DaparToolshed::design.qf(rv$dataIn)$Condition,
        pal = NULL,
        # Consider only 2% of the entire dataset
        n = .n,
        subset.view = .subset
      )
    })
    
    observeEvent(rv.widgets$Normalization_method, {
      req(rv.widgets$Normalization_method)
      req(rv$dataIn)
      shinyjs::toggle("Normalization_btn_validate",
        condition = rv.widgets$Normalization_method != "None")
      
      shinyjs::toggle("spanLOESS",
        condition = rv.widgets$Normalization_method == "LOESS")
      
      .choice <- c("QuantileCentering", "MeanCentering", "SumByColumns", 
        "LOESS", "vsn")
      
      shinyjs::toggle("Normalization_type_ui",
        condition = (rv.widgets$Normalization_method %in% .choice)
      )
      
      cond <- S4Vectors::metadata(rv$dataIn[[length(rv$dataIn)]])[['typeDataset']] == "protein"
      
      .meths <- DaparToolshed::normalizeMethods('withTracking')
      trackAvailable <- rv.widgets$Normalization_method %in% .meths
      shinyjs::toggle("Normalization_sync_ui",
        condition = cond && trackAvailable)
      
    })
    
    
    MagellanNTK::mod_popover_for_help_server(
      id = 'quantile_help',
      title = "Normalization quantile",
      content = "lower limit/noise (quantile = 0.15),
            median (quantile = 0.5). Min value=0, max value=1"
    )
    

    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Normalization', btnEvents()))
      
      shiny::withProgress(message = paste0("Normalization process", id), {
        shiny::incProgress(0.5)
        print(paste0(id, ' : shiny::withProgress(message = paste0("Normalization process", id), {'))
      if ( is.null(rv$dataIn) ||
          rv.widgets$Normalization_method == widgets.default.values$Normalization_method)
        shinyjs::info(btnVentsMasg)
      else {
        
        
      # Do some stuff 
      req(rv.widgets$Normalization_method)
      req(rv$dataIn)
      
      
      rv.custom$tmpAssay <- NULL
      try({
        .conds <- SummarizedExperiment::colData(rv$dataIn)[, "Condition"]
        qdata <- SummarizedExperiment::assay(rv$dataIn, length(rv$dataIn))
        
        
        switch(rv.widgets$Normalization_method,
          
          G_noneStr = {
            rv.custom$tmpAssay <- rv$dataIn[[length(rv$dataIn)]]
          },
          
          GlobalQuantileAlignment = {
            rv.custom$tmpAssay <- DaparToolshed::GlobalQuantileAlignment(qdata)
            rv.custom$history[['Normalization_method']] <- rv.widgets$Normalization_method
            
          },
          
          QuantileCentering = {
            quant <- NA
            if (!is.null(rv.widgets$Normalization_quantile)) {
              quant <- as.numeric(rv.widgets$Normalization_quantile)
            }
            
            rv.custom$tmpAssay <- DaparToolshed::QuantileCentering(
              qData = qdata, 
              conds = .conds, 
              type = rv.widgets$Normalization_type, 
              subset.norm = selectProt()$indices, 
              quantile = quant)
            
            rv.custom$history[['Normalization_method']] <- rv.widgets$Normalization_method
            rv.custom$history[['Normalization_quantile']] <- quant
            rv.custom$history[['Normalization_type']] <- rv.widgets$Normalization_type
            rv.custom$history[['subset.norm']] <- selectProt()$indices
          },
          
          MeanCentering = {
            rv.custom$tmpAssay <- DaparToolshed::MeanCentering(
              qData = qdata, 
              conds = .conds,
              type = rv.widgets$Normalization_type,
              scaling = rv.widgets$Normalization_varReduction,
              subset.norm = selectProt()$indices
            )
            
            rv.custom$history[['Normalization_method']] <- rv.widgets$Normalization_method
            rv.custom$history[['Normalization_varReduction']] <- rv.widgets$Normalization_varReduction
            rv.custom$history[['Normalization_type']] <- rv.widgets$Normalization_type
            rv.custom$history[['subset.norm']] <- selectProt()$indices
            
          },
          SumByColumns = {
            rv.custom$tmpAssay <- DaparToolshed::SumByColumns(
              qData = qdata,
              conds = .conds,
              type = rv.widgets$Normalization_type,
              subset.norm = selectProt()$indices
            )
            
            rv.custom$history[['Normalization_method']] <- rv.widgets$Normalization_method
            rv.custom$history[['Normalization_type']] <- rv.widgets$Normalization_type
            rv.custom$history[['subset.norm']] <- selectProt()$indices
            
          },
          LOESS = {
            rv.custom$tmpAssay <- DaparToolshed::LOESS(
              qData = qdata,
              conds = .conds,
              type = rv.widgets$Normalization_type,
              span = as.numeric(rv.widgets$Normalization_spanLOESS)
            )
            
            rv.custom$history[['Normalization_method']] <- rv.widgets$Normalization_method
            rv.custom$history[['Normalization_type']] <- rv.widgets$Normalization_type
            rv.custom$history[['Normalization_spanLOESS']] <- as.numeric(rv.widgets$Normalization_spanLOESS)
            
          },
          vsn = {
            rv.custom$tmpAssay <- DaparToolshed::vsn(
              qData = qdata,
              conds = .conds,
              type = rv.widgets$Normalization_type
            )
            
            rv.custom$history[['Normalization_method']] <- rv.widgets$Normalization_method
            rv.custom$history[['Normalization_type']] <- rv.widgets$Normalization_type
            
          }
        )
      })
      
      
      
      if(inherits(rv.custom$tmpAssay, "try-error") || is.null(rv.custom$tmpAssay)) {
        
        MagellanNTK::mod_SweetAlert_server(id = 'sweetalert_perform_normalization',
          text = rv.custom$tmpAssay[[1]],
          type = 'error' )
        
        # DO NOT MODIFY THE THREE FOLLOWING LINES
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- rv$dataIn
      } else {
        
        new.dataset <- rv$dataIn[[length(rv$dataIn)]]
        SummarizedExperiment::assay(new.dataset) <- rv.custom$tmpAssay
        DaparToolshed::paramshistory(new.dataset) <- NULL
        DaparToolshed::paramshistory(new.dataset) <- rv.custom$history
        rv$dataIn <- QFeatures::addAssay(rv$dataIn, new.dataset, 'Normalization')
      }
      
      # DO NOT MODIFY THE THREE FOLLOWING LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status['Normalization'] <- stepStatus$VALIDATED
    }
    })
    })

    
    # >>> START ------------- Code for step 3 UI---------------
    output$Save <- renderUI({
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(),
        content = tagList(
          uiOutput(ns('dl_ui'))
        )
      )
    })
    
    output$dl_ui <- renderUI({
      req(rv$steps.status['Save'] == stepStatus$VALIDATED)
      req(config@mode == 'process')
      
      Prostar2::download_dataset_ui(ns(paste0(id, '_createQuickLink')))
    })
    

    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(grepl('Save', btnEvents()))

    
      shiny::withProgress(message = paste0("Saving process", id), {
        shiny::incProgress(0.5)
        
        tmp <- dataIn()
        if(is.null(tmp))
          tmp <- rv.custom$result_open_dataset()$dataset
          
      if (isTRUE(all.equal(SummarizedExperiment::assays(rv$dataIn),
          SummarizedExperiment::assays(tmp))))
          shinyjs::info(btnVentsMasg)
      else {
      # Do some stuff
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Save'] <- stepStatus$VALIDATED
      
      Prostar2::download_dataset_server(paste0(id, '_createQuickLink'), dataIn = reactive({rv$dataIn}))
      }
      })
    })
    # <<< END ------------- Code for step 3 UI---------------
    
    
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  }
  )
}
