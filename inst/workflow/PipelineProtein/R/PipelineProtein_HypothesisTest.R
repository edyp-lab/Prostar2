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
#' In this example, `PipelineProtein_HypothesisTest_ulength(rv$dataIn)` and `PipelineProtein_HypothesisTest_server()` define
#' the code for the process `ProcessProtein` which is part of the pipeline called `PipelineProtein`.
#' 
#' @examples
#' if (interactive()){
#' library(MagellanNTK)
#' library(DaparToolshed)
#' data(Exp1_R25_prot, package = "DaparToolshedData")
#' obj <- Exp1_R25_prot
#' # Simulate imputation of missing values
#' obj <- NAIsZero(obj, 1)
#' path <- system.file('workflow/PipelineProtein', package = 'Prostar2')
#' shiny::runApp(workflowApp("PipelineProtein_HypothesisTest", path, dataIn = obj))
#' }
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' 
#' @name PipelineProtein
#' 
NULL

#' 
#' @rdname PipelineProtein
#' @export
#' 
PipelineProtein_HypothesisTest_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelineProtein_HypothesisTest',
    mode = 'process',
    steps = c('HypothesisTest'),
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
PipelineProtein_HypothesisTest_ui <- function(id){
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
#' @rdname PipelineProtein
#' 
#' @importFrom stats setNames rnorm
#' @import DaparToolshed
#' @importFrom shinyjs useShinyjs
#' 
#' @export
#' 
PipelineProtein_HypothesisTest_server <- function(id,
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
    HypothesisTest_design = "None",
    HypothesisTest_method = "None",
    HypothesisTest_ttestOptions = 'Student',
    HypothesisTest_thlogFC = 0
  )
  
  rv.custom.default.values <- list(
    result_open_dataset = reactive({NULL}),
    containsNA = FALSE,
    listNomsComparaison = NULL,
    n = NULL,
    swap.history = NULL,
    AllPairwiseComp = NULL,
    logFC_onevsall = NULL,
    logFC_onevsone = NULL,
    history = MagellanNTK::InitializeHistory()
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
    #   MagellanNTK::open_dataset_ui(id = ns("open_dataset"))
    # })
    
    
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
    

    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Description', btnEvents()))
     req(dataIn())

     m <- DaparToolshed::match.metacell(
       DaparToolshed::qMetacell(dataIn()[[length(dataIn())]]),
       pattern = c("Missing", "Missing POV", "Missing MEC"),
       level = DaparToolshed::typeDataset(dataIn()[[length(dataIn())]])
     )
     
     rv.custom$containsNA <- length(which(m)) > 0
     
     if (rv.custom$containsNA){
       warntxt <- "The dataset contains missing values.
        It must be first filtered or imputed."
       MagellanNTK::mod_errorModal_server('warn_NA',
         title = 'Warning',
         text = warntxt)
     } else {
     
      rv$dataIn <- rv.custom$dataIn <- dataIn()
      
      # if(!is.null(rv.custom$result_open_dataset()$dataset))
      #   rv$dataIn <- rv.custom$result_open_dataset()$dataset
      # 
      shiny::withProgress(message = paste0("Reseting process", id), {
        shiny::incProgress(0.5)
        
        tmp_onevsone <- DaparToolshed::limmaCompleteTest(
          qData = SummarizedExperiment::assay(rv.custom$dataIn, length(rv.custom$dataIn)),
          sTab = SummarizedExperiment::colData(rv.custom$dataIn),
          comp.type = "OnevsOne" )
        rv.custom$logFC_onevsone <- tmp_onevsone$logFC
        
        tmp_onevsall <- DaparToolshed::limmaCompleteTest(
          qData = SummarizedExperiment::assay(rv.custom$dataIn, length(rv.custom$dataIn)),
          sTab = SummarizedExperiment::colData(rv.custom$dataIn),
          comp.type = "OnevsAll" )
        rv.custom$logFC_onevsall <- tmp_onevsall$logFC

      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status['Description'] <- MagellanNTK::stepStatus$VALIDATED
      })
      
     }
    })
    
    
    
    # >>>
    # >>> START ------------- Code for HypothesisTest UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI ------------------------------------
    output$HypothesisTest <- renderUI({
      shinyjs::useShinyjs()
      
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          #timeline_process_ui(ns('HypothesisTest_timeline')),
          uiOutput(ns('HypothesisTest_widgets_ui'))),
        content = uiOutput(ns('HypothesisTest_plots_ui'))
      )
    })
    
    
    
    output$HypothesisTest_plots_ui <- renderUI({
      req(rv.custom$dataIn)

        tagList(
          uiOutput(ns('HypothesisTest_warning_conditions_ui')),
          uiOutput(ns("HypothesisTest_swapConds_ui")),
          highcharter::highchartOutput(ns("FoldChangePlot")) )
    })
    
    
    
    output$HypothesisTest_widgets_ui <- renderUI({
      .style <- "display:inline-block; vertical-align: middle; 
      padding-right: 20px;"
      
      tagList(
        div(id = ns('div_HypothesisTest_widgets_ui'),
          div(id = ns('div_HypothesisTest_design_ui'),
            style = .style, uiOutput(ns('HypothesisTest_design_ui'))),
          div(id = ns('div_HypothesisTest_method_ui'),
            style = .style, uiOutput(ns('HypothesisTest_method_ui'))),
          div(id = ns('div_HypothesisTest_ttestOptions_ui'),
            style = .style, uiOutput(ns('HypothesisTest_ttestOptions_ui'))),
          div(id = ns('div_HypothesisTest_thlogFC_ui'),
            style = .style, uiOutput(ns('HypothesisTest_thlogFC_ui'))),
          div(id = ns('div_HypothesisTest_correspondingRatio_ui'),
            style = .style, uiOutput(ns("HypothesisTest_correspondingRatio_ui"))),
          div(id = ns('div_HypothesisTest_info_Limma_disabled_ui'),
            style = .style, uiOutput(ns('HypothesisTest_info_Limma_disabled_ui')))
        )
      )
    })
    
    output$HypothesisTest_warning_conditions_ui <- renderUI({
      req(rv.custom$dataIn)
      req(length(unique(DaparToolshed::design.qf(rv.custom$dataIn)$Condition)) > 26)
      req(getDesignLevel(SummarizedExperiment::colData(rv.custom$dataIn)) > 1)
      h3('Limma with this version of Prostar does not handle datasets with 
      more than 26 conditions. Such, the Limma option is desactivated for the 
        current dataset')
    })
    # >>> START: Definition of the widgets
    
    
    output$HypothesisTest_design_ui <- renderUI({
      widget <- selectInput(ns("HypothesisTest_design"), "Contrast",
        choices = c("None" = "None", 
          "One vs One" = "OnevsOne",
          "One vs All" = "OnevsAll"),
        selected = rv.widgets$HypothesisTest_design,
        width = "150px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['HypothesisTest'] &&
          !isTRUE(rv.custom$containsNA))
    })
    
    output$HypothesisTest_method_ui <- renderUI({
      .methods <- c("None" = "None", "t-tests" = "ttests")
      if(enable_Limma())
        .methods <- c(.methods, "Limma" = "Limma")
      
      widget <- selectInput(ns("HypothesisTest_method"), "Statistical test",
        choices = .methods,
        selected = rv.widgets$HypothesisTest_method,
        width = "150px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['HypothesisTest'] &&
          !isTRUE(rv.custom$containsNA))
    })
    
    output$HypothesisTest_ttestOptions_ui <- renderUI({
      req(rv.widgets$HypothesisTest_method == "ttests")
      widget <- radioButtons(ns("HypothesisTest_ttestOptions"), 
        "t-tests options",
        choices = c("Student", "Welch"),
        selected = rv.widgets$HypothesisTest_ttestOptions,
        width = "150px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['HypothesisTest'] &&
          !isTRUE(rv.custom$containsNA))
    })
    
    output$HypothesisTest_thlogFC_ui <- renderUI({
      widget <- textInput(ns("HypothesisTest_thlogFC"),
        "log(FC) threshold",
        value = rv.widgets$HypothesisTest_thlogFC,
        width = "150px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['HypothesisTest'] &&
          !isTRUE(rv.custom$containsNA))
    })
    
    output$HypothesisTest_correspondingRatio_ui <- renderUI({
      ratio <- as.numeric(rv.widgets$HypothesisTest_thlogFC)
      p("(FC = ", 2^(ratio), ")")
    })
    
    
    observeEvent(req(rv.widgets$HypothesisTest_design != 'None'), {
      req(rv.custom$dataIn)
      # Get logFC
      if(rv.widgets$HypothesisTest_design == "OnevsOne"){
        rv.custom$logFC <- rv.custom$logFC_onevsone
      } else if(rv.widgets$HypothesisTest_design == "OnevsAll") {
        rv.custom$logFC <- rv.custom$logFC_onevsall
      }
      # Get comparison names
      rv.custom$listNomsComparaison <- colnames(rv.custom$logFC)
      rv.custom$listNomsComparaison <- unlist(strsplit(rv.custom$listNomsComparaison, split = '_logFC'))
      # Get number of comparison 
      rv.custom$n <- ncol(rv.custom$logFC)
      rv.custom$swap.history <- rep(0, rv.custom$n)   
    })

    output$FoldChangePlot <- highcharter::renderHighchart({
      req(rv.custom$logFC) 
      
      withProgress(message = "Computing plot...", detail = "", value = 0.5, {
        rv.widgets$HypothesisTest_thlogFC <- as.numeric(
          rv.widgets$HypothesisTest_thlogFC)
        
        DaparToolshed::hc_logFC_DensityPlot(
          df_logFC = as.data.frame(rv.custom$logFC),
          th_logFC = as.numeric(rv.widgets$HypothesisTest_thlogFC)
        )
      })
    })
    
    output$showConds <- renderUI({
      req(rv.custom$listNomsComparaison)
      

      widget <- lapply(seq_len(rv.custom$n), function(i) {
        ll.conds <- unlist(
          strsplit(rv.custom$listNomsComparaison[i], split = "_vs_")
        )
        
        div(id = ns('div_showConds'),
            style = "margin-bottom: -15px;",
            div(id = ns('div_compswap'),
                style = "display: inline-block; margin-right: 10px;",
                checkboxInput(ns(paste0("compswap", i)), "",
                              value = rv.custom$swap.history[i],
                              width = "100%") ),
            div(id = ns('div_ll.conds1'),
                style = "display: inline-block;", 
                paste0(gsub("[()]", "", ll.conds[1]), "   VS   ",gsub("[()]", "", ll.conds[2])) )
        )
      })
      
      do.call(tagList, widget)
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['HypothesisTest'] &&
          !isTRUE(rv.custom$containsNA))
    })
    
    
    observeEvent(GetSwapShinyValue(), ignoreInit = TRUE,{
      ind.swap <- which(GetSwapShinyValue() != rv.custom$swap.history)
      
      req(length(ind.swap) > 0)
      rv.custom$swap.history <- GetSwapShinyValue()
      #if (length(ind.swap) > 0) {
      # for (i in ind.swap) {
      current.comp <- colnames(rv.custom$logFC)[ind.swap]
      
      # Swap comparisons names
      ll <- unlist(strsplit(current.comp, split = "_"))
      tmp.cond1 <- gsub("[( )]", "", ll[1])
      tmp.cond2 <- gsub("[( )]", "", ll[3])
      tmp.logFC <- paste0("(", tmp.cond2, ")_vs_(", tmp.cond1, ")_logFC" )
      
      
      colnames(rv.custom$logFC)[ind.swap] <- tmp.logFC 
      
      # Swap logFC values
      .logFC <- rv.custom$logFC
      rv.custom$logFC[, ind.swap] <- -.logFC[, ind.swap]
    })
    
    GetSwapShinyValue <- reactive({
      req(rv.custom$n)
      
      unlist(lapply(seq(rv.custom$n), function(x) 
        input[[paste0("compswap", x)]]
      ))
    })
    
    
    ### Computation of comparisons selected in the variable
    # 'rv$widgets$hypothesisTest$design'
    ComputeComparisons <- reactive({
      
      req(rv.widgets$HypothesisTest_method != "None")
      req(rv.widgets$HypothesisTest_design != "None")
      if (rv.widgets$HypothesisTest_method == 'ttests')
        req(rv.widgets$HypothesisTest_ttestOptions != "None")
      
      
      rv.custom$AllPairwiseComp <- NULL
      rv.custom$AllPairwiseCompMsg <- NULL
   
      rv.custom$AllPairwiseComp <- tryCatch({
        switch(rv.widgets$HypothesisTest_method,
          Limma = {
            DaparToolshed::limmaCompleteTest(
              qData = SummarizedExperiment::assay(rv.custom$dataIn, length(rv.custom$dataIn)),
              sTab = SummarizedExperiment::colData(rv.custom$dataIn),
              comp.type = rv.widgets$HypothesisTest_design )
          },
          ttests = {
            rv.custom$AllPairwiseComp <- DaparToolshed::compute_t_tests(
              obj = rv.custom$dataIn,
              i = length(rv.custom$dataIn),
              contrast = rv.widgets$HypothesisTest_design,
              type = rv.widgets$HypothesisTest_ttestOptions )
          })
        },
        warning = function(w) {
          msg <- w
          rv.custom$AllPairwiseCompMsg <- w$message
          return(NULL)
          },
        error = function(e) {
          rv.custom$AllPairwiseCompMsg <- e$message
          return(NULL)
          },
        finally = {
          # cleanup-code
        })

      rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'HypothesisTest', 'HypothesisTest', 'method', rv.widgets$HypothesisTest_method)
      rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'HypothesisTest', 'HypothesisTest', 'design', rv.widgets$HypothesisTest_design)
      
      
      if (rv.widgets$HypothesisTest_method == 'ttests')
        rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'HypothesisTest', 'HypothesisTest', 'ttestOptions', rv.widgets$HypothesisTest_ttestOptions)

      if(!is.null(rv.custom$AllPairwiseComp)){
      rv.custom$listNomsComparaison <- colnames(rv.custom$AllPairwiseComp$logFC)
      rv.custom$listNomsComparaison <- unlist(strsplit(rv.custom$listNomsComparaison, split = '_logFC'))
      }
    }) 
    
    
    
    enable_Limma <- reactive({
      req(rv.custom$dataIn)
      
      enable <- TRUE
      nConds <-length(unique(DaparToolshed::design.qf(rv.custom$dataIn)$Condition))
      design <- SummarizedExperiment::colData(rv.custom$dataIn)
      nLevel <- DaparToolshed::getDesignLevel(design)   
      enable <- (nConds <= 26 && nLevel == 1) ||
        (nConds < 10 && (nLevel%in% c(2,3)))
      
      enable
    })
    
    output$HypothesisTest_info_Limma_disabled_ui <- renderUI({
      req(!enable_Limma())
      tagList(
        tags$p('Info: Limma has been disabled because the design of your dataset:'),
        tags$ul(
          tags$li(p('is of level 1 and contains more than 26 conditions,')),
          tags$li('is of level 2 or 3 and contains more than 9 conditions.')
        ),
        tags$p('Prostar does not currently handle these cases.')
      )
    })
    

    output$HypothesisTest_swapConds_ui <- renderUI({
      widget <- tagList(
        h3("Swap conditions"),
        uiOutput(ns("showConds")),
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['HypothesisTest']  &&
          !isTRUE(rv.custom$containsNA))
    })
    
    
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('HypothesisTest', btnEvents()))
     
      shiny::withProgress(message = paste0("Computing Hypothesis Test", id), {
        shiny::incProgress(0.5)
        
      if ( is.null(rv.custom$dataIn) || rv.widgets$HypothesisTest_method == "None" || rv.widgets$HypothesisTest_design == "None" || 
           (rv.widgets$HypothesisTest_method == 'ttests' && rv.widgets$HypothesisTest_ttestOptions == "None"))
        shinyjs::info(btnVentsMasg)
      else {
        req(rv.custom$dataIn)
        
        rv.widgets$HypothesisTest_thlogFC <- as.numeric(
          rv.widgets$HypothesisTest_thlogFC)
        
        ComputeComparisons()
        if(is.null(rv.custom$AllPairwiseComp)){
          
          MagellanNTK::mod_SweetAlert_server(id = 'sweetalert_PerformLogFCPlot',
                                             text = rv.custom$AllPairwiseCompMsg,
                                             type = 'error' )
        } 
        else if(inherits(rv.custom$AllPairwiseComp, "try-error")) {
          
          MagellanNTK::mod_SweetAlert_server(id = 'sweetalert_PerformLogFCPlot',
                                             text = rv.custom$AllPairwiseComp[[1]],
                                             type = 'error' )
        } else {
          rv.custom$n <- ncol(rv.custom$AllPairwiseComp$logFC)
          rv.custom$swap.history <- rep(0, rv.custom$n)
        }
        
        
        
        new.dataset <- rv.custom$dataIn[[length(rv.custom$dataIn)]]
        df <- cbind(rv.custom$AllPairwiseComp$logFC, 
          rv.custom$AllPairwiseComp$P_Value)
        DaparToolshed::HypothesisTest(new.dataset) <- as.data.frame(df)
        rv.custom$history <- Prostar2::Add2History(rv.custom$history, 'HypothesisTest', 'HypothesisTest', 'thlogFC', as.numeric(rv.widgets$HypothesisTest_thlogFC))
        
        DaparToolshed::paramshistory(new.dataset) <- rbind(DaparToolshed::paramshistory(new.dataset),
          rv.custom$history)
        rv.custom$dataIn <- QFeatures::addAssay(rv.custom$dataIn, new.dataset, 'HypothesisTest')

        # DO NOT MODIFY THE THREE FOLLOWINF LINES
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status['HypothesisTest'] <- MagellanNTK::stepStatus$VALIDATED
      }
      })
    })
    
    # >>> END: Definition of the widgets

    # >>> START ------------- Code for step 3 UI---------------
    output$Save <- renderUI({
      MagellanNTK::process_layout(session,
        ns = NS(id),
        sidebar = tagList(
          #timeline_process_ui(ns('Save_timeline'))
        ),
        content = tagList(
          #uiOutput(ns('dl_ui'))
          )
      )
    })
    
    
    # output$dl_ui <- renderUI({
    #   req(rv$steps.status['Save'] == MagellanNTK::stepStatus$VALIDATED)
    #   req(config@mode == 'process')
    # 
    #   MagellanNTK::download_dataset_ui(ns('createQuickLink'))
    # })
    

    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Save', btnEvents()))
      shiny::withProgress(message = paste0("Reseting process", id), {
        shiny::incProgress(0.5)
        # Do some stuff

      if (isTRUE(all.equal(SummarizedExperiment::assays(rv.custom$dataIn),
        SummarizedExperiment::assays(dataIn()))))
        shinyjs::info(btnVentsMasg)
      else {
        S4Vectors::metadata(rv.custom$dataIn)$name.pipeline <- 'PipelineProtein'
        
        # DO NOT MODIFY THE THREE FOLLOWING LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv.custom$dataIn
      rv$steps.status['Save'] <- MagellanNTK::stepStatus$VALIDATED
      # Prostar2::download_dataset_server('createQuickLink',
      #   dataIn = reactive({rv.custom$dataIn}))
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
