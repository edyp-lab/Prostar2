#' @title Shiny example process module.
#'
#' @description
#' This module contains the configuration informations for the corresponding pipeline.
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
#' In this example, `PipelineProtein_Imputation_ui()` and `PipelineProtein_Imputation_server()` define
#' the code for the process `PipelineProtein_Imputation` which is part of the pipeline called `PipelineProtein`.
#' 
#' @examples
#' if (interactive()){
#' library(MagellanNTK)
#' data(Exp1_R25_prot, package = 'DaparToolshedData')
#' path <- system.file('workflow/PipelineProtein', package = 'Prostar2')
#' shiny::runApp(workflowApp("PipelineProtein_Imputation", path, dataIn = Exp1_R25_prot))
#' }
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' 
NULL

#' @rdname PipelineProtein
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

#' @param id xxx
#' 
#' @rdname PipelineProtein
#' 
#' @author Samuel Wieczorek
#' 
#' @export
#'
PipelineProtein_Imputation_ui <- function(id){
  ns <- NS(id)
}


#' @param id xxx
#'
#' @param dataIn The dataset
#'
#' @param steps.enabled A vector of boolean which has the same length of the steps
#' of the pipeline. This information is used to enable/disable the widgets. It is not
#' a communication variable between the caller and this module, thus there is no
#' corresponding output variable
#'
#' @param remoteReset It is a remote command to reset the module. A boolean that
#' indicates is the pipeline has been reseted by a program of higher level
#' Basically, it is the program which has called this module
#' 
#' @param steps.status xxx
#' 
#' @param current.pos xxx
#'
#' @rdname PipelineProtein
#' 
#' @importFrom stats setNames rnorm
#' @importFrom shinyjs useShinyjs
#'
#' @export
#' 
PipelineProtein_Imputation_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1}),
  path = NULL,
  btnEvents = reactive({NULL})
){
  
  
  pkgs.require(c('QFeatures', 'SummarizedExperiment', 'S4Vectors'))
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
    history = list(),
    mv.present = FALSE,
    POVImputation_SummaryDT = data.frame(
      operation = "-",
      nbImputed = "0",
      TotalMainAssay = '0',
      stringsAsFactors = FALSE
    ),
    MECImputation_SummaryDT = data.frame(
      operation = "-",
      nbImputed = "0",
      TotalMainAssay = '0',
      stringsAsFactors = FALSE
    )
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
    add.resourcePath()
    
    
    observe({
      #Utile for the MEC imputation
      qdata <- SummarizedExperiment::assay(dataIn()[[length(dataIn())]])
      rv.custom$mv.present <- sum(is.na(qdata)) > 0
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
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
        paste0(id, '.Rmd')))
      
      MagellanNTK::process_layout(
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
      req(dataIn())
      rv$dataIn <- dataIn()
      rv.custom$dataIn1 <- dataIn()
      rv.custom$dataIn2 <- dataIn()
      
      rv.custom$POVImputation_SummaryDT <- data.frame(
        operation = "-",
        nbImputed = "0",
        TotalMainAssay = nrow(rv$dataIn[[length(rv$dataIn)]]),
        stringsAsFactors = FALSE
      )
      
      rv.custom$MECImputation_SummaryDT <- data.frame(
        operation = "-",
        nbImputed = "0",
        TotalMainAssay = nrow(rv$dataIn[[length(rv$dataIn)]]),
        stringsAsFactors = FALSE
      )
      
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- stepStatus$VALIDATED
    })
    
    
    
    # >>>
    # >>> START ------------- Code for step 1 UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI ------------------------------------
    output$POVImputation <- renderUI({
      shinyjs::useShinyjs()
      path <- file.path(system.file('www/css', package = 'MagellanNTK'),'MagellanNTK.css')
      includeCSS(path)
      
      MagellanNTK::process_layout(
        ns = NS(id),
        sidebar = tagList(
         # timeline_process_ui(ns('POVImputation_timeline')),
          uiOutput(ns("POVImputation_algorithm_UI")),
          uiOutput(ns("POVImputation_KNN_nbNeighbors_UI")),
          uiOutput(ns("POVImputation_detQuant_UI"))
        ),
        content = tagList(
          uiOutput(ns('POVImputation_DT_UI')),
          uiOutput(ns("POVImputation_showDetQuantValues")),
          htmlOutput("helpForImputation"),
          tags$hr(),
          uiOutput(ns('mvplots_ui'))
        )
      )
      
    })
    
    
    
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
      
      mod_mv_plots_server("POVImputation_mvplots",
        data = reactive({rv.custom$dataIn1[[length(rv.custom$dataIn1)]]}),
        grp = reactive({get_group(rv.custom$dataIn1)}),
        mytitle = reactive({"POV imputation"}),
        pal = reactive({NULL}),
        pattern = reactive({c("Missing", "Missing POV", "Missing MEC")})
      )
    })
    
    
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
    
    
    output$POVImputation_showDetQuantValues <- renderUI({
      req(rv.widgets$POVImputation_algorithm == "detQuantile")
      
      mod_DetQuantImpValues_server(
        id = "POVImputation_DetQuantValues_DT",
        dataIn = reactive({rv.custom$dataIn1[[length(rv.custom$dataIn1)]]}),
        quant = reactive({rv.widgets$POVImputation_detQuant_quantile}),
        factor = reactive({rv.widgets$POVImputation_detQuant_factor})
      )
      
      tagList(
        h5("The POV will be imputed by the following values :"),
        mod_DetQuantImpValues_ui(ns("POVImputation_DetQuantValues_DT"))
      )
    })
    
    
    
    output$POVImputation_KNN_nbNeighbors_UI <- renderUI({
      req(rv.widgets$POVImputation_algorithm == 'KNN')
      
      widget <- numericInput(ns("POVImputation_KNN_nbNeighbors"), "Neighbors",
        value = rv.widgets$POVImputation_KNN_n, step = 1, min = 0,
        max = max(nrow(rv.custom$dataIn1), rv.widgets$POVImputation_KNN_n),
        width = "100px"
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["POVImputation"])
    })
    
    
    output$POVImputation_detQuant_UI <- renderUI({
      req(rv.widgets$POVImputation_algorithm == 'detQuantile')
      
      widget <- tagList(
        numericInput(ns("POVImputation_detQuant_quantile"), "Quantile",
            value = rv.widgets$POVImputation_detQuant_quantile,
            step = 0.5, min = 0, max = 100, width = "100px"
          ),
        numericInput(ns("POVImputation_detQuant_factor"), "Factor",
            value = rv.widgets$POVImputation_detQuant_factor,
            step = 0.1, min = 0, max = 10, width = "100px"
          )
        )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["POVImputation"])
      
    })
    
    
    # output$POVImputation_btn_validate_ui <- renderUI({
    #   widget <- actionButton(ns("POVImputation_btn_validate"),
    #     "Perform",
    #     class = "btn-success")
    #   MagellanNTK::toggleWidget(widget, rv$steps.enabled['POVImputation'])
    # })
    # 
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('POVImputation', btnEvents()))
      req(rv.custom$dataIn1)
      
      if ( is.null(rv.custom$dataIn1) || 
          rv.widgets$POVImputation_algorithm == "None")
        info(btnVentsMasg)
      else {
      req(rv.widgets$POVImputation_algorithm != "None")
      m <- match.metacell(
        DaparToolshed::qMetacell(rv.custom$dataIn1[[length(rv.custom$dataIn1)]]),
        pattern = "Missing POV",
        level = DaparToolshed::typeDataset(rv.custom$dataIn1[[length(rv.custom$dataIn1)]])
      )
      nbPOVBefore <- length(which(m))
      
      withProgress(message = "", detail = "", value = 0, {
        incProgress(0.25, detail = "Find MEC blocks")
        
        .tmp <- NULL
        .param <- list()
        
        
        try({
          switch(rv.widgets$POVImputation_algorithm,
            None = .tmp <- rv.custom$dataIn1[[length(rv.custom$dataIn1)]],
            slsa = {
              incProgress(0.5, detail = "slsa Imputation")
              
              .tmp <- wrapper.impute.slsa(
                obj = rv.custom$dataIn1[[length(rv.custom$dataIn1)]],
                design = design.qf(rv.custom$dataIn1))
              
              .param <- list(
                POVImputation_algorithm = rv.widgets$POVImputation_algorithm
              )
            },
            detQuantile = {
              incProgress(0.5, detail = "det quantile Imputation")
              .tmp <- wrapper.impute.detQuant(
                obj = rv.custom$dataIn1[[length(rv.custom$dataIn1)]],
                qval = rv.widgets$POVImputation_detQuant_quantile / 100,
                factor = rv.widgets$POVImputation_detQuant_factor,
                na.type = 'Missing POV')
              .param <- list(
                POVImputation_algorithm = rv.widgets$POVImputation_algorithm,
                qval = rv.widgets$POVImputation_detQuant_quantile / 100,
                factor = rv.widgets$POVImputation_detQuant_factor,
                na.type = 'Missing POV')
            },
            KNN = {
              incProgress(0.5, detail = "KNN Imputation")
              
              .tmp <- wrapper.impute.KNN(
                obj = rv.custom$dataIn1[[length(rv.custom$dataIn1)]],
                grp = DaparToolshed::design.qf(rv.custom$dataIn1)$Condition,
                K = rv.widgets$POVImputation_KNN_n);
              .param <- list(
                POVImputation_algorithm = rv.widgets$POVImputation_algorithm,
                K = rv.widgets$POVImputation_KNN_n
              )
              
            },
            None = {}
          )
        })
        
        
        if(inherits(.tmp, "try-error") || inherits(.tmp, "try-warning")) {
          mod_SweetAlert_server(id = 'sweetalert_perform_POVimputation_button',
            text = .tmp,
            type = 'error' )
        } else {
          # sendSweetAlert(
          #   session = session,
          #   title = "Success",
          #   type = "success"
          # )
          #rv$dataIn[[length(rv$dataIn)]] <- .tmp
          # incProgress(0.75, detail = 'Reintroduce MEC blocks')
          incProgress(1, detail = "Finalize POV imputation")
          
          m <- match.metacell(DaparToolshed::qMetacell(.tmp),
            pattern = "Missing POV",
            level = DaparToolshed::typeDataset(.tmp)
          )
          nbPOVAfter <- length(which(m))
          rv$nbPOVimputed <- nbPOVBefore - nbPOVAfter
        }
        
        
        
        
        
        rv.custom$dataIn1 <- Prostar2::addDatasets(
          rv.custom$dataIn1,
          .tmp,
          'POVImputation')
        
        paramshistory(rv.custom$dataIn1[[length(rv.custom$dataIn1)]]) <- .param
        
        
        # Add infos
        nBefore <- nrow(rv.custom$dataIn1[[length(rv.custom$dataIn1) - 1]])
        nAfter <- nrow(rv.custom$dataIn1[[length(rv.custom$dataIn1)]])

        rv.custom$POVImputation_SummaryDT <- rbind(
          rv.custom$POVImputation_SummaryDT ,
          c("POV Imputation",
            nBefore - nAfter,
            0,
            NA)
          )

      })


     .history <- rv.custom$dataIn1[[length(rv.custom$dataIn1)]]
      rv.custom$params.tmp[['Imputation']][['POVImputation']] <- paramshistory(.history)
      
      
      rv.custom$dataIn2 <- rv.custom$dataIn1
      
      # DO NOT MODIFY THE THREE FOLLOWING LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status['POVImputation'] <- stepStatus$VALIDATED
      }
    })

    # <<< END ------------- Code for step 1 UI---------------
    
    
    # >>> START ------------- Code for step 2 UI---------------
    
    output$MECImputation <- renderUI({
      shinyjs::useShinyjs()
      #path <- file.path(system.file('www/css', package = 'MagellanNTK'),'MagellanNTK.css')
      #includeCSS(path)
     
      
      widget <- NULL
      .style <- "display:inline-block; vertical-align: middle; padding: 7px;"

      MagellanNTK::process_layout(
        ns = NS(id),
        sidebar = tagList(
          #timeline_process_ui(ns('MECImputation_timeline')),
          if (rv.custom$mv.present) {
            div(
              div(style = .style, uiOutput(ns("MECImputation_chooseImputationMethod_ui"))),
              div(style = .style, uiOutput(ns("MECImputation_Params_ui")))
            )
            
          }
        ),
        content = tagList(
          
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
      mod_mv_plots_server("MECImputation_mvplots",
        data = reactive({rv.custom$dataIn2[[length(rv.custom$dataIn2)]]}),
        grp = reactive({get_group(rv.custom$dataIn2)}),
        mytitle = reactive({"MEC imputation"}),
        pal = reactive({NULL}),
        pattern = reactive({c("Missing", "Missing POV", "Missing MEC")})
      )
    })

    
    output$MECImputation_chooseImputationMethod_ui <- renderUI({
      widget <- selectInput(ns("MECImputation_algorithm"), "Algorithm for MEC",
        choices = imputationAlgorithmsProteins_MEC,
        selected = rv.widgets$MECImputation_algorithm, 
        width = "150px"
      )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["MECImputation"])
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
        h5("The MEC will be imputed by the following values :"),
        mod_DetQuantImpValues_ui(ns("MECImputation_DetQuantValues_DT"))
      )
    })
    
    
    
    output$MECImputation_Params_ui <- renderUI({
      req(rv.widgets$MECImputation_algorithm)
      .style <- "display:inline-block; vertical-align: middle; padding: 7px;"
      
      widget <- switch(rv.widgets$MECImputation_algorithm,
        detQuantile = {
          tagList(
            div(style = .style,
              numericInput(ns("MECImputation_detQuant_quantile"), "Quantile",
                value = rv.widgets$MECImputation_detQuant_quantile,
                step = 0.5,
                min = 0,
                max = 100,
                width = "100px"
              )),
            div(style = .style,
              numericInput(ns("MECImputation_detQuant_factor"), "Factor",
                value = rv.widgets$MECImputation_detQuant_factor,
                step = 0.1, min = 0, max = 10,
                width = "100px"
              )
            )
          )
        },
        fixedValue = {
          div(style = .style,
            numericInput(ns("MECImputation_fixedValue"), "Fixed value",
              value = rv.widgets$MECImputation_fixedValue,
              step = 0.1, min = 0, max = 100,
              width = "100px"
            )
          )
        },
        None = {}
      )
      
      MagellanNTK::toggleWidget(widget, rv$steps.enabled["MECImputation"])
    })
    
  
    
    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
       
      req(grepl('MECImputation', btnEvents()))
      shiny::withProgress(message = paste0("Reseting process", id), {
        shiny::incProgress(0.5)
        
      if ( is.null(rv.custom$dataIn2) || 
          rv.widgets$MECImputation_algorithm == "None")
        info(btnVentsMasg)
      else {
        
        
      req(rv.custom$dataIn2)
      req(rv.widgets$MECImputation_algorithm != "None")
      withProgress(message = "", detail = "", value = 0, {
        incProgress(0.25, detail = "Reintroduce MEC")
        
        m <- match.metacell(
          DaparToolshed::qMetacell(rv.custom$dataIn2[[length(rv.custom$dataIn2)]]),
          pattern = "Missing MEC",
          level = DaparToolshed::typeDataset(rv.custom$dataIn2[[length(rv.custom$dataIn2)]])
        )
        nbMECBefore <- length(which(m))
        incProgress(0.75, detail = "MEC Imputation")
        
        #browser()
        withProgress(message = "", detail = "", value = 0, {
          incProgress(0.25, detail = "Find MEC blocks")
          
          .tmp <- NULL
          .param <- list()
          try({
            switch(rv.widgets$MECImputation_algorithm,
              None = .tmp <- rv.custom$dataIn2[[length(rv.custom$dataIn2)]],
              
              detQuantile = {
                incProgress(0.5, detail = "det quantile Imputation")
                .tmp <- wrapper.impute.detQuant(
                  obj = rv.custom$dataIn2[[length(rv.custom$dataIn2)]],
                  qval = rv.widgets$MECImputation_detQuant_quantile / 100,
                  factor = rv.widgets$MECImputation_detQuant_factor,
                  na.type = 'Missing MEC')
                .param <- list(
                  MECImputation_algorithm = rv.widgets$MECImputation_algorithm,
                  qval = rv.widgets$MECImputation_detQuant_quantile / 100,
                  factor = rv.widgets$MECImputation_detQuant_factor,
                  na.type = 'Missing MEC')
              },
              fixedValue = {
                .tmp <- wrapper.impute.fixedValue(
                  obj = rv.custom$dataIn2[[length(rv.custom$dataIn2)]],
                  fixVal = rv.widgets$MECImputation_fixedValue,
                  na.type = "Missing MEC"
                )
                .param <- list(
                  MECImputation_algorithm = rv.widgets$MECImputation_algorithm,
                  fixVal = rv.widgets$MECImputation_fixedValue,
                  na.type = 'Missing MEC')
              },
              None = {}
            )
          })
          
          if(inherits(.tmp, "try-error")) {
            mod_SweetAlert_server(id = 'sweetalert_perform_MECimputation_button',
              text = .tmp,
              type = 'error' )
          } else {
            # sendSweetAlert(
            #   session = session,
            #   title = "Success",
            #   type = "success"
            # )
            #rv$dataIn[[length(rv$dataIn)]] <- .tmp
            # incProgress(0.75, detail = 'Reintroduce MEC blocks')
            incProgress(1, detail = "Finalize MEC imputation")
            
            
            m <- match.metacell(DaparToolshed::qMetacell(.tmp),
              pattern = "Missing MEC",
              level = DaparToolshed::typeDataset(.tmp)
            )
            nbMECAfter <- length(which(m))
            rv$nbMECimputed <- nbMECBefore - nbMECAfter
            
          }
          
          
          rv.custom$dataIn2 <- Prostar2::addDatasets(
            rv.custom$dataIn2,
            .tmp,
            'MECImputation')
          
          paramshistory(rv.custom$dataIn2[[length(rv.custom$dataIn2)]]) <- .param # Do some stuff
      
      .history <- rv.custom$dataIn2[[length(rv.custom$dataIn2)]]
      rv.custom$params.tmp[['Imputation']][['MECImputation']] <- paramshistory(.history)
      
      
      
      # Add infos
      nBefore <- nrow(rv.custom$dataIn1[[length(rv.custom$dataIn1) - 1]])
      nAfter <- nrow(rv.custom$dataIn1[[length(rv.custom$dataIn1)]])
      
      rv.custom$MECImputation_SummaryDT <- rbind(
        rv.custom$MECImputation_SummaryDT ,
        c("MEC Imputation",
          nBefore - nAfter,
          0,
          NA)
      )
      
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- NULL
      rv$steps.status['MECImputation'] <- stepStatus$VALIDATED
    })
        
      })
      
      }
        
      })
    })
    
    # <<< END ------------- Code for step 2 UI---------------
    
    
    # >>> START ------------- Code for step 3 UI---------------
    output$Save <- renderUI({
     
      MagellanNTK::process_layout(
        ns = NS(id),
        sidebar = tagList(
          #timeline_process_ui(ns('Save_timeline'))
        ),
        content = tagList()
      )
    })
    
    # 
    # output$dl_ui <- renderUI({
    #   req(rv$steps.status['Save'] == stepStatus$VALIDATED)
    #   req(config@mode == 'process')
    #   
    #   MagellanNTK::download_dataset_ui(ns('createQuickLink'))
    # })
    

    observeEvent(req(btnEvents()), ignoreInit = TRUE, ignoreNULL = TRUE,{
      req(grepl('Save', btnEvents()))
      
      shiny::withProgress(message = paste0("Reseting process", id), {
        shiny::incProgress(0.5)
        
      if (isTRUE(all.equal(assays(rv$dataIn),assays(rv.custom$dataIn2))))
        info(btnVentsMasg)
      else {
      # Do some stuff
      len_start <- length(rv$dataIn)
      len_end <- length(rv.custom$dataIn2)
      len_diff <- len_end - len_start
      
      req(len_diff > 0)
      
      if (len_diff == 2)
        rv.custom$dataIn2 <- removeAssay(rv.custom$dataIn2, 
          length(rv.custom$dataIn2) - 1)
      
      
      # Rename the new dataset with the name of the process
      i <- length(rv.custom$dataIn2)
      names(rv.custom$dataIn2)[i] <- 'Imputation'
      DaparToolshed::paramshistory(rv.custom$dataIn2[[i]]) <- 
        rv.custom$params.tmp
      
      
      # DO NOT MODIFY THE THREE FOLLOWING LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv.custom$dataIn2
      rv$steps.status['Save'] <- stepStatus$VALIDATED
      
      # Prostar2::download_dataset_server('createQuickLink', 
      #   dataIn = reactive({rv.custom$dataIn2}))
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
