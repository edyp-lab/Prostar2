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
#' In this example, `PipelinePeptide_Normalization_ui()` and `PipelinePeptide_Normalization_server()` define
#' the code for the process `ProcessPeptide` which is part of the pipeline called `PipelinePeptide`.
#' 
#' @example inst/workflow/PipelinePeptide/examples/example_PipelinePeptide_Normalization.R
#' 
NULL

#' @rdname PipelinePeptide
#' @export
#' 
PipelinePeptide_Normalization_conf <- function(){
  MagellanNTK::Config(
    fullname = 'PipelinePeptide_Normalization',
    mode = 'process',
    steps = c('Normalization'),
    mandatory = c(FALSE)
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
PipelinePeptide_Normalization_ui <- function(id){
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
#' @importFrom stats setNames rnorm
#' 
#' @export
#' 
PipelinePeptide_Normalization_server <- function(id,
  dataIn = reactive({NULL}),
  steps.enabled = reactive({NULL}),
  remoteReset = reactive({0}),
  steps.status = reactive({NULL}),
  current.pos = reactive({1})
){
  
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
    init.dataset = NULL,
    history = NULL,
    selectProt = reactive({NULL})
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

    # >>>
    # >>> START ------------- Code for Description UI---------------
    # >>> 
    
    
    output$Description <- renderUI({
      # print('titi')
      # print(session$userData$workflow.path)
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
      rv.custom$init.dataset <- dataIn()
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Description'] <- stepStatus$VALIDATED
    })
    
    
    
    # >>>
    # >>> START ------------- Code for Normalization UI---------------
    # >>> 
    
    # >>>> -------------------- STEP 1 : Global UI ------------------------------------
    output$Normalization <- renderUI({
      shinyjs::useShinyjs()
      
      .style <- "display:inline-block; vertical-align: middle; 
      padding-right: 20px;"
      
      wellPanel(
        # uiOutput for all widgets in this UI
        # This part is mandatory
        # The renderUI() function of each widget is managed by MagellanNTK
        # The dev only have to define a reactive() function for each
        # widget he want to insert
        # Be aware of the naming convention for ids in uiOutput()
        # For more details, please refer to the dev document.
        
        
        tagList(

          div(style = .style,
            uiOutput(ns("Normalization_btn_validate_ui"))
          ),
          div(
            div(style = .style,
              uiOutput(ns('Normalization_method_ui'))
            ),
            
              div(id = "div_Normalization_type_ui",
                style = .style,
                shinyjs::hidden(uiOutput(ns('Normalization_type_ui')))
            ),
            div(style = .style,
              shinyjs::hidden(uiOutput(ns('Normalization_spanLOESS_ui'))),
              #module_Not_a_numericUI(ns("test_spanLOESS")),
              uiOutput(ns("Normalization_quantile_ui")),
              uiOutput(ns("Normalization_varReduction_ui"))
            ),
            div(style = .style,
              uiOutput(ns('tracking')),
              shinyjs::hidden(uiOutput(ns("Normalization_sync_ui"))
              )
            )
          ),
          tags$hr()
          # ,div(style = .style,
          #   omXplore::omXplore_intensity_ui(ns("boxPlot_Norm"))
          # )
           ,fluidRow(
             column(width = 5,
               omXplore::omXplore_density_ui(ns("densityPlot_Norm"))
               ),
             column(width = 5,
               omXplore::omXplore_intensity_ui(ns("boxPlot_Norm"))
             ),
             column(width = 5,
               highcharter::highchartOutput(ns("viewComparisonNorm_hc"))
            )
          )
        )
        
      )
    })
    
    
    
     # observe({
     #   rv$dataIn
     #   remoteReset()
     #  rv.custom$
        selectProt <- omXplore::plots_tracking_server(
        id = "tracker",
        obj = reactive({rv$dataIn[[length(rv$dataIn)]]}),
        remoteReset = reactive({remoteReset()})
      )
    #})
    

    #   req(rv.custom$init.dataset)
    #   req(rv$dataIn)
    #   remoteReset()
     # rv.custom$selectProt()$value
    omXplore::omXplore_density_server("densityPlot_Norm", 
      obj = reactive({rv$dataIn}),
      i = reactive({length(rv$dataIn)})
    )
    

   observe({
     rv.custom$selectProt()
     rv$dataIn
     
    omXplore::omXplore_intensity_server("boxPlot_Norm",
      obj = reactive({rv$dataIn}),
      i = reactive({length(rv$dataIn)}),
      track.indices = reactive({selectProt()$indices}),
      remoteReset = reactive({remoteReset()}),
      is.enabled = reactive({!rv$steps.enabled['Normalization']})
    )
    
     })
    
   
   observeEvent(rv.custom$selectProt()$indices, ignoreNULL = FALSE,{
   
     print("in observeEvent(selectProt()$indices")
     print('new value fo selectProt()$indices = ')
     print(selectProt()$indices)

   })

    # >>> START: Definition of the widgets
    
    # This part must be customized by the developer of a new module
    output$Normalization_method_ui <- renderUI({
      widget <- selectInput(
        ns('Normalization_method'),
        "Normalization method",
        choices = DaparToolshed::normalizeMethods(),
        selected = rv.widgets$Normalization_method,
        width = '250px')
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Normalization'] )
    })
    
    
    output$Normalization_type_ui <- renderUI({
      widget <- selectInput(ns('Normalization_type'),
        "Normalization type",
        choices = stats::setNames(
          nm = c("overall", "within conditions")),
        selected = rv.widgets$Normalization_type,
        width = '150px')
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Normalization'] )
    })
    
    
    output$Normalization_spanLOESS_ui <- renderUI({
      widget <- textInput(
        ns('Normalization_spanLOESS'),
        'Span',
        value = rv.widgets$Normalization_spanLOESS,
        width = '100px')
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Normalization'] )
    })
    
    
    output$Normalization_quantile_ui <- renderUI({
      req(rv.widgets$Normalization_method == "QuantileCentering")
      widget <- textInput(
        ns('Normalization_quantile'),
        MagellanNTK::mod_popover_for_help_ui(ns('quantile_help')),
        value = rv.widgets$Normalization_quantile,
        width = '100px')
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Normalization'] )
    })
    
    
    output$Normalization_varReduction_ui <- renderUI({
      req(rv.widgets$Normalization_method == "MeanCentering")
      widget <- checkboxInput(
        ns('Normalization_varReduction'),
        "Include variance reduction",
        value = rv.widgets$Normalization_varReduction
        )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Normalization'] )
    })
    
    
    output$Normalization_sync_ui <- renderUI({
      widget <- checkboxInput(
          ns('Normalization_sync'),
          "Synchronise with selection above",
          value = rv.widgets$Normalization_sync
          )
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Normalization'] )
    })
    

    output$tracking <- renderUI({
      req(rv.widgets$Normalization_method %in% c('QuantileCentering', 'MeanCentering', 'SumByColumns'))
      widget <-  omXplore::plots_tracking_ui(ns("tracker"))
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Normalization'] )
    })
    
   
    
    
    output$viewComparisonNorm_hc <- highcharter::renderHighchart({
      req(rv$dataIn)
      req(length(rv$dataIn) > 1)
      obj1 <- rv$dataIn[[length(rv$dataIn)]]
      obj2 <- rv$dataIn[[length(rv$dataIn)-1]]
      
      protId <- omXplore::get_colID(rv$dataIn[[length(rv$dataIn)]])
      #browser()
      DaparToolshed::compareNormalizationD_HC(
        qDataBefore = SummarizedExperiment::assay(obj1),
        qDataAfter = SummarizedExperiment::assay(obj2),
        keyId = rowData(rv$dataIn[[length(rv$dataIn)]])[, protId],
        conds = omXplore::get_group(rv$dataIn),
        pal = NULL,
        # Consider only 2% of the entire dataset
        n = if (!is.null(rv.custom$selectProt()$indices)) {
          length(selectProt()$indices)
        } else {
          floor(0.02 * nrow(SummarizedExperiment::assay(obj1)))
        },
        subset.view = if (!is.null(selectProt()$indices)) {
          selectProt()$indices
        } else {
          seq(nrow(obj1))
        }
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
 
      cond <- metadata(rv$dataIn[[length(rv$dataIn)]])[['typeDataset']] == "peptide"
      
      .meths <- DaparToolshed::normalizeMethods('withTracking')
      trackAvailable <- rv.widgets$Normalization_method %in% .meths
      shinyjs::toggle("Normalization_sync_ui",
        condition = cond && trackAvailable)
      
      shinyjs::toggle("Normalization_sync_ui",
        condition = cond && trackAvailable)
      
    })
    

    
    
    MagellanNTK::mod_popover_for_help_server(id = 'quantile_help',
      title = "Normalization quantile",
      content = "lower limit/noise (quantile = 0.15),
            median (quantile = 0.5). Min value=0, max value=1"
    )
    

    
    
    

    
    output$Normalization_btn_validate_ui <- renderUI({
      widget <-  actionButton(ns("Normalization_btn_validate"),
                              "Run Normalization",
                              class = "btn-success")
      MagellanNTK::toggleWidget(widget, rv$steps.enabled['Normalization'] )
      
    })
    # >>> END: Definition of the widgets
    
    observeEvent(input$Normalization_btn_validate, {
      # Do some stuff 
      req(rv.widgets$Normalization_method)
      req(rv$dataIn)
      

      rv.custom$tmpAssay <- NULL
      try({
        .conds <- colData(rv$dataIn)[, "Condition"]
        qdata <- SummarizedExperiment::assay(rv$dataIn[[length(rv$dataIn)]])
        
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
      
      if(inherits(rv.custom$tmpAssay, "try-error")) {
        
        MagellanNTK::mod_SweetAlert_server(id = 'sweetalert_perform_normalization',
          text = rv.custom$tmpAssay[[1]],
          type = 'error' )
      } else {
 
        new.dataset <- rv$dataIn[[length(rv$dataIn)]]
        assay(new.dataset) <- rv.custom$tmpAssay
        DaparToolshed::paramshistory(new.dataset) <- NULL
        DaparToolshed::paramshistory(new.dataset) <- rv.custom$history
        rv$dataIn <- QFeatures::addAssay(rv$dataIn, new.dataset, 'Normalization')
 
        # DO NOT MODIFY THE THREE FOLLOWING LINES
        dataOut$trigger <- MagellanNTK::Timestamp()
        dataOut$value <- NULL
        rv$steps.status['Normalization'] <- stepStatus$VALIDATED
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
          actionButton(ns("Save_btn_validate"), "Save",
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
      
      # DO NOT MODIFY THE THREE FOLLOWINF LINES
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
      rv$steps.status['Save'] <- stepStatus$VALIDATED
      Prostar2::download_dataset_server('createQuickLink', dataIn = reactive({rv$dataIn}))
      
    })
    # <<< END ------------- Code for step 3 UI---------------
    
    
    
    # Insert necessary code which is hosted by MagellanNTK
    # DO NOT MODIFY THIS LINE
    eval(parse(text = MagellanNTK::Module_Return_Func()))
  }
  )
}
