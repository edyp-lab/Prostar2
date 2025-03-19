#' @title Filtering Shiny module
#'
#' @description
#' This function is a shiny module to xxx
#' This function is written with specifications of the package `MagellanNTK` so
#' as to be easily integrated into workflfow compliant with `MagellanNTK`.
#'
#' @name mod_Prot_Imputation_POV
#'
#' @return As for all modules used with `MagellanNTK`, the return value is a
#' `list()` of two items:
#' - trigger : xxx
#' - value: In this case, it contains a list() of three slots:
#'   - ll.var: a list() of instances of the class `Filtering`,
#'   - ll.query: a list of `character()` which describe the queries in natural
#'   language,
#'   - ll.widgets.value: a list of the values of widgets.
#'
#' @examples
#' \dontrun{
#' data(Exp1_R25_pept, package="DaparToolshedData")
#' obj <- Exp1_R25_pept[seq_len(100)]
#' shiny::runApp(mod_Prot_Imputation_POV(obj, 1))
#' }
#' 
NULL





#' @export
#'
#' @rdname mod_Prot_Imputation_POV
#'
mod_Prot_Imputation_POV_ui <- function(id) {
  ns <- NS(id)
  .localStyle <- "display:inline-block; vertical-align: top;
                  padding-right: 20px;"
  wellPanel(
    # uiOutput for all widgets in this UI
    # This part is mandatory
    # The renderUlength(rv$dataIn) function of each widget is managed by MagellanNTK
    # The dev only have to define a reactive() function for each
    # widget he want to insert
    # Be aware of the naming convention for ids in uiOutput()
    # For more details, please refer to the dev document.
    
    tags$div(
      tags$div(style = .localStyle, uiOutput(ns("POV_algorithm_UI"))),
      tags$div(style = .localStyle, uiOutput(ns("POV_KNN_nbNeighbors_UI"))),
      tags$div(style = .localStyle, uiOutput(ns("POV_detQuant_UI"))),
      tags$div(style = .localStyle, uiOutput(ns("POV_showDetQuantValues")))
    ),
    # Insert validation button
    uiOutput(ns("mod_Prot_Imputation_POV_btn_validate_ui")),
    htmlOutput("helpForImputation"),
    tags$hr(),
    uiOutput(ns('mvplots_ui'))
  )
}




#' @param id xxx
#' @param obj An instance of the class `QFeatures`
#' @param remoteReset A `integer(1)` xxxx
#' @param is.enabled A `logical(1)` that indicates whether the module is
#' enabled or disabled. This is a remote command.
#'
#' @rdname mod_Prot_Imputation_POV
#'
#' @export
#'
mod_Prot_Imputation_POV_server <- function(id,
  obj = reactive({NULL}),
  i = reactive({NULL}),
  remoteReset = reactive({0}),
  is.enabled = reactive({TRUE})) {
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list(
    POV_algorithm = NULL,
    POV_KNN_n = 10,
    POV_detQuant_quantile = 2.5,
    POV_detQuant_factor = 1
  )
  
  rv.custom.default.values <- list(
    
  )
  
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    .localStyle <- "display:inline-block; vertical-align: top; padding-right: 20px;"
    # eval(
    #   str2expression(
    #     MagellanNTK::Get_AdditionalModule_Core_Code(
    #       w.names = names(widgets.default.values),
    #       rv.custom.names = names(rv.custom.default.values)
    #     )
    #   )
    # )
    
    core <- paste0(
      MagellanNTK::Get_Code_Declare_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_ObserveEvent_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_rv_reactiveValues(),
      MagellanNTK::Get_Code_Declare_rv_custom(names(rv.custom.default.values)),
      MagellanNTK::Get_Code_for_dataOut(),
      MagellanNTK::Get_Code_for_remoteReset(widgets = TRUE,
        custom = TRUE,
        dataIn = 'obj()'),
      sep = "\n"
    )
    
    eval(str2expression(core))
    

    observeEvent(obj(), ignoreNULL = TRUE, ignoreInit = FALSE, {
      req(obj())
      stopifnot(inherits(obj(), 'QFeatures'))
      rv$dataIn <- obj()
    }, priority = 1000)
    
    

    output$mvplots_ui <- renderUI({
      widget <- mod_mv_plots_ui(ns("mvplots"))
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    
    observe({
      req(rv$dataIn)
  
    mod_mv_plots_server("mvplots",
      data = reactive({rv$dataIn[[length(rv$dataIn)]]}),
      grp = reactive({get_group(rv$dataIn)}),
      mytitle = reactive({"POV imputation"}),
      pal = reactive({NULL}),
      pattern = reactive({c("Missing", "Missing POV", "Missing MEC")})
      )
    })
    

    output$POV_algorithm_UI <- renderUI({

      widget <- selectInput(ns("POV_algorithm"), "Algorithm for POV",
        choices = list(
          "None" = "None",
          "slsa" = "slsa",
          "Det quantile" = "detQuantile",
          "KNN" = "KNN"
        ),
        selected = rv.widgets$POV_algorithm,
        width = "150px"
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    
    output$POV_showDetQuantValues <- renderUI({
      req(rv.widgets$POV_algorithm == "detQuantile")
      
      mod_DetQuantImpValues_server(id = "POV_DetQuantValues_DT",
        obj = reactive({rv$dataIn[[length(rv$dataIn)]]}),
        quant = reactive({rv.widgets$POV_detQuant_quantile}),
        factor = reactive({rv.widgets$POV_detQuant_factor})
      )
      
       tagList(
          h5("The POV will be imputed by the following values :"),
          mod_DetQuantImpValues_ui(ns("POV_DetQuantValues_DT"))
        )
    })
    
    
    
    output$POV_KNN_nbNeighbors_UI <- renderUI({
      req(rv.widgets$POV_algorithm == 'KNN')
     
      widget <- numericInput(ns("POV_KNN_nbNeighbors"), "Neighbors",
        value = rv.widgets$POV_KNN_n, step = 1, min = 0,
        max = max(nrow(rv$dataIn), rv.widgets$POV_KNN_n),
        width = "100px"
      )
      
      MagellanNTK::toggleWidget(widget, is.enabled())
    })

    
    output$POV_detQuant_UI <- renderUI({
      req(rv.widgets$POV_algorithm == 'detQuantile')
      
      widget <- tagList(
        tags$div(style = .localStyle,
        numericInput(ns("POV_detQuant_quantile"), "Quantile",
          value = rv.widgets$POV_detQuant_quantile,
          step = 0.5, min = 0, max = 100, width = "100px"
        )
      ),
      tags$div(style = .localStyle,
        numericInput(ns("POV_detQuant_factor"), "Factor",
          value = rv.widgets$POV_detQuant_factor,
          step = 0.1, min = 0, max = 10, width = "100px"
        )
      )
      )
      
      MagellanNTK::toggleWidget(widget, is.enabled())

    })
    


    
    output$mod_Prot_Imputation_POV_btn_validate_ui <- renderUI({

      widget <- actionButton(ns("mod_Prot_Imputation_POV_btn_validate"),
        "Validate step", class = "btn-success")
      
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    # >>> END: Definition of the widgets
    
    
    observeEvent(input$mod_Prot_Imputation_POV_btn_validate, {

      req(rv$dataIn)
      req(rv.widgets$POV_algorithm != "None")
      m <- match.metacell(
        omXplore::get_metacell(rv$dataIn[[length(rv$dataIn)]]),
        pattern = "Missing POV",
        level = omXplore::get_type(rv$dataIn[[length(rv$dataIn)]])
      )
      nbPOVBefore <- length(which(m))
      #browser()
      withProgress(message = "", detail = "", value = 0, {
        incProgress(0.25, detail = "Find MEC blocks")
        
        .tmp <- NULL
        .param <- list()
        
      
        try({
          switch(rv.widgets$POV_algorithm,
            None = .tmp <- rv$dataIn[[length(rv$dataIn)]],
            slsa = {
              incProgress(0.5, detail = "slsa Imputation")
              .tmp <- wrapper.impute.slsa(
                obj = rv$dataIn[[length(rv$dataIn)]],
                grp = omXplore::get_group(rv$dataIn),
                coldata = MultiAssayExperiment::colData(rv$dataIn)
                )
              .param <- list(
                  POV_algorithm = rv.widgets$POV_algorithm
                )
            },
            detQuantile = {
              incProgress(0.5, detail = "det quantile Imputation")
              .tmp <- wrapper.impute.detQuant(
                  obj = rv$dataIn[[length(rv$dataIn)]],
                  qval = rv.widgets$POV_detQuant_quantile / 100,
                  factor = rv.widgets$POV_detQuant_factor,
                  na.type = 'Missing POV')
              .param <- list(
                  POV_algorithm = rv.widgets$POV_algorithm,
                  qval = rv.widgets$POV_detQuant_quantile / 100,
                  factor = rv.widgets$POV_detQuant_factor,
                  na.type = 'Missing POV')
            },
            KNN = {
              incProgress(0.5, detail = "KNN Imputation")
              
              .tmp <- wrapper.impute.KNN(
                obj = rv$dataIn[[length(rv$dataIn)]],
                grp = omXplore::get_group(rv$dataIn),
                K = rv.widgets$POV_KNN_n);
              .param <- list(
                POV_algorithm = rv.widgets$POV_algorithm,
                K = rv.widgets$POV_KNN_n
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

          
          m <- match.metacell(omXplore::get_metacell(.tmp),
            pattern = "Missing POV",
            level = omXplore::get_type(.tmp)
          )
          nbPOVAfter <- length(which(m))
          rv$nbPOVimputed <- nbPOVBefore - nbPOVAfter
        }

      rv$dataIn <- Prostar2::addDatasets(
        rv$dataIn,
        .tmp,
        'POVImputation')
      
      paramshistory(rv$dataIn[[length(rv$dataIn)]]) <- .param
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn
    })
      
    })
    
    return(reactive({dataOut}))
  })
}




#' @export
#' @rdname mod_Prot_Imputation_POV
#' 
mod_Prot_Imputation_POV <- function(obj, i){
  ui <- mod_Prot_Imputation_POV_ui('pov')
  
  server <- function(input, output, session){
    
    res <- mod_Prot_Imputation_POV_server('pov',
      obj = reactive({obj}),
      i = reactive({i}))
    
    observeEvent(res()$trigger, {
      print(res()$value)
    })
  }
  
  app <- shiny::shinyApp(ui, server)
  
}