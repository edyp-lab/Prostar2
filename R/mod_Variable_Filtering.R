#' @title Filtering Shiny module
#'
#' @description
#' This function is a shiny module to create a list of queries (instances of 
#' the class `Filtering` to filter the quantitative metadata of an instance
#' of the class `SummarizedExperiment`).
#' This function is written with specifications of the package `MagellanNTK` so
#' as to be easily integrated into workflfow compliant with `MagellanNTK`.
#'
#' @name mod_Variable_Filtering
#' 
#' @param id xxx
#' @param obj An instance of the class `QFeatures`
#' @param keep_vs_remove A character(1) indicating whether to keep or delete 
#' items. Default value is "delete"
#' @param operator xxx
#' @param remoteReset A `ìnteger(1)` xxxx
#' @param is.enabled A `logical(1)` that indicates whether the module is
#' enabled or disabled. This is a remote command.
#'
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
#' data(Exp1_R25_prot, package = 'DaparToolshedData')
#' shiny::runApp(mod_Variable_Filtering(Exp1_R25_prot, 1))
#' }
#' 
NULL





#' @export
#'
#' @rdname mod_Variable_Filtering
#'
mod_Variable_Filtering_ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    # uiOutput for all widgets in this UI
    # This part is mandatory
    # The renderUlength(obj()) function of each widget is managed by MagellanNTK
    # The dev only have to define a reactive() function for each
    # widget he want to insert
    # Be aware of the naming convention for ids in uiOutput()
    # For more details, please refer to the dev document.
    
    uiOutput(ns("variable_buildQuery_ui")),
    uiOutput(ns("variable_Filter_DT"))
    
    # Insert validation button
    #uiOutput(ns("variable_btn_validate_ui")),
  )
}





#' @rdname mod_Variable_Filtering
#'
#' @export
#'
mod_Variable_Filtering_server <- function(id,
  obj = reactive({NULL}),
  i = reactive({1}),
  remoteReset = reactive({0}),
  is.enabled = reactive({TRUE})) {
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list()
  
  rv.custom.default.values <- list(
    indices = NULL,
    query = list(),
    widgets.value = list(),
    variable_Filter_SummaryDT = data.frame(
      query = NA,
      nbDeleted = NA,
      TotalBeforeFiltering = NA,
      TotalAfterFiltering = NA,
      stringsAsFactors = FALSE
    ), 
    history = list()
  )
  
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    
    core <- paste0(
      MagellanNTK::Get_Code_Declare_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_ObserveEvent_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_rv_reactiveValues(),
      MagellanNTK::Get_Code_Declare_rv_custom(names(rv.custom.default.values)),
      MagellanNTK::Get_Code_for_dataOut(),
      MagellanNTK::Get_Code_for_remoteReset(),
      sep = "\n"
    )
    
    eval(str2expression(core))
    
    
    observeEvent(req(obj()), ignoreNULL = FALSE, ignoreInit = FALSE, {
      stopifnot(inherits(obj(), 'QFeatures'))
      rv$dataIn <- obj()
    }, priority = 1000)
    

   
    output$variable_Filter_DT <- renderUI({
      MagellanNTK::format_DT_server("dt", 
        obj = reactive({rv.custom$variable_Filter_SummaryDT}))
      
      MagellanNTK::format_DT_ui(ns("dt"))
    })
    

    output$variable_buildQuery_ui <- renderUI({
      widget <- mod_VariableFilter_Generator_ui(ns("query"))
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    funFilter <- mod_VariableFilter_Generator_server(
      id = "query",
      obj = reactive({rv$dataIn[[length(rv$dataIn)]]}),
      is.enabled = reactive({is.enabled()}),
      remoteReset = reactive({remoteReset()})
    )
    
    observeEvent(funFilter()$trigger, ignoreInit = TRUE, ignoreNULL = TRUE, {
      req(length(funFilter()$value$ll.var) > 0)
      req(rv$dataIn)

      tmp <- filterFeaturesOneSE(
        object = rv$dataIn,
        i = length(rv$dataIn),
        name = paste0("variableFiltered", MagellanNTK::Timestamp()),
        filters = funFilter()$value$ll.var
      )
      indices <- funFilter()$value$ll.indices
      
      # Add infos
      
      nBefore <- nrow(tmp[[length(tmp) - 1]])
      nAfter <- nrow(tmp[[length(tmp)]])
      
      
      .html <- funFilter()$value$ll.query
      .nbDeleted <- nBefore - nAfter
      .nbBefore <- nrow(assay(obj()))
      .nbAfter <- nrow(assay(tmp[[length(tmp)]]))
      
      rv.custom$variable_Filter_SummaryDT <- rbind(
        rv.custom$variable_Filter_SummaryDT , 
        c(.html, .nbDeleted, .nbBefore, .nbAfter))
      print('update du DT')
      # Keeps only the last filtered SE
      len_start <- length(obj())
      len_end <- length(tmp)
      len_diff <- len_end - len_start

      req(len_diff > 0)
      
      if (len_diff == 2)
        rv$dataIn <- QFeatures::removeAssay(tmp, length(tmp)-1)
      else 
        rv$dataIn <- tmp
      
      
     
      # Rename the new dataset with the name of the process
      names(rv$dataIn)[length(rv$dataIn)] <- 'Variable_Filtering'
      

      query <- funFilter()$value$ll.query
      i <- length(rv$dataIn)
      .history <- DaparToolshed::paramshistory(rv$dataIn[[i]])[['Variable_Filtering']]
      .history[[paste0('query_', length(.history))]] <- query
      DaparToolshed::paramshistory(rv$dataIn[[i]])[['Variable_Filtering']] <- .history
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn 
    })
    
    
    
    return(reactive({dataOut}))
  })
}



#' @export
#' @rdname mod_Variable_Filtering
#' 
mod_Variable_Filtering <- function(obj, i){
  ui <- tagList(
    actionButton('Reset', "Reset"),
    mod_Variable_Filtering_ui('query')
  )
  
  server <- function(input, output, session){
    
    res <- mod_Variable_Filtering_server('query',
      obj = reactive({obj}),
      i = reactive({i}),
      remoteReset = reactive({input$Reset})
      )
    
    observeEvent(res()$trigger, {
      print(res()$value)
    })
  }
  
  app <- shiny::shinyApp(ui, server)
  
}