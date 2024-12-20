#' @title Filtering Shiny module
#'
#' @description
#' This function is a shiny module to create a list of queries (instances of 
#' the class `Filtering` to filter the quantitative metadata of an instance
#' of the class `SummarizedExperiment`).
#' This function is written with specifications of the package `MagellanNTK` so
#' as to be easily integrated into workflfow compliant with `MagellanNTK`.
#'
#' @name mod_Metacell_Filtering
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
#' shiny::runApp(mod_Metacell_Filtering(Exp1_R25_prot, 1))
#' }
#' 
NULL





#' @export
#'
#' @rdname mod_Metacell_Filtering
#'
mod_Metacell_Filtering_ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    # uiOutput for all widgets in this UI
    # This part is mandatory
    # The renderUlength(rv$dataIn) function of each widget is managed by MagellanNTK
    # The dev only have to define a reactive() function for each
    # widget he want to insert
    # Be aware of the naming convention for ids in uiOutput()
    # For more details, please refer to the dev document.
    
    uiOutput(ns("Quantimetadatafiltering_buildQuery_ui")),
    
    uiOutput(ns("qMetacell_Filter_DT"))
    ,uiOutput(ns('plots_ui'))
    
    # Insert validation button
    # uiOutput(ns("Quantimetadatafiltering_btn_validate_ui"))
  )
}




#' @param id xxx
#' @param obj An instance of the class `QFeatures`
#' @param keep_vs_remove A character(1) indicating whether to keep or delete 
#' items. Default value is "delete"
#' @param operator xxx
#' @param remoteReset A `ìnteger(1)` xxxx
#' @param is.enabled A `logical(1)` that indicates whether the module is
#' enabled or disabled. This is a remote command.
#'
#' @rdname mod_Metacell_Filtering
#'
#' @export
#'
mod_Metacell_Filtering_server <- function(id,
  obj = reactive({NULL}),
  i = reactive({1}),
  remoteReset = reactive({0}),
  is.enabled = reactive({TRUE})) {
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list()
  
  rv.custom.default.values <- list(
    indices = NULL,
    Filtering = NULL,
    query = list(),
    fun.list = list(),
    widgets.value = list(),
    funFilter = reactive({NULL}),
    qMetacell_Filter_SummaryDT = NULL, 
    df = data.frame(),
    history = list()
  )
  
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    eval(
      str2expression(
        MagellanNTK::Get_AdditionalModule_Core_Code(
          w.names = names(widgets.default.values),
          rv.custom.names = names(rv.custom.default.values)
        )
      )
    )
    
    
    observeEvent(req(obj()), ignoreNULL = TRUE,{
      
      stopifnot(inherits(obj(), 'QFeatures'))
      rv$dataIn <- obj()
      rv.custom$qMetacell_Filter_SummaryDT <- data.frame(
        query = "-",
        nbDeleted = "0",
        TotalMainAssay = nrow(rv$dataIn[[length(rv$dataIn)]]),
        stringsAsFactors = FALSE
      )
      
      
      mod_ds_metacell_Histos_server(
        id = "plots",
        obj = reactive({rv$dataIn[[length(rv$dataIn)]]}),
        pattern = reactive({rv.custom$funFilter()$value$ll.pattern}),
        group = reactive({omXplore::get_group(rv$dataIn)})
      )
    }, priority = 1000)
    
    
    
    output$plots_ui <- renderUI({
      req(rv.custom$funFilter()$value$ll.pattern)
      
      widget <- mod_ds_metacell_Histos_ui(ns("plots"))
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    
    MagellanNTK::format_DT_server("dt", 
      obj = reactive({rv.custom$qMetacell_Filter_SummaryDT}))
    
    
    output$qMetacell_Filter_DT <- renderUI({
      req(rv.custom$qMetacell_Filter_SummaryDT)
      MagellanNTK::format_DT_ui(ns("dt"))
    })
    
    
    observe({
      req(is.enabled())
      req(rv$dataIn)
      
      rv.custom$funFilter <- mod_qMetacell_FunctionFilter_Generator_server(
        id = "query",
        obj = reactive({rv$dataIn[[length(rv$dataIn)]]}),
        conds = reactive({omXplore::get_group(rv$dataIn)}),
        keep_vs_remove = reactive({stats::setNames(c('Push p-value', 'Keep original p-value'), nm = c("delete", "keep"))}),
        val_vs_percent = reactive({stats::setNames(nm = c("Count", "Percentage"))}),
        operator = reactive({stats::setNames(nm = SymFilteringOperators())}),
        remoteReset = reactive({remoteReset()}),
        is.enabled = reactive({is.enabled()})
      )
    })
    
    output$Quantimetadatafiltering_buildQuery_ui <- renderUI({
      widget <- mod_qMetacell_FunctionFilter_Generator_ui(ns("query"))
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    
    # output$Quantimetadatafiltering_btn_validate_ui <- renderUI({
    # 
    #   req(length(rv.custom$funFilter()$value$ll.fun) > 0)
    #   
    #   widget <- actionButton(ns("Quantimetadatafiltering_btn_validate"),
    #     "Perform qMetacell filtering",
    #     class = "btn-success"
    #   )
    #   
    #   MagellanNTK::toggleWidget(widget, is.enabled())
    # })
    # >>> END: Definition of the widgets
    
    
    # observeEvent(input$Quantimetadatafiltering_btn_validate, {
    observeEvent(req(rv.custom$funFilter()$trigger), {
      req(length(rv.custom$funFilter()$value$ll.fun) > 0)
      req(rv$dataIn)
      
      tmp <- filterFeaturesOneSE(
        object = rv$dataIn,
        i = length(rv$dataIn),
        name = paste0("qMetacellFiltered", MagellanNTK::Timestamp()),
        filters = rv.custom$funFilter()$value$ll.fun
      )
      indices <- rv.custom$funFilter()$value$ll.indices
      
      #browser()
      # Add infos
      nBefore <- nrow(tmp[[length(tmp) - 1]])
      nAfter <- nrow(tmp[[length(tmp)]])
      
      #.html <- ConvertListToHtml(rv.custom$funFilter()$value$ll.query)
      
      .html <- rv.custom$funFilter()$value$ll.query
      .nbDeleted <- nBefore - nAfter
      .nbRemaining <- nrow(assay(tmp[[length(tmp)]]))
      
      rv.custom$qMetacell_Filter_SummaryDT <- rbind(
        rv.custom$qMetacell_Filter_SummaryDT , 
        c(.html, .nbDeleted, .nbRemaining))
      
      
      
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
      names(rv$dataIn)[length(rv$dataIn)] <- 'qMetacellFiltering'
      
      # Add params
      #par <- rv.custom$funFilter()$value$ll.widgets.value
      query <- rv.custom$funFilter()$value$ll.query
      i <- length(rv$dataIn)
      .history <- DaparToolshed::paramshistory(rv$dataIn[[i]])[['Metacell_Filtering']]
      
      .history[[paste0('query_', length(.history))]] <- query
      DaparToolshed::paramshistory(rv$dataIn[[i]])[['Metacell_Filtering']] <- .history
      
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn 
    })
    
    return(reactive({dataOut}))
  })
}



#' @export
#' @rdname mod_Metacell_Filtering
#' 
mod_Metacell_Filtering <- function(obj = NULL, i = 1){
  ui <- mod_Metacell_Filtering_ui('query')
  
  server <- function(input, output, session){
    
    res <- mod_Metacell_Filtering_server('query',
      obj = reactive({obj}),
      i = reactive({i}))
    
    observeEvent(res()$trigger, {
      print(res()$value)
    })
  }
  
  app <- shiny::shinyApp(ui, server)
  
}