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
#' @examplesIf interactive()
#' data(Exp1_R25_prot, package = 'DaparToolshedData')
#' shiny::runApp(mod_Variable_Filtering(Exp1_R25_prot, 1))
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
  remoteReset = reactive({NULL}),
  is.enabled = reactive({TRUE})) {
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list()
  
  rv.custom.default.values <- list(
    indices = NULL,
    query = list(),
    widgets.value = list(),
    funFilter = reactive({NULL}),
    variable_Filter_SummaryDT = data.frame(
      query = "-",
      nbDeleted = "-",
      TotalMainAssay = "-",
      stringsAsFactors = FALSE
    )
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
    }, priority = 1000)
    

    MagellanNTK::format_DT_server("dt", 
      obj = reactive({rv.custom$variable_Filter_SummaryDT}))

    output$variable_Filter_DT <- renderUI({
     
      req(rv.custom$variable_Filter_SummaryDT)
      MagellanNTK::format_DT_ui(ns("dt"))
    })
    
    
    observe({
      req(is.enabled())
      req(obj())
      rv.custom$funFilter <- mod_VariableFilter_Generator_server(
        id = "query",
        obj = reactive({obj()[[length(obj())]]}),
        is.enabled = reactive({is.enabled()}),
        remoteReset = reactive({remoteReset()})
      )
    })
    

    output$variable_buildQuery_ui <- renderUI({
      widget <- mod_VariableFilter_Generator_ui(ns("query"))
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    
    
    observeEvent(rv.custom$funFilter()$trigger, {
      req(rv$dataIn)
 
      tmp <- filterFeaturesOneSE(
        object = rv$dataIn,
        i = length(rv$dataIn),
        name = "variableFiltered",
        filters = rv.custom$funFilter()$value$ll.var
      )
      
      # Add infos
      
      nBefore <- nrow(tmp[[length(tmp) - 1]])
      nAfter <- nrow(tmp[[length(tmp)]])
      
      
      .html <- rv.custom$funFilter()$value$ll.query
      .nbDeleted <- nBefore - nAfter
      .nbRemaining <- nrow(assay(tmp[[length(tmp)]]))
      
      rv.custom$variable_Filter_SummaryDT <- rbind(
        rv.custom$variable_Filter_SummaryDT , 
        c(.html, .nbDeleted, .nbRemaining))

      .history <- DaparToolshed::paramshistory(tmp[[length(tmp)]])[['Filtering']][['Variable_Filtering']]
      
      ll.query <-  rv.custom$funFilter()$value$ll.widgets.value
      .history <- append(.history, ll.query)
      DaparToolshed::paramshistory(tmp[[length(tmp)]])[['Filtering']][['Variable_Filtering']] <- .history
      
      
      rv$dataIn <- tmp
      
      
      
      dataOut$trigger <- MagellanNTK::Timestamp()
      dataOut$value <- rv$dataIn 
    })
    
    # 
    # output$variable_btn_validate_ui <- renderUI({
    #   #browser()
    #   #req(length(rv.custom$funFilter()$value$ll.var) > 0)
    #   
    #   widget <- actionButton(ns("variable_btn_validate"),
    #     "Perform filtering", class = "btn-success")
    #   
    #   MagellanNTK::toggleWidget(widget, is.enabled())
    # })
    # # >>> END: Definition of the widgets
    # 
    # 
    # observeEvent(input$variable_btn_validate, {
    #  req(obj())
    #   tmp <- filterFeaturesOneSE(
    #     object = obj(),
    #     i = length(obj()),
    #     name = "variableFiltered",
    #     filters = rv.custom$funFilter()$value$ll.var
    #   )
    #   
    #   # Add infos
    #   #browser()
    #   nBefore <- nrow(tmp[[length(tmp) - 1]])
    #   nAfter <- nrow(tmp[[length(tmp)]])
    #   
    #   rv.custom$variable_Filter_SummaryDT[, "nbDeleted"] <- nBefore - nAfter
    #   rv.custom$variable_Filter_SummaryDT[, "TotalMainAssay"] <- nrow(assay(tmp[[length(tmp)]]))
    # 
    #   .history <- DaparToolshed::paramshistory(tmp[[i]])[['Filtering']][['Variable_Filtering']]
    #   
    #   ll.query <-  rv.custom$funFilter()$value$ll.widgets.value
    #   .history <- append(.history, ll.query)
    #   DaparToolshed::paramshistory(tmp[[i]])[['Filtering']][['Variable_Filtering']] <- .history
    #   
    #   
    #   rv$dataIn <- tmp
    #   
    #   dataOut$trigger <- MagellanNTK::Timestamp()
    #   dataOut$value <- tmp
    # })
    
    return(reactive({dataOut}))
  })
}



#' @export
#' @rdname mod_Variable_Filtering
#' 
mod_Variable_Filtering <- function(obj, i){
  ui <- mod_Variable_Filtering_ui('query')
  
  server <- function(input, output, session){
    
    res <- mod_Variable_Filtering_server('query',
      obj = reactive({obj}),
      i = reactive({i}))
    
    observeEvent(res()$trigger, {
      print(res()$value)
    })
  }
  
  app <- shiny::shinyApp(ui, server)
  
}