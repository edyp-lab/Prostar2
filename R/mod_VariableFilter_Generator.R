#' @title Build Variable Filter function
#'
#' @description
#' This function is a shiny module to create a list of queries (instances of 
#' the class `VariableFilter` to filter the quantitative metadata of an instance
#' of the class `SummarizedExperiment`).
#' This function is written with specifications of the package `MagellanNTK` so
#' as to be easily integrated into workflfow compliant with `MagellanNTK`.
#'
#' @name mod_VariableFilter_Generator
#'
#' @return As for all modules used with `MagellanNTK`, the return value is a
#' `list()` of two items:
#' - trigger : xxx
#' - value: In this case, it contains a list() of three slots:
#'   - ll.var: a list() of instances of the class `VariableFilter`,
#'   - ll.query: a list of `character()` which describe the queries in natural
#'   language,
#'   - ll.widgets.value: a list of the values of widgets.
#'
#' @examples
#' \dontrun{
#' data(Exp1_R25_prot, package = 'DaparToolshedData')
#' obj <- Exp1_R25_prot[[1]]
#' operator = setNames(nm = SymFilteringOperators())
#' keep_vs_remove <- setNames(nm = c("delete", "keep"))
#' value = 3
#' shiny::runApp(mod_VariableFilter_Generator(obj, keep_vs_remove, value, operator))
#' }
#' 
#' @importFrom shinyFeedback useShinyFeedback showFeedbackWarning hideFeedback
NULL





#' @export
#'
#' @rdname mod_VariableFilter_Generator
#'
mod_VariableFilter_Generator_ui <- function(id) {
  ns <- NS(id)
  
  .style <- "display:inline-block; vertical-align: middle; padding: 7px;"
  wellPanel(
    useShinyFeedback(), # include shinyFeedback
    #DT::dataTableOutput(ns("VarFilter_DT")),
    # Build queries
    div(
      div(style = .style, uiOutput(ns("chooseKeepRemove_ui"))),
      div(style = .style, uiOutput(ns("cname_ui"))),
      div(style = .style, uiOutput(ns("operator_ui"))),
      div(style = .style, uiOutput(ns("value_ui")))
      ),
    uiOutput(ns("addFilter_btn_ui"))
    # Show example
    #uiOutput(ns("example_ui")),
    # Process the queries
    #uiOutput(ns("btn_validate_ui"))
  )
}




#' @param id xxx
#' @param obj An instance of the class `SummarizedExperiment`
#' @param keep_vs_remove xxx
#' @param operator xxx
#' @param remoteReset A `ìnteger(1)` xxxx
#' @param is.enabled A `logical(1)` that indicates whether the module is
#' enabled or disabled. This is a remote command.
#'
#' @rdname mod_VariableFilter_Generator
#'
#' @export
#'
mod_VariableFilter_Generator_server <- function(id,
  obj = reactive({NULL}),
  cname = reactive({NULL}),
  value = reactive({NULL}),
  operator = reactive({NULL}),
  keep_vs_remove = reactive({setNames(nm = c("delete", "keep"))}),
  remoteReset = reactive({0}),
  is.enabled = reactive({TRUE})) {
  
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list(
    cname = "None",
    value = NA,
    keep_vs_remove = "delete",
    operator = "None"
  )
  
  rv.custom.default.values <- list(
    ll.var = list(),
    ll.query = list(),
    ll.widgets.value = list()
  )
  

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # eval(
    #   str2expression(
    #     MagellanNTK::Get_AdditionalModule_Core_Code(
    #       w.names = names(widgets.default.values),
    #       rv.custom.names = names(rv.custom.default.values)
    #     )
    #   )
    # )
    
    #eval(str2expression(Get_Code_for_remoteReset()))
    
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
    
    
    
    # One need to rewrite a second observer for remoteReset because
    observeEvent(remoteReset(), ignoreInit = FALSE, ignoreNULL = TRUE, {
      updateTextInput(session, "value", 
        placeholder = 'Enter value...', 
        value = widgets.default.values[['value']])
    })


    
    observeEvent(req(obj()), ignoreNULL = FALSE, {
      stopifnot(inherits(obj(), 'SummarizedExperiment'))
      rv$dataIn <- obj()
    }, priority = 1000)
    
    
    
    # observeEvent(req(remoteReset() >= 1), ignoreInit = TRUE, ignoreNULL = FALSE, {
    #   lapply(names(rv.widgets), function(x){
    #     rv.widgets[[x]] <- widgets.default.values[[x]]
    #   })
    #   
    #   lapply(names(rv.custom), function(x){
    #     rv.custom[[x]] <- rv.custom.default.values[[x]]
    #   })
    # })
    # 
    
    output$chooseKeepRemove_ui <- renderUI({
 
      widget <- radioButtons(ns("keep_vs_remove"),
        "Type of filter operation",
        choices = keep_vs_remove(),
        selected = rv.widgets$keep_vs_remove
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    

    
    output$cname_ui <- renderUI({
      req(rv$dataIn)
      .choices <- c("None", colnames(rowData(rv$dataIn)))
      
      widget <- selectInput(ns("cname"),
        "Column name",
        choices = stats::setNames(.choices, nm = .choices),
        selected = rv.widgets$cname,
        width = "200px"
      )
      
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    
    output$operator_ui <- renderUI({
      req(rv$dataIn)
      req(rv.widgets$cname %in% colnames(rowData(rv$dataIn)))

      
      if (is.numeric(rowData(rv$dataIn)[, rv.widgets$cname])) {
        .operator <- DaparToolshed::SymFilteringOperators()
      } else {
        .operator <- c("==", "!=", "startsWith", "endsWith", "contains")
      }
      
      .operator = c("None" = "None", .operator)
      print(.operator)
      widget <- selectInput(ns("operator"),
        "operator",
        choices = stats::setNames(nm = .operator),
        selected = rv.widgets$operator,
        width = "100px"
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    

    output$value_ui <- renderUI({
      
      widget <- textInput(ns("value"),
        "value",
        placeholder = 'Enter value...',
        width = "100px"
        #value = rv.widgets$value
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    

    output$addFilter_btn_ui <- renderUI({
      widget <- actionButton(ns("addFilter_btn"), "Add filter",
        class = "btn-info")
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    

    observeEvent(c(rv.widgets$value, rv.widgets$cname), {
      req(rv$dataIn)
      req(rv.widgets$value != 'Enter value...')
      req(rv.widgets$cname != "None")
      
      if (is.na(Extract_Value(rv.widgets$value))) {
        showFeedbackWarning(
          inputId = "value",
          text = "wrong type of value"
        )  
      } else {
        hideFeedback("value")
      }
      
    })
    
    
    
    Extract_Value <- function(value){
      val <- NULL
      val <- tryCatch({
        
        
        if (is.numeric(rowData(rv$dataIn)[, rv.widgets$cname]) ) {
          as.numeric(value)
        } else if (!is.numeric(rowData(rv$dataIn)[, rv.widgets$cname])){
          as.character(value)
        }
        },
        warning = function(w){NA},
        error = function(e) NA
)
      return(val)
    }
    
    
    
    BuildVariableFilter <- reactive({
      req(obj())

      req(rv.widgets$value != 'Enter value...')
      req(rv.widgets$operator != "None")
      req(rv.widgets$cname != "None")
      req(Extract_Value(rv.widgets$value))

        QFeatures::VariableFilter(
          field = rv.widgets$cname,
          value = Extract_Value(rv.widgets$value),
          condition = rv.widgets$operator,
          not = rv.widgets$keep_vs_remove == "delete"
        )
    })
    
    
    WriteQuery <- reactive({

      
      value <- Extract_Value(rv.widgets$value)
      query <- paste0(
        rv.widgets$keep_vs_remove, " values for which ",
        rv.widgets$cname, " ", rv.widgets$operator, " ", value)
      query
      
    })
    
    observeEvent(input$addFilter_btn, ignoreInit = FALSE, ignoreNULL = TRUE, {
      rv.widgets$value
      rv.widgets$operator
      rv.widgets$cname
      
      req(BuildVariableFilter())
      req(WriteQuery())
      
      rv.custom$ll.var <- list(BuildVariableFilter())
      rv.custom$ll.query <- list(WriteQuery())
      rv.custom$ll.widgets.value <- list(reactiveValuesToList(rv.widgets))
      
      
      print("OUtput of mod_VariableFilter_Generator")
      # Append a new FunctionFilter to the list
      dataOut$trigger <- as.numeric(Sys.time())
      dataOut$value <- list(
        ll.var = rv.custom$ll.var,
        ll.query = rv.custom$ll.query,
        ll.widgets.value = rv.custom$ll.widgets.value
      )
      
      print(dataOut$trigger)
      print(dataOut$value)
      
    })
    
    return(reactive({dataOut}))
  })
}



#' @export
#' @rdname mod_VariableFilter_Generator
#' 
mod_VariableFilter_Generator <- function(
    obj,
  cname = NULL,
  value = NULL,
  operator = NULL,
  keep_vs_remove = 'delete',
  remoteReset = reactive({0})){
  
  
  ui <- tagList(
    actionButton("Reset", "Simulate reset"),
    mod_VariableFilter_Generator_ui('query')
  )
  
  server <- function(input, output, session){
    
    res <- mod_VariableFilter_Generator_server('query',
      obj = reactive({obj}),
      cname = reactive({cname}),
      value = reactive({value}),
      operator = reactive({operator}),
      keep_vs_remove = reactive({keep_vs_remove}),
      remoteReset = reactive({remoteReset() + input$Reset})
      )
    
    observeEvent(res()$trigger, {
      print(res()$value)
    })
  }
  
  app <- shiny::shinyApp(ui, server)
  
}