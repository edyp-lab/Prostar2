#' @title Module set pval threshold
#' @description  A shiny Module.
#'
#' @param id A `charcater()` for the shiny module id
#' @param pval_init xxx
#' @param remoteReset A `logical(1)` which acts as a remote command to reset
#' the module to its default values. Default is FALSE.
#' @param is.enabled xxx
#'
#' @return NA
#'
#' @name mod_set_pval_threshold
#'
#' @examples
#' if (interactive()){
#' library(Prostar2)
#' shiny::runApp(mod_set_pval_threshold())
#' }
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut format_DT_ui format_DT_server Timestamp toggleWidget
#'
NULL




#'
#' @export
#' @importFrom shiny NS tagList
#' @importFrom shinyjs inlineCSS useShinyjs toggleState
#' @rdname mod_set_pval_threshold
#'
mod_set_pval_threshold_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("#pvalPanel {width: 100%;}
                     #pvalPanel .form-control {height: 20px; font-size: 13px;}
                     #pvalPanel .radio {margin-top: 7px;}")),
    wellPanel(
      id = "pvalPanel",
      div(style = "display: flex; margin-top: 10px; gap: 10px;",
        uiOutput(ns("thresholdType_UI")),
        div(style = "display: flex; flex-direction: column;",
          uiOutput(ns("text_pval_UI")),
          uiOutput(ns("text_log_pval_UI"))
        )
      ),
      uiOutput(ns('ApplyThreshold_UI'))
    )
  )
}

#' @rdname mod_set_pval_threshold
#' @importFrom shinyjs inlineCSS useShinyjs toggleState
#' @export
#'
mod_set_pval_threshold_server <- function(
    id,
  pval_init = reactive({1}),
  remoteReset = reactive({0}),
  is.enabled = reactive({TRUE})) {
  
  
  widgets.default.values <- list(
    thresholdType = "pval",
    text_pval = NULL,
    text_log_pval = NULL
  )
  
  rv.custom.default.values <- list(
    text_pval = 1,
    text_log_pval = 0,
    pval_init = 1
  )
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    core <- paste0(
      MagellanNTK::Get_Code_Declare_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_ObserveEvent_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_rv_reactiveValues(),
      MagellanNTK::Get_Code_Declare_rv_custom(names(rv.custom.default.values)),
      MagellanNTK::Get_Code_for_dataOut(),
      sep = "\n"
    )
    eval(str2expression(core))
    
    observeEvent(remoteReset(), ignoreInit = TRUE, ignoreNULL = TRUE, {
      lapply(names(rv.widgets), function(x){
        rv.widgets[[x]] <- widgets.default.values[[x]]})

      #rv.widgets$text <- pval_init()
      shinyWidgets::updateAutonumericInput(session, 'text_pval', value = pval_init())
      shinyWidgets::updateAutonumericInput(session, 'text_log_pval', value = -log10(pval_init()))
      
      lapply(names(rv.custom), function(x){
        rv.custom[[x]] <- rv.custom.default.values[[x]]
      })
        
        })

    dataOut <- reactiveVal()
    
    output$ApplyThreshold_UI <- renderUI({
      widget <- actionButton(ns("ApplyThreshold"), "Apply threshold")
      
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    output$thresholdType_UI <- renderUI({
      widget <- radioButtons(ns("thresholdType"), NULL,
        choices = c( "p-value" = "pval", "-log10(p-value)" = "logpval"),
        selected = rv.widgets$thresholdType
      )
      
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    observeEvent(pval_init(), {
      rv.custom$text_log_pval <- -log10(pval_init())
      rv.custom$text_pval <- pval_init()
    })
    
    output$text_pval_UI <- renderUI({
      widget <- shinyWidgets::autonumericInput(
        ns("text_pval"),
        label = NULL,
        value = rv.custom$text_pval, 
        width = "100px",
        minimumValue = 0,
        maximumValue = 1,
        decimalCharacter = ".",
        decimalPlaces = 3,
        modifyValueOnWheel = FALSE,
        align = "left"
      )
      
      MagellanNTK::toggleWidget(widget, is.enabled() && rv.widgets$thresholdType == "pval")
    })
    
    output$text_log_pval_UI <- renderUI({
      widget <- shinyWidgets::autonumericInput(
        ns("text_log_pval"),
        label = NULL,
        value = rv.custom$text_log_pval, 
        width = "100px",
        minimumValue = 0,
        decimalCharacter = ".",
        decimalPlaces = 3,
        modifyValueOnWheel = FALSE,
        align = "left"
      )
      
      MagellanNTK::toggleWidget(widget, is.enabled() && rv.widgets$thresholdType == "logpval")
    })

    observeEvent(rv.widgets$text_pval, ignoreInit = TRUE, {
      req(rv.widgets$thresholdType == "pval")
      shinyWidgets::updateAutonumericInput(session, "text_log_pval", value = -log10(rv.widgets$text_pval))
    })
    
    observeEvent(rv.widgets$text_log_pval, ignoreInit = TRUE, {
      req(rv.widgets$thresholdType == "logpval")
      shinyWidgets::updateAutonumericInput(session, "text_pval", value = 10^(-(rv.widgets$text_log_pval)))
    })
    
    observeEvent(input$ApplyThreshold, ignoreInit = FALSE, ignoreNULL = FALSE, {
      dataOut(as.numeric(rv.widgets$text_log_pval))
    })
    
    return(reactive({dataOut()}))
  })
}



#' @rdname mod_set_pval_threshold
#' @export
#'
mod_set_pval_threshold <- function(
    pval_init = 1) {
  ui <- fluidPage(
    actionButton("SimulateReset", "Reset"),
    mod_set_pval_threshold_ui("Title")
  )
  
  server <- function(input, output) {
    rv <- reactiveValues(
      logpval = reactive({
        NULL
      })
    )
    

    rv$logpval <- mod_set_pval_threshold_server(
      id = "Title",
      pval_init = reactive({pval_init}),
      remoteReset = reactive({input$SimulateReset})
    )
    
    observe({message(rv$logpval())})
    
  }
  
  app <- shiny::shinyApp(ui, server)
}