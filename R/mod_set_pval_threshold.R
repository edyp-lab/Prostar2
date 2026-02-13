#' @title Module set pval threshold
#' @description  A shiny Module.
#'
#' @param id A `charcater()` for the shiny module id
#' @param pval_init xxx
#' @param fdr xxx
#' @param remoteReset A `logical(1)` which acts as a remote command to reset
#' the module to its default values. Default is FALSE.
#' @param is.enabled xxx
#'
#'
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
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut format_DT_ui format_DT_server Timestamp toggleWidget mod_popover_for_help_server mod_popover_for_help_ui
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
    tags$style("#pvalPanel {width: 100%;}"),
    wellPanel(
      id = "pvalPanel",
      uiOutput(ns("warn_NULL_fdr_UI")),
      MagellanNTK::mod_popover_for_help_ui(ns("modulePopover_pValThreshold")),
      br(),
      tags$div(
        style = "align: center;display:inline-block; vertical-align: top;",
        tags$style(HTML("#pvalPanel .form-control {height: 20px; font-size: 13px;}")),
        tags$style(HTML("#pvalPanel .radio {padding-right: 0px; padding-bottom: 10px;}")),
        uiOutput(ns("thresholdType_UI"))
      ),
      tags$div(
        style = "align: center;display:inline-block; vertical-align: center;",
        uiOutput(ns("text_pval_UI")),
        uiOutput(ns("text_log_pval_UI"))
      ),
      tags$div(
        style = "align: center;display:inline-block; vertical-align: center;",
        uiOutput(ns("warn_text_pval_UI")),
        uiOutput(ns("warn_text_log_pval_UI"))
      ),
      br(),
      tags$div(
        style = "align: center;display:inline-block; vertical-align: center; ",
        uiOutput(ns('ApplyThreshold_UI'))
      )
      # tags$div(
      #   style = "align: center;display:inline-block; vertical-align: center; padding-left: 20px;",
      #   uiOutput(ns("showFDR_UI"))
      # )
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
       updateTextInput(session, 'text_pval', value = pval_init())
       updateTextInput(session, 'text_log_pval', value = -log10(pval_init()))

      
      
      lapply(names(rv.custom), function(x){
        rv.custom[[x]] <- rv.custom.default.values[[x]]
      })
        
        })

    
    dataOut <- reactiveVal()

    
    output$ApplyThreshold_UI <- renderUI({
      widget <- actionButton(ns("ApplyThreshold"), "Apply threshold", class = actionBtnClass)
      MagellanNTK::toggleWidget(widget, is.enabled())
    })

    
    output$thresholdType_UI <- renderUI({
      widget <- radioButtons(ns("thresholdType"), NULL,
        choices = c( "p-value" = "pval", "-log10(p-value)" = "logpval"),
        selected = rv.widgets$thresholdType
      )
      
      MagellanNTK::toggleWidget(widget, is.enabled())
    })

    output$warn_text_pval_UI <- renderUI({
      .value <-as.numeric(rv.widgets$text_pval)
      req(0 > .value || .value > 1)
      p(style = "color: red;", "Must be between 0 and 1.")

    })
    
    output$warn_text_log_pval_UI <- renderUI({
      req(0 > as.numeric(rv.widgets$text_log_pval))
      p(style = "color: red;", "Must be greater than 0.")
    })
    
    observeEvent(pval_init(), {
      rv.custom$text_log_pval <- -log10(pval_init())
      rv.custom$text_pval <- pval_init()
    })
    
    
    output$text_pval_UI <- renderUI({

      widget <- textInput(ns("text_pval"), NULL,
        value = rv.custom$text_pval,
        width = "100px"
      )
      
      MagellanNTK::toggleWidget(widget, is.enabled() && rv.widgets$thresholdType == "pval")
    })
    
    output$text_log_pval_UI <- renderUI({

      widget <- textInput(ns("text_log_pval"), NULL,
        value = rv.custom$text_log_pval,
        width = "100px"
      )
      
      MagellanNTK::toggleWidget(widget, is.enabled() && rv.widgets$thresholdType == "logpval")
    })

    
    observeEvent(rv.widgets$text_pval, ignoreInit = TRUE, {
      req(rv.widgets$thresholdType == "pval")
      updateTextInput(session, "text_log_pval", value = -log10(as.numeric(rv.widgets$text_pval)))
    })
    
    observeEvent(rv.widgets$text_log_pval, ignoreInit = TRUE, {
      req(rv.widgets$thresholdType == "logpval")
      updateTextInput(session, "text_pval", value = 10^(-as.numeric((rv.widgets$text_log_pval))))
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
    
    observe({print(rv$logpval())})
    
  }
  
  app <- shiny::shinyApp(ui, server)
}