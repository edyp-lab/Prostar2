
###############################################################################
##
## Module which create a little ui for the widgets of a function
## 
###############################################################################

mod_params_QuantileCentering_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('QuantileCentering_ui'))
  )
}


mod_params_QuantileCentering_server <- function(input, output, session, obj, paramsIn){
  ns <- session$ns
  
  params <- reactiveValues(
    fun = 'QuantileCentering',
    args = list(
      type = "overall",
      quantile = 0.15,
      subset.norm = NULL,
      conds = NULL
    )
  )
  
  
  observeEvent(paramsIn(), ignoreNULL = FALSE, {
    
    if (is.null(paramsIn()) || length(paramsIn())==0){
      params$fun = 'QuantileCentering'
      params$args$type = "overall"
      params$args$quantile = 0.15
      params$args$subset.norm = NULL
      params$args$conds = colData(obj())$Condition
    } else {
      params$args$type = paramsIn()$args$type
      params$args$quantile = as.numeric(paramsIn()$args$quantile)
      params$args$subset.norm = paramsIn()$args$subset.norm
      params$args$conds = colData(obj())$Condition
    }
  })
  
  
  
  observeEvent(input$norm.type, ignoreInit=TRUE,{params$args$type <- input$norm.type})
  
  
  output$QuantileCentering_ui <- renderUI({
    
    tagList(
      selectInput(ns("norm.type"), "Normalization type",  
                  choices = c("None", "overall", "within conditions"), 
                  selected = params$args$type),
      textInput(ns("normalization.quantile"), 'Quantile',
                value = params$args$quantile),
    )
  })
  
  
  
  return(reactive({params}))
}
