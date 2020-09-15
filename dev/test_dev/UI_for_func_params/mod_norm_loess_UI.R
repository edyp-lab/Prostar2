
###############################################################################
##
## Module which create a little ui for the widgets of a function
## 
###############################################################################

mod_params_LOESS_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('loess_ui'))
  )
}


mod_params_LOESS_server <- function(input, output, session, obj, paramsIn){
  ns <- session$ns
  
  params <- reactiveValues(
    fun = 'loess',
    args = list(
      span = 0.7,
      type = "overall",
      conds = NULL
    )
  )
  
  
  observeEvent(paramsIn(), ignoreNULL = FALSE, {
    
    if (is.null(paramsIn()) || length(paramsIn())==0){
      params$fun = 'loess'
      params$args$span=0.7
      params$args$type = "overall"
      params$args$conds = colData(obj())$Condition
    } else {
      params$args$span= paramsIn()$args$span
      params$args$type = paramsIn()$args$type
      params$args$conds = colData(obj())$Condition
    }
  })
  
  
  
  observeEvent(input$spanLOESS, ignoreInit=TRUE,{ params$args$span <- input$spanLOESS })
  observeEvent(input$normalization.type, ignoreInit=TRUE,{params$args$type <- input$normalization.type})
  
  
  output$loess_ui <- renderUI({
    
    tagList(
      selectInput(ns("normalization.type"), "Normalization type",  
                  choices = c("None", "overall", "within conditions"), 
                  selected = params$args$type),
      textInput(ns("spanLOESS"), "Span",value = params$args$span)
    )
  })
  
  
  
  return(reactive({params}))
}

