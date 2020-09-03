
###############################################################################
##
## Module which create a little ui for the widgets of a function
## 
###############################################################################

mod_params_vsn_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('vsn_ui'))
  )
}


mod_params_vsn_server <- function(input, output, session, paramsIn){
  ns <- session$ns
  
  params <- reactiveValues(
    fun = 'vsn',
    args = list(
      type = "overall"
    )
  )
  
  
  observeEvent(paramsIn(), ignoreNULL = FALSE, {
    
    if (is.null(paramsIn()) || length(paramsIn())==0){
      params$fun = 'vsn'
      params$args$type = "overall"
    } else {
      params$args$type = paramsIn()$args$type
    }
  })
  
  
  
  observeEvent(input$normalization.type, ignoreInit=TRUE,{params$args$type <- input$normalization.type})
  
  
  output$loess_ui <- renderUI({
    
    tagList(
      selectInput(ns("normalization.type"), "Normalization type",  
                  choices = c("None", "overall", "within conditions"), 
                  selected = params$args$type)
    )
  })
  
  return(reactive({params}))
}

