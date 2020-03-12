# Module UI
  
#' @title   mod_choose_pipeline_ui and mod_choose_pipeline_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param pipeline.def xxx
#'
#' @rdname mod_choose_pipeline
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_choose_pipeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("selectWidgetPipeline"))
  )
}
    
# Module Server
    
#' @rdname mod_choose_pipeline
#' @export
#' @keywords internal
    
mod_choose_pipeline_server <- function(input, output, session, pipeline.def, dataType){
  ns <- session$ns
  
  rv.choosePipeline <- reactiveValues(
    choice = NULL )
  
  observeEvent( req(input$pipelineChoice), {
    rv.choosePipeline$choice <- pipeline.def[input$pipelineChoice]
  })
  
  output$selectWidgetPipeline<- renderUI({
    req(pipeline.def)
    selectizeInput(ns("pipelineChoice"),
                   "Choose the pipeline",
                   multiple = T,
                   options = list(maxItems = 1),
                   choices = names(pipeline.def))
  })
  
  return( reactive({rv.choosePipeline$choice }))
}
    
## To be copied in the UI
# mod_choose_pipeline_ui("choose_pipeline_ui_1")
    
## To be copied in the server
# callModule(mod_choose_pipeline_server, "choose_pipeline_ui_1")
 
