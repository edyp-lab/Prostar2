# Module UI
  
#' @title   mod_choose_pipeline_ui and mod_choose_pipeline_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param dataType xxx
#' @param package xxx
#'
#' @rdname mod_choose_pipeline
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import sos
#' 
#' 
mod_choose_pipeline_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        selectInput(ns('dataType'), 'Data type', choices = c('None', 'protein', 'peptide'), width='150px')
        ),
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        uiOutput(ns("selectWidgetPipeline"))
      )
      ),
    uiOutput(ns('describePipeline'))
  )
}
    
# Module Server
    
#' @rdname mod_choose_pipeline
#' @export
#' @keywords internal
#' 
#' 
    
mod_choose_pipeline_server <- function(id, dataType = NULL, package = NULL){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$selectWidgetPipeline <- renderUI({
      library(package, character.only = TRUE)
      req(input$dataType != 'None')
      selectizeInput(ns("pipelineChoice"),
                     "Choose the pipeline",
                     multiple = T,
                     options = list(maxItems = 1),
                     choices = names(Pipelines()[grep(input$dataType, Pipelines())]), 
                     width='150px'
      )
    })
    
    output$describePipeline <- renderUI({
      req(input$pipelineChoice)
      includeMarkdown(system.file('md', paste0(input$pipelineChoice, '.md'), package=package))
    })
    
    return( reactive({input$pipelineChoice}))
    
    
  })
  
}
    
## To be copied in the UI
# mod_choose_pipeline_ui("choose_pipeline_ui_1")
    
## To be copied in the server
# callModule(mod_choose_pipeline_server, "choose_pipeline_ui_1")
 
