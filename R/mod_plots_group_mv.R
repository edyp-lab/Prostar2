# Module UI
  
#' @title   mod_plots_group_mv_ui and mod_plots_group_mv_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_plots_group_mv
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_plots_group_mv_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 4, highchartOutput(ns("histo_MV")), height="600px"),
      column(width = 4, highchartOutput(ns("histo_MV_per_lines"))),
      column(width = 4, highchartOutput(ns("histo_MV_per_lines_per_conditions")))
    )
  )
}
    
# Module Server
    
#' @rdname mod_plots_group_mv
#' @export
#' @keywords internal
#' @importFrom DAPAR wrapper.mvPerLinesHistoPerCondition_HC wrapper.mvHisto_HC wrapper.mvPerLinesHistoPerCondition_HC
    
mod_plots_group_mv_server <- function(input, output, session, obj=NULL, base_palette=NULL){
  ns <- session$ns
  
  
  # rv.group.mv <- reactiveValues(
  #   palette = NULL
  # )
  # 
  observe({
    req(obj())
  if (class(obj())[1] != "MSnSet") { return(NULL) }
  })
  
  # observeEvent(settings()$examplePalette, {
  #   rv.group.mv$palette <- settings()$examplePalette
  # })
  
  
  output$histo_MV <- renderHighchart({
    req(obj())
    base_palette()
    withProgress(message = 'Making plot', value = 100, {
      tmp <- DAPAR::wrapper.mvHisto_HC(obj(),palette=base_palette())
    })
    tmp
  })
  
  
  
  output$histo_MV_per_lines <- renderHighchart({
    req(obj())
    
    isolate({
      withProgress(message = 'Making plot', value = 100, {
        tmp <- DAPAR::wrapper.mvPerLinesHisto_HC(obj(), c(2:length(colnames(Biobase::pData(obj())))))
      })
    })
    tmp
  })
  
  
  
  
  output$histo_MV_per_lines_per_conditions <- renderHighchart({
    req(obj())
    base_palette()
    isolate({
      withProgress(message = 'Making plot', value = 100, {
        tmp <- DAPAR::wrapper.mvPerLinesHistoPerCondition_HC(obj(), 
                                                    c(2:length(colnames(Biobase::pData(obj()))))
                                                    ,base_palette())
      })
      })
    tmp
  })
  
}
    
## To be copied in the UI
# mod_plots_group_mv_ui("plots_group_mv_ui_1")
    
## To be copied in the server
# callModule(mod_plots_group_mv_server, "plots_group_mv_ui_1")
 
