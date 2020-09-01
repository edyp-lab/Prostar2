# Module UI

#' @title   mod_plots_mv_ui and mod_plots_mv_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_plots_mv_for_imputation
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_plots_mv_for_imputation_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                highchartOutput(ns("plot_viewNAbyMean"), width='600px')),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                plotOutput(ns("plot_showImageNA"), width='600px'))
    )
  )
}

# Module Server

#' @rdname mod_plots_mv_for_imputation
#' @export
#' @keywords internal

mod_plots_mv_for_imputation_server <- function(input, output, session,
                                obj,
                                ind,
                                title=NULL,
                                palette=NULL){
  
  ns <- session$ns
  
  
  
  output$plot_viewNAbyMean <- renderHighchart({
    req(obj())
    
    withProgress(message = 'Making MV Intensity plot', value = 100, {
      DAPAR2::hc_mvTypePlot2(obj = obj(), 
                             i = ind(),
                             title = title(), 
                             palette = palette())
    })
  })
  
  
  output$plot_showImageNA <- renderPlot({
    req(obj())
    
      withProgress(message = 'Making MV Heatmap plot', value = 100, {
        DAPAR2::mvImage(obj(), ind())
      })

  })
  


}

## To be copied in the UI
# mod_plots_mv_ui("plots_mv_ui_1")

## To be copied in the server
# callModule(mod_plots_mv_server, "plots_mv_ui_1")

