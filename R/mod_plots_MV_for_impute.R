#' plots_MV_for_impute UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plots_MV_for_impute_ui <- function(id){
  ns <- NS(id)
  tagList( 
    tags$div(
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                highchartOutput(ns("plot_viewNAbyMean"), width='600px')),
      tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                uiOutput(ns("WarnForImageNA")),
                imageOutput(ns("plot_showImageNA"), width='600px'))
    )
  )
}
    
#' plots_MV_for_impute Server Function
#'
#' @noRd 
mod_plots_MV_for_impute_server <- function(input, output, session, obj, ind, title, palette) {
  ns <- session$ns
  
  output$plot_viewNAbyMean <- renderHighchart({
    req(obj(), ind())
    hc_mvTypePlot2(obj=obj()[[ind()]], title=title(), palette = palette())
  })
  
  
  
  
  output$WarnForImageNA <- renderUI({
    
    tryCatch(
      {
        wrapper.mvImage(obj()[[ind()]])
      },
      warning = function(w) { p(conditionMessage(w))},
      error = function(e) {p(conditionMessage(e))},
      finally = {
        #cleanup-code 
      })
    
  })
  
  output$plot_showImageNA <- renderImage({
    #req(wrapper.mvImage(data()))
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    
    png(outfile)
    wrapper.mvImage(obj()[[ind()]])
    dev.off()
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
}

    
## To be copied in the UI
# mod_plots_MV_for_impute_ui("plots_MV_for_impute_ui_1")
    
## To be copied in the server
# callModule(mod_plots_MV_for_impute_server, "plots_MV_for_impute_ui_1")
 
