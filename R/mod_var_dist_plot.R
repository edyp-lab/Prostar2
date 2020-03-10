# Module UI
  
#' @title   mod_var_dist_plot_ui and mod_var_dist_plot_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_var_dist_plot
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_var_dist_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    helpText("Display the condition-wise distributions of the log-intensity CV (Coefficient of Variation) 
               of the protein/peptides."),
    helpText("For better visualization, it is possible to zoom in by click-and-drag."),
    #highchartOutput(ns("viewDistCV"),width = plotWidth, height = plotHeight) %>% shinycssloaders::withSpinner(type=spinnerType)
    highchartOutput(ns("viewDistCV"),width = 600, height = 600) %>% shinycssloaders::withSpinner(type=8)
  )
}


    
# Module Server
    
#' @rdname mod_var_dist_plot
#' @export
#' @keywords internal
    
mod_var_dist_plot_server <- function(input, output, session, obj){
  ns <- session$ns
  
  examplePalette = RColorBrewer::brewer.pal(ncol(Biobase::exprs(obj)),"Dark2")
  
  varDist = NULL
  
  #------------------------------
  
  viewDistCV <- reactive({
    
    req(obj)
    examplePalette
    
    isolate({
      varDist <- wrapper.CVDistD_HC(obj,examplePalette)
    })
    varDist
  })
  
  
  output$viewDistCV <- renderHighchart({
    viewDistCV()
  })
  
  
  
}

    
## To be copied in the UI
# mod_var_dist_plot_ui("var_dist_plot_ui_1")
    
## To be copied in the server
# callModule(mod_var_dist_plot_server, "var_dist_plot_ui_1")
 
