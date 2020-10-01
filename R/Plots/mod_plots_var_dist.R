# Module UI

#' @title   mod_var_dist_plot_ui and mod_var_dist_plot_server
#' 
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
#' 
#' @export 
#' 
#' @importFrom shiny NS tagList 
#' 
mod_plots_var_dist_ui <- function(id){
  ns <- NS(id)
  tagList(
    helpText("Display the condition-wise distributions of the log-intensity CV (Coefficient of Variation) 
               of the protein/peptides."),
    helpText("For better visualization, it is possible to zoom in by click-and-drag."),
    #highchartOutput(ns("viewDistCV"),width = plotWidth, height = plotHeight) %>% shinycssloaders::withSpinner(type=spinnerType)
    highchartOutput(ns("viewDistCV"),width = 600, height = 600)
  )
}



# Module Server

#' @rdname mod_var_dist_plot
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @importFrom DAPAR2 CVDistD_HC
#' 
#' @importFrom SummarizedExperiment assay
#' 
mod_plots_var_dist_server <- function(id,
                                      obj,
                                      conds,
                                      base_palette=NULL){
  
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observe({
      req(obj())
      
      if (class(obj()) != "Summarizedexperiment") { return(NULL) }
    })
    
    
    
    viewDistCV <- reactive({
      req(obj())
      
      isolate({
        varDist <- DAPAR2::CVDistD_HC(SummarizedExperiment::assay(obj()),
                                      conds(),
                                      palette = base_palette())
      })
      varDist
    })
    
    
    output$viewDistCV <- renderHighchart({
      withProgress(message = 'Making plot', value = 100, {
        viewDistCV()
      })
    })
    
  })
  
  
}


## To be copied in the UI
# mod_plots_var_dist_ui("var_dist_plot_ui_1")

## To be copied in the server
# callModule(mod_plots_var_dist_server, "var_dist_plot_ui_1")

