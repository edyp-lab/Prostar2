#' @title   mod_plots_density_ui and mod_plots_density_server
#' 
#' @description  A shiny Module.
#'
#' @param id shiny id
#' 
#' @param input internal
#' 
#' @param output internal
#' 
#' @param session internal
#'
#' @rdname mod_plots_density
#'
#' @keywords internal
#' 
#' @export 
#' 
#' @importFrom shiny NS tagList 
#' 
mod_plots_density_ui <- function(id){
  ns <- NS(id)
  tagList(
    highchartOutput(ns("Densityplot"))
  )
}

#' @rdname mod_plots_density
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @importFrom DAPAR2 densityPlotD_HC
#' @importFrom SummarizedExperiment assay
#' 
mod_plots_density_server <- function(id, obj, conds, legend=NULL, base_palette=NULL){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observe({
      req(obj())
      if (class(obj()) != "SummarizedExperiment") { return(NULL) }
    })
    
    
    output$Densityplot <- renderHighchart({
      req(obj())
      req(conds())
      
      tmp <- NULL
      isolate({
        
        withProgress(message = 'Making plot', value = 100, {
          tmp <- DAPAR2::densityPlotD_HC(qData = assay(obj()),
                                         conds = conds(),
                                         legend = legend(),
                                         palette = base_palette())
        })
      })
      tmp
    })
    
    
  })
  
  
}

## To be copied in the UI
# mod_plots_density_ui("plots_density_ui_1")

## To be copied in the server
# callModule(mod_plots_density_server, "plots_density_ui_1")

