# Module UI
  
#' @title   mod_plots_density_ui and mod_plots_density_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_plots_density
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_plots_density_ui <- function(id){
  ns <- NS(id)
  tagList(
    highchartOutput(ns("Densityplot"))
  )
}
    
# Module Server
    
#' @rdname mod_plots_density
#' @export
#' @keywords internal
    
mod_plots_density_server <- function(input, output, session, obj = NULL){
  ns <- session$ns
  
  output$Densityplot <- renderHighchart({
    req(obj)
    # settings <- rv.prostar$settings()
    # rv.prostar$settings()$examplePalette
    # rv.prostar$settings()$legendForSamples
    legend <- Biobase::pData(obj)[,"Condition"]
    palette <- RColorBrewer::brewer.pal(8,"Dark2")[1:length(unique(legend))]
    palette <- rep(palette, each=length(legend)/length(unique(legend)))
    
    
    print("IN : moduleDensityplot ")
    tmp <- NULL
    isolate({
      
      withProgress(message = 'Making plot', value = 100, {
        pattern <- paste0(obj@experimentData@name,".densityplot")
        tmp <- densityPlotD_HC(obj, 
                               # rv.prostar$settings()$legendForSamples,
                               # rv.prostar$settings()$examplePalette
                               legend = legend,
                               palette = palette)
        # future(createPNGFromWidget(rv$tempplot$boxplot,pattern))
      })
    })
    tmp
  })
  
}
    
## To be copied in the UI
# mod_plots_density_ui("plots_density_ui_1")
    
## To be copied in the server
# callModule(mod_plots_density_server, "plots_density_ui_1")
 
