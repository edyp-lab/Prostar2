# Module UI
  
#' @title   mod_plots_missing_values_ui and mod_plots_missing_values_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param dataIn xxxx
#'
#' @rdname mod_plots_missing_values
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_plots_missing_values_ui <- function(id){
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
    
#' @rdname mod_plots_missing_values
#' @export
#' @keywords internal
    
mod_plots_missing_values_server <- function(input, output, session, dataIn){
  ns <- session$ns
  
  output$histo_MV <- renderHighchart({
    req(dataIn)
    #rv.prostar$settings()$examplePalette
    tmp <- NULL
    #isolate({
    #pattern <- paste0(GetCurrentObjName(),".MVplot1")
    tmp <- DAPAR::wrapper.mvHisto_HC(dataIn,palette=NULL)
    #future(createPNGFromWidget(tmp,pattern))
    #  })
    tmp
  })
  
  
  
  output$histo_MV_per_lines <- renderHighchart({
    req(dataIn)
    tmp <- NULL
    isolate({
      # pattern <- paste0(GetCurrentObjName(),".MVplot2")
      tmp <- 
        DAPAR::wrapper.mvPerLinesHisto_HC(dataIn, 
                                   c(2:length(colnames(Biobase::pData(dataIn)))))
      #future(createPNGFromWidget(tmp,pattern))
    })
    tmp
  })
  
  
  
  output$histo_MV_per_lines_per_conditions <- renderHighchart({
    req(dataIn)
    #rv.prostar$settings()$examplePalette
    tmp <- NULL
    isolate({
      # pattern <- paste0(GetCurrentObjName(),".MVplot2")
      tmp <- DAPAR::wrapper.mvPerLinesHistoPerCondition_HC(dataIn, 
                                                    c(2:length(colnames(Biobase::pData(dataIn))))
                                                    ,NULL)
      #future(createPNGFromWidget(tmp,pattern))
    })
    tmp
  })
}
    
## To be copied in the UI
# mod_plots_missing_values_ui("plots_missing_values_ui_1")
    
## To be copied in the server
# callModule(mod_plots_missing_values_server, "plots_missing_values_ui_1")
 
