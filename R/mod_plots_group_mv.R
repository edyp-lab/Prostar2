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
    
mod_plots_group_mv_server <- function(input, output, session, obj=NULL, settings=NULL){
  ns <- session$ns
  
  
  rv.group.mv <- reactiveValues(
    palette = NULL
  )
  
  if (is.null(obj) | class(obj) != "MSnSet") { return(NULL) }
  
  observeEvent(settings(), {
 
    if (is.null(settings()$examplePalette)){
      conds <- Biobase::pData(obj)$Conditions
      palette.init <- RColorBrewer::brewer.pal(8,"Dark2")[1:max(3,length(unique(conds)))]
      for (i in 1:length(conds)){
        rv.group.mv$palette[i] <- palette.init[which(qData[i] == unique(qData))]
      }
     } else {
        rv.group.mv$palette <- settings()$examplePalette
      }
  })
  
  qData <- Biobase::pData(obj)
  conds <- unique(qData$Condition)
  
  output$histo_MV <- renderHighchart({
    req(obj)
    tmp <- wrapper.mvHisto_HC(obj,palette=rv.group.mv$palette)
    tmp
  })
  
  
  
  output$histo_MV_per_lines <- renderHighchart({
    req(obj)
    
    isolate({
      tmp <- wrapper.mvPerLinesHisto_HC(obj, c(2:length(colnames(Biobase::pData(obj)))))
    })
    tmp
  })
  
  
  
  
  output$histo_MV_per_lines_per_conditions <- renderHighchart({
    req(obj)
   
    isolate({
      tmp <- wrapper.mvPerLinesHistoPerCondition_HC(obj, 
                                                    c(2:length(colnames(Biobase::pData(obj))))
                                                    ,RColorBrewer::brewer.pal(8,"Dark2"))
      })
    tmp
  })
  
}
    
## To be copied in the UI
# mod_plots_group_mv_ui("plots_group_mv_ui_1")
    
## To be copied in the server
# callModule(mod_plots_group_mv_server, "plots_group_mv_ui_1")
 
