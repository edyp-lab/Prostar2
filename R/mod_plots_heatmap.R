#' @title   mod_plots_heatmap_ui and mod_plots_heatmap_server
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
#' @rdname mod_plots_heatmap
#'
#' @keywords internal
#' 
#' @export 
#' 
#' @importFrom shiny NS tagList 
mod_plots_heatmap_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        selectInput(ns("distance"),"Distance",
                    choices = list("Euclidean" ="euclidean",
                                   "Manhattan"="manhattan",
                                   "Maximum" = "maximum",
                                   "Canberra" = "canberra",
                                   "Binary" = "binary",
                                   "Minkowski" = "minkowski"),
                    selected = "euclidean",
                    width="150px")
      ),
      div(
        style="display:inline-block; vertical-align: middle; padding-right: 20px;",
        selectInput(ns("linkage"),"Linkage",
                    choices=list("Complete" = "complete",
                                 "Average"="average",
                                 "Ward.D"="ward.D",
                                 "Ward.D2"="ward.D2",
                                 "Single" = "single",
                                 "Centroid" = "centroid",
                                 "Mcquitty" = "mcquitty",
                                 "Median" = "median"),
                    selected='complete',
                    width="150px")
      ),
      
      tags$hr(),
      uiOutput(ns("DS_PlotHeatmap"))
    )
  )
}


#' @rdname mod_plots_heatmap
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @importFrom DAPAR2 heatmapD
#' 
#' @importFrom SummarizedExperiment assay
mod_plots_heatmap_server <- function(input, output, session, 
                                     obj, 
                                     width = 900){
  ns <- session$ns
  
  observe({
    req(obj())
    if (class(obj()) != "SummarizedExperiment") { return(NULL) }
  })
  
  limitHeatmap <- 20000
  height <- paste0(2*width/3,"px")
  width <- paste0(width,"px")
  
  output$DS_PlotHeatmap <- renderUI({
    req(obj())
    if (nrow(assay(obj())) > limitHeatmap){
      tags$p("The dataset is too big to compute the heatmap in a reasonable time.")
    }else {
      tagList(
        plotOutput(ns("heatmap_ui"), width = width, height = height)
      )
    }
  })
  
  
  
  output$heatmap_ui <- renderPlot({
    heatmap()
  })
  
  
  
  heatmap <- reactive({
    
    req(obj())
    req(input$linkage)
    req(input$distance)
    
    isolate({ 
      withProgress(message = 'Making plot', value = 100, {
        DAPAR2::heatmapD(assay(obj()),
                         input$distance, 
                         input$linkage,
                         TRUE)
      })
    })
  })
}

## To be copied in the UI
# mod_plots_heatmap_ui("plots_heatmap_ui_1")

## To be copied in the server
# callModule(mod_plots_heatmap_server, "plots_heatmap_ui_1")

