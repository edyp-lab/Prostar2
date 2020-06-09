# Module UI

#' @title   mod_plots_intensity_plots_ui and mod_plots_intensity_plots_server
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
#' @rdname mod_plots_intensity_plots
#'
#' @keywords internal
#' 
#' @export 
#' 
#' @importFrom shiny NS tagList 
#' 
#' @import shinyjs
#' 
mod_plots_intensity_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$div(
      tags$div(style="display:inline-block; vertical-align: middle;",
               highchartOutput(ns("BoxPlot")),
               shinyjs::hidden(imageOutput(ns("viewViolinPlot")))
      ),
      tags$div(style="display:inline-block; vertical-align: middle;",
               selectInput(ns("choosePlot"), "Choose plot", 
                           choices=c( "violinplot"="violinplot",
                                      "boxplot"="boxplot"), 
                           width='100px')
      ),
      
      uiOutput(ns('showTrackProt'))
      
    )  
  )
}

# Module Server

#' @rdname mod_plots_intensity_plots
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @importFrom DAPAR2 violinPlotD boxPlotD_HC 
#' 
#' @importFrom SummarizedExperiment rowData
#' 
mod_plots_intensity_server <- function(input, output, session,
                                       dataIn,
                                       meta,
                                       conds,
                                       params=NULL,
                                       reset=NULL,
                                       base_palette=NULL){
  
  
  ns <- session$ns
  
  rv.modboxplot <- reactiveValues(
    var = NULL,
    ind = NULL,
    indices = NULL
  )
  
  rv.modboxplot$var <- callModule(mod_plots_tracking_server, "widgets",
                                  obj = reactive({dataIn()}),
                                  params=reactive({params()}),
                                  keyId=reactive({meta()[['keyId']]}),
                                  reset=reactive({reset()}))
  
  
  output$showTrackProt <- renderUI({
    dataIn()
    
    if (S4Vectors::metadata(dataIn())[["typeOfData"]]=='protein'){
      tags$div(style="display:inline-block; vertical-align: middle;",
               mod_plots_tracking_ui(ns('widgets'))
      ) } else { return(NULL)}
  })
  
  
  
  observeEvent(req(rv.modboxplot$var()),{
    
    if (is.null(rv.modboxplot$var()$type)){
      return(NULL)
    }
    
    switch(rv.modboxplot$var()$type,
           ProteinList = rv.modboxplot$indices <- rv.modboxplot$var()$list.indices,
           Random = rv.modboxplot$indices <- rv.modboxplot$var()$rand.indices,
           Column = rv.modboxplot$indices <- rv.modboxplot$var()$col.indices
    )
    #if (length(rv.modboxplot$ind)==0){rv.modboxplot$ind <- NULL}
    if (length(rv.modboxplot$indices)==0){
      rv.modboxplot$indices <- NULL
    }
  })
  
  
  observeEvent(input$choosePlot, {
    shinyjs::toggle('viewViolinPlot', condition=input$choosePlot=='violinplot')
    shinyjs::toggle('BoxPlot', condition=input$choosePlot=='boxplot')
  })
  
  
  
  output$BoxPlot <- renderHighchart({
    dataIn()
    rv.modboxplot$indices
    tmp <- NULL
    
    pattern <- paste0('test',".boxplot")
    withProgress(message = 'Making plot', value = 100, {
      tmp <- DAPAR2::boxPlotD_HC(SummarizedExperiment::assay(dataIn()),
                                 conds=conds(),
                                 sequence=SummarizedExperiment::rowData(dataIn())[[ meta()[['keyId']] ]],
                                 palette=base_palette(),
                                 subset.view = rv.modboxplot$indices)
      #future(createPNGFromWidget(tmp,pattern))
    })
    tmp
  })
  
  
  output$viewViolinPlot<- renderImage({
    dataIn()
    rv.modboxplot$indices
    tmp <- NULL

    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    # Generate a png
    withProgress(message = 'Making plot', value = 100, {
      # png(outfile, width = 640, height = 480, units = "px")
      png(outfile)
      pattern <- paste0('test',".violinplot")
      tmp <- DAPAR2::violinPlotD(SummarizedExperiment::assay(dataIn()),
                                 keyId = SummarizedExperiment::rowData(dataIn())[[ meta()[['keyId']] ]],
                                 legend = conds(),
                                 palette = base_palette(),
                                 subset.view =  rv.modboxplot$indices)
      #future(createPNGFromWidget(tmp,pattern))
      dev.off()
    })
    tmp
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  return(reactive({rv.modboxplot$var()}))
}

## To be copied in the UI
# mod_plots_intensity_plots_ui("plots_intensity_plots_ui_1")

## To be copied in the server
# callModule(mod_plots_intensity_plots_server, "plots_intensity_plots_ui_1")

