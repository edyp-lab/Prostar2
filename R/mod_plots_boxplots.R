# Module UI
  
#' @title   mod_plots_boxplots_ui and mod_plots_boxplots_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_plots_boxplots
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_plots_boxplots_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      tags$div(style="display:inline-block; vertical-align: middle;",
               highchartOutput(ns("BoxPlot")),
               imageOutput(ns("viewViolinPlot"))
      ),
      tags$div(style="display:inline-block; vertical-align: middle;",
               selectInput(ns("choosePlot"), "Choose plot", choices=c( "violinplot"="violinplot","boxplot"="boxplot"), width='100px')
      ),
      
      uiOutput(ns('showTrackProt'))
      
    )  
  )
}
    
# Module Server
    
#' @rdname mod_plots_boxplots
#' @export
#' @keywords internal
#' @importFrom DAPAR 
    
mod_plots_boxplots_server <- function(input, output, session, dataIn, params, reset, base_palette){
  ns <- session$ns
  
  rv.modboxplot <- reactiveValues(
    var = NULL, #result of module tracking 
    ind = NULL,
    indices = NULL
  )
  
  
  rv.modboxplot$var <- callModule(mod_plots_tracking_server, "widgets", 
                                  obj = dataIn(),
                                  params=reactive({params()}), 
                                  reset=reactive({reset()}))
  
  
  output$showTrackProt <- renderUI({
    req(dataIn())
    if (typeOfData(dataIn()=='protein')){
      tags$div(style="display:inline-block; vertical-align: middle;",
               mod_plots_tracking_ui(ns('widgets'))
      ) } else { return(NULL)}
  })
  
  
  
  observeEvent(req(rv.modboxplot$var()),{
    print("In observe rv.modboxplot$var")
    print(rv.modboxplot$var())
    
    if (is.null(rv.modboxplot$var()$type)){
      return(NULL)
      }
    ll <- Biobase::fData(dataIn())[,parentProtId(dataIn())]
    
    
    switch(rv.modboxplot$var()$type,
           #ProteinList = rv.modboxplot$ind <- rv.modboxplot$var()$list,
           #Random = rv.modboxplot$ind <- rv.modboxplot$var()$rand,
           # Column = rv.modboxplot$ind <- rv.modboxplot$var()$col,
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
    # switch(input$choosePlot,
    #        boxplot={
    #          shinyjs::hide('viewViolinPlot')
    #          shinyjs::show('BoxPlot')
    #        },
    #        violinplot={
    #          shinyjs::hide('BoxPlot')
    #          shinyjs::show('viewViolinPlot')
    #        }
    # )
  })
  
  
  
  output$BoxPlot <- renderHighchart({
    dataIn()
    #rv$current.obj.name
    #rv$PlotParams$paletteConditions
    #rv$PlotParams$legendForSamples
    rv.modboxplot$indices
    tmp <- NULL
    isolate({
      
      ll <- Biobase::fData(dataIn())[,parentProtId(dataIn())]
      
      pattern <- paste0('test',".boxplot")
      conds <- Biobase::pData(dataIn())$Conditions
      tmp <- DAPAR::boxPlotD_HC(dataIn(), 
                                legend=conds, 
                                palette=base_palette,
                                subset.view = rv.modboxplot$indices)
      #future(createPNGFromWidget(tmp,pattern))
    })
    tmp
  })
  
  
  output$viewViolinPlot<- renderImage({
    dataIn()
    #rv$PlotParams$legendForSamples
    #rv$PlotParams$paletteConditions
    rv.modboxplot$indices
    tmp <- NULL
    isolate({
      
      # A temp file to save the output. It will be deleted after renderImage
      # sends it, because deleteFile=TRUE.
      outfile <- tempfile(fileext='.png')
      print("IN violinPlot")
      print(rv.modboxplot$indices)
      print("END IN violinplot")
      # Generate a png
      # png(outfile, width = 640, height = 480, units = "px")
      png(outfile)
      pattern <- paste0('test',".violinplot")
      conds <- Biobase::pData(dataIn())$Conditions
      tmp <- DAPAR::violinPlotD(dataIn(), 
                                legend = conds, 
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
# mod_plots_boxplots_ui("plots_boxplots_ui_1")
    
## To be copied in the server
# callModule(mod_plots_boxplots_server, "plots_boxplots_ui_1")
 
