#' @title xxx
#' @description xxx
#' 
#' 
#' @param data xxx
#' @param title xxx
#' @param pal xxx
#' @param pattern xxx
#' @param ... xxx
#' 
#' @name mod_mv_plots
#' 
#' 
#' @examples
#' \dontrun{
#' library(DaparToolshed)
#' library(highcharter)
#' data(Exp1_R25_prot, package = 'DaparToolshedData')
#' pattern <- c("Missing POV")
#' grp <- design.qf(Exp1_R25_prot)$Condition
#' shiny::runApp(mod_mv_plots(obj[[4]], pattern = pattern, grp = grp))
#' }
#' 
NULL



#' @export
#' @rdname mod_mv_plots
#' 
mod_mv_plots_ui <- function(id) {
  ns <- NS(id)
  .style <- "display:inline-block; vertical-align: top; padding-right: 20px;"
  tagList(
    tags$div(
      tags$div( style = .style,
        highchartOutput(ns("plot_viewNAbyMean"), width = "600px")
      ),
      tags$div(style = .style,
        tagList(
          uiOutput(ns("WarnForImageNA")),
          imageOutput(ns("plot_showImageNA"), width = "600px")
        )
      )
    )
  )
}





#' @export
#' @rdname mod_mv_plots
#' 
mod_mv_plots_server <- function(id, 
  data = reactive({NULL}), 
  grp = reactive({NULL}),
  mytitle = reactive({NULL}), 
  pal = reactive({NULL}), 
  pattern = reactive({NULL}),
  is.enabled = reactive({TRUE}),
  remoteReset = reactive({0})
  ) {
  
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
   
    
    rv <- reactiveValues(
      grp = NULL,
      mytitle = NULL,
      palette = NULL,
      pattern = NULL
    )
    
    observeEvent(req(data()), {
      stopifnot(inherits(data(), 'SummarizedExperiment'))
      
      if(is.null(grp()))
        rv$grp <- paste0('grp_', seq(ncol(data())))
      else
        rv$grp <- grp()
      
      if(is.null(mytitle()))
        rv$mytitle <- 'my title'
      else
        rv$mytitle <- mytitle()
      
      if(is.null(palette()))
        rv$palette <- GetColorsForConditions(grp())
      else
        rv$palette <- palette()
      
      rv$pattern <- pattern()
    })
    
  output$plot_viewNAbyMean <- highcharter::renderHighchart({
    req(data())
    req(rv$pattern)
    req(rv$grp)

    hc_mvTypePlot2(
      obj = data(),
      group = rv$grp,
      pattern = rv$pattern,
      title = rv$mytitle,
      pal = rv$palette
    )
  })
  
  
  
  
  output$WarnForImageNA <- renderUI({
    req(!is.null(wrapper.mvImage(data(), rv$grp)))
    wellPanel(
      p(style = 'color: red;',
        "The 'MEC plot' cannot be showed as the dataset contains empty lines.")
    )
    # tryCatch(
    #     {
    #         wrapper.mvImage(data())
    #     },
    #     warning = function(w) {
    #         #p(conditionMessage(w))
    #         p('toto')
    #     },
    #     error = function(e) {
    #         #p(conditionMessage(e))
    #         p('toto')
    #     },
    #     finally = {
    #         # cleanup-code
    #     }
    # )
  })
  
  output$plot_showImageNA <- renderImage({
      # req(wrapper.mvImage(data()))
      
      # A temp file to save the output. It will be deleted after
      # renderImage
      # sends it, because deleteFile=TRUE.
      outfile <- tempfile(fileext = ".png")
      #browser()
      png(outfile)
      wrapper.mvImage(obj = data(), group = rv$grp)
      dev.off()
      
      # Return a list
      list(
        src = outfile,
        alt = "This is alternate text"
      )
    }, deleteFile = TRUE
  )
})

 } 



#' @export
#' @rdname mod_mv_plots
#' 
mod_mv_plots <- function(
    data, 
  grp = NULL,
  title = NULL,
  pal = NULL,
  pattern = NULL){
  
  ui <- fluidPage(
    mod_mv_plots_ui("mvImputationPlots_MV")
  )
    
    
  server <- function(input, output, session){
    
    #observe({
      res.imp <- mod_mv_plots_server("mvImputationPlots_MV",
      data = reactive({data}),
        grp = reactive({grp}),
        mytitle = reactive({title}),
      pal = reactive({pal}),
      pattern = reactive({pattern})
        )
    #})
  }
  
  app <- shiny::shinyApp(ui, server)
}

