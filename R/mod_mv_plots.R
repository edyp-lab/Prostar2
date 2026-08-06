#' @title xxx
#' @description xxx
#'
#'
#' @param id xxx
#' @param data xxx
#' @param mytitle xxx
#' @param pal xxx
#' @param pattern xxx
#' @param grp xxx
#' @param is.enabled xxx
#' @param remoteReset A `logical(1)` which acts as a remote command to reset
#' the module to its default values. Default is FALSE.
#'
#' @return NA
#'
#' @name mod_mv_plots
#'
#' @examples
#' if (interactive()){
#' library(DaparToolshed)
#' library(plotly)
#' data(Exp1_R25_prot, package = "DaparToolshedData")
#' pattern <- c("Missing POV")
#' grp <- DaparToolshed::design_qf(Exp1_R25_prot)$Condition
#' shiny::runApp(mod_mv_plots(obj[[4]], pattern = pattern, grp = grp))
#' }
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut format_DT_ui format_DT_server Timestamp toggleWidget
#'
NULL



#' @export
#' @rdname mod_mv_plots
#'
mod_mv_plots_ui <- function(id) {
  ns <- NS(id)
  .style <- "display:inline-block; vertical-align: top;"
  tagList(
    tags$div(
      style = .style,
      plotly::plotlyOutput(ns("plot_viewNAbyMean"), width = "600px")
    ),
    tags$div(
      style = .style,
      tagList(
        uiOutput(ns("WarnForImageNA")),
        imageOutput(ns("plot_showImageNA"), width = "600px")
      )
    )
  )
}





#' @export
#' @rdname mod_mv_plots
#'
mod_mv_plots_server <- function(
    id,
    data = reactive({
      NULL
    }),
    grp = reactive({
      NULL
    }),
    mytitle = reactive({
      NULL
    }),
    pal = NULL,
    pattern = reactive({
      NULL
    }),
    is.enabled = reactive({
      TRUE
    }),
    remoteReset = reactive({
      0
    })) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  #pkgs_require('grDevices')
    rv <- reactiveValues(
      grp = NULL,
      mytitle = NULL,
      palette = NULL,
      pattern = NULL
    )

    observeEvent(req(data()), {
      req(inherits(data(), "SummarizedExperiment"))

      if (is.null(grp())) {
        rv$grp <- paste0("grp_", seq(ncol(data())))
      } else {
        rv$grp <- grp()
      }

      if (is.null(mytitle())) {
        rv$mytitle <- "my title"
      } else {
        rv$mytitle <- mytitle()
      }

      #if (is.null(grDevices::palette())) {
      if (is.null(pal)) {
        rv$palette <- GetColorsForConditions(grp())
      } else {
        rv$palette <- pal #grDevices::palette()
      }

      rv$pattern <- pattern()
    })

    output$plot_viewNAbyMean <- plotly::renderPlotly({
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




    # output$WarnForImageNA <- renderUI({
    #   req(!is.null(wrapperMVImage(data(), rv$grp)))
    #   wellPanel(
    #     p(
    #       style = "color: red;",
    #       "The 'MEC plot' cannot be showed as the dataset contains empty lines."
    #     )
    #   )
    #   # tryCatch(
    #   #     {
    #   #         wrapperMVImage(data())
    #   #     },
    #   #     warning = function(w) {
    #   #         #p(conditionMessage(w))
    #   #         p('toto')
    #   #     },
    #   #     error = function(e) {
    #   #         #p(conditionMessage(e))
    #   #         p('toto')
    #   #     },
    #   #     finally = {
    #   #         # cleanup-code
    #   #     }
    #   # )
    # })

    output$plot_showImageNA <- renderImage({
      req(data())
      req(length(which(DaparToolshed::qMetacell(data()) == "Missing MEC")) != 0)
        pkgs_require('grDevices')
        # A temp file to save the output. It will be deleted after
        # renderImage
        # sends it, because deleteFile=TRUE.
        outfile <- tempfile(fileext = ".png")
        withProgress(message = "Making plot", value = 100, {
          grDevices::png(outfile)
          tmp <- wrapperMVImage(obj = data(), group = rv$grp)
          grDevices::dev.off()
        })
        tmp
        # Return a list
        list(
          src = outfile,
          alt = "This is alternate text"
        )
      },
      deleteFile = TRUE
    )
  })
}



#' @export
#' @rdname mod_mv_plots
#'
mod_mv_plots <- function(
    data,
    grp = NULL,
    mytitle = NULL,
    pal = NULL,
    pattern = NULL) {
  ui <- fluidPage(
    mod_mv_plots_ui("mvImputationPlots_MV")
  )


  server <- function(input, output, session) {
    # observe({
    res.imp <- mod_mv_plots_server("mvImputationPlots_MV",
      data = reactive({
        data
      }),
      grp = reactive({
        grp
      }),
      mytitle = reactive({
        mytitle
      }),
      pal = reactive({
        pal
      }),
      pattern = reactive({
        pattern
      })
    )
    # })
  }

  app <- shiny::shinyApp(ui, server)
}
