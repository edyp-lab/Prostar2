#' @title   popover_for_help_ui and popover_for_help_server
#' @description  A shiny Module.
#'
#' @param id xxx
#' @param dataIn An instance of the class `SummarizedExperiment` containing
#' results of hypothesis testing stored as a data.frame in its metadata
#' @param thlogfc xxxx
#' @param thpval xxx
#' @param tooltip xxx
#' @param remoteReset xxx
#' @param is.enabled xxx
#' @param comparison xxx
#' @param group xxx
#'
#'
#' @name volcanoplot
#'
#' @examples
#' if (interactive()){
#' library(highcharter)
#' library(DaparToolshed)
#' library(SummarizedExperiment)
#' data(Exp1_R25_prot, package = "DaparToolshedData")
#' obj <- Exp1_R25_prot
#' # Simulate imputation of missing values
#' obj <- DaparToolshed::NAIsZero(obj, 1)
#' obj <- DaparToolshed::NAIsZero(obj, 2)
#' qData <- as.matrix(SummarizedExperiment::assay(obj[[2]]))
#' sTab <- colData(obj)
#' limma <- limmaCompleteTest(qData, sTab)
#'
#' df <- data.frame(
#'   x = limma$logFC[["25fmol_vs_10fmol_logFC"]],
#'   y = -log10(limma$P_Value[["25fmol_vs_10fmol_pval"]]),
#'   index = as.character(rownames(obj[[2]]))
#' )
#' colnames(df) <- c("x", "y", "index")
#' #tooltipSlot <- c("Fasta_headers", "Sequence_length")
#' #df <- cbind(df, colData(obj[[2]])[, tooltipSlot])
#' #colnames(df) <- gsub(".", "_", colnames(df), fixed = TRUE)
#' #if (ncol(df) > 3) {
#' #  colnames(df)[seq.int(from = 4, to = ncol(df))] <-
#' #    paste("tooltip_", colnames(df)[seq.int(from = 4, to = ncol(df))],
#' #      sep = ""
#' #    )
#' #}
#' hc_clickFunction <- JS("function(event) {
#' Shiny.onInputChange('eventPointClicked',[this.index]+'_'+ [this.series.name]);}")
#' cond <- c("25fmol", "10fmol")
#' diffAnaVolcanoplot_rCharts(
#'   df,
#'   th_pval = 2.5,
#'   th_logfc = 1,
#'   conditions = cond,
#'   clickFunction = hc_clickFunction
#' )
#' 
#' 
#' shiny::runApp(
#' volcanoplot(df,
#'     comparison = "25fmol_vs_10fmol",
#'     group,
#'     thlogfc = 2.5,
#'     thpval = 1))
#' }
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets 
#' Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut 
#' format_DT_ui format_DT_server Timestamp toggleWidget mod_popover_for_help_server mod_popover_for_help_ui
#'
NULL




#' @importFrom shiny NS tagList
#' @importFrom shinyjs inlineCSS useShinyjs
#' @importFrom DT DTOutput renderDT datatable formatStyle styleEqual
#' @importFrom highcharter highchartOutput
#' @export
#' @rdname volcanoplot
#'
mod_volcanoplot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    highcharter::highchartOutput(ns("volcanoPlot_UI"),
      width = "600px", height = "600px"
    ),
    uiOutput(ns("quantiDT"))
  )
}



#' @export
#' @rdname volcanoplot
#' @importFrom SummarizedExperiment rowData assay colData
#'
mod_volcanoplot_server <- function(
    id,
    dataIn = reactive({NULL}),
    comparison = reactive({NULL}),
    group = reactive({NULL}),
    thlogfc = reactive({0}),
    thpval = reactive({0}),
    tooltip = reactive({NULL}),
    remoteReset = reactive({0}),
    is.enabled = reactive({TRUE})) {
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list(
    DifferentialAnalysis_thpval = NULL,
    widget2 = NULL,
    widget3 = NULL
  )


  rv.custom.default.values <- list(
    colorsVolcanoplot = NULL,
    data = NULL,
    conditions = NULL,
    clickFun = NULL,
    eventPointClicked = NULL
  )
 
  
  MSG_WARNING_SIZE_DT <- "The size of the table is too big to be exported with
    the buttons below (only the first 154 rows will be exported). It is advised
    to use the Export tool of Prostar."

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pkgs.require(c('magrittr', "shinyjqui"))
    # DO NOT MODIFY THIS FUNCTION CALL
    eval(
      str2expression(
        MagellanNTK::Get_AdditionalModule_Core_Code(
          w.names = names(widgets.default.values),
          rv.custom.names = names(rv.custom.default.values)
        )
      )
    )


    observeEvent(req(dataIn()), {
      rv$dataIn <- dataIn()
      rv.custom$data <- HypothesisTest(dataIn())
    })

    
    observeEvent(input$eventPointClicked, {
      rv.custom$eventPointClicked <- input$eventPointClicked
    })
    
    observeEvent(remoteReset(), {
      
      #browser()
      rv.custom$clickFun <- NULL
    })

    output$quantiDT <- renderUI({
      req(rv.custom$eventPointClicked)
#browser()

      if (DaparToolshed::typeDataset(rv$dataIn) == "protein") {
        if (!('adjacencymatrix' %in% names(rowData(rv$dataIn)))) {
          tabsetPanel(id = ns("collapseVolcanoInfos"),
            shiny::tabPanel("Protein",
              tagList(
                uiOutput(ns("Warning_Infos")),
                DT::DTOutput(ns("Infos"))
              )
            )
          )
        } else {
          tabsetPanel(id = ns("collapseVolcanoInfos"),
           shiny::tabPanel("Protein",
              tagList(
                uiOutput(ns("Warning_Infos")),
                DT::DTOutput(ns("Infos"))
              )
             ),
            shiny::tabPanel("Specific peptides",
              tagList(
                uiOutput(ns("Warning_specificPeptidesInfos")),
                DT::DTOutput(ns("specificPeptidesInfos"))
              )), 
            shiny::tabPanel("Shared peptides",
              tagList(
                uiOutput(ns("Warning_sharedPeptidesInfos")),
                DT::DTOutput(ns("sharedPeptidesInfos"))
              )
            )
            )
        }
      } else if (omXplore::get_type(rv$dataIn) == "peptide") {
        tabsetPanel(id = ns("collapseVolcanoInfos"),
          shiny::tabPanel("Peptide",
            tagList(
              uiOutput(ns("Warning_Infos")),
              DT::DTOutput(ns("Infos"))
            )
          )
        )
      }
    })



    GetSortingIndices <- reactive({
      req(comparison())

      condition1 <- comparison()[1]
      condition2 <- comparison()[2]
      .ind <- which(group() == comparison()[1])

      if (length(grep("all", comparison()[2])) == 0) {
        ind <- c(
          which(group() == comparison()[1]),
          which(group() == comparison()[2])
        )
      } else {
        ind <- c(
          which(group() == comparison()[1]),
          c(seq(length(group())))[-.ind]
        )
      }
      ind
    })

    GetBorderIndices <- reactive({
      conds <- group()[GetSortingIndices()]
      ## build index for border-formatting
      borders_index <- unlist(lapply(
        unique(conds),
        function(x) {
          which.max(x == conds)
        }
      ))
      borders_index
    })



    output$Warning_sharedPeptidesInfos <- renderUI({
      GetDataFor_sharedPeptidesInfos()
      if (nrow(GetDataFor_sharedPeptidesInfos()) > 153) {
        p(MSG_WARNING_SIZE_DT)
      }
    })




    GetDataFor_sharedPeptidesInfos <- reactive({
      req(comparison())
      req(rv$dataIn)
      ind <- GetSortingIndices()
      borders_index <- GetBorderIndices()

      prot <- rv.custom$dataGetExprsClickedProtein
      prot.indice <- rownames(prot)

      data <- Build_enriched_qdata(rv$dataIn, 3)
      data <- data[, c(ind, (ind + ncol(data) / 2))]

      Xspec <- ExtractUniquePeptides(QFeatures::adjacencyMatrix(rv$dataIn))
      Xshared <- QFeatures::adjacencyMatrix(rv$dataIn)

      i <- which(colnames(Xspec) == prot.indice)
      specificPeptidesIndices <- which(Xspec[, i] == 1)
      allPeptidesIndices <- which(Xshared[, i] == 1)
      peptidesIndices <- setdiff(allPeptidesIndices, specificPeptidesIndices)
      data <- data[peptidesIndices, ]
      data
    })

    output$sharedPeptidesInfos <- DT::renderDT(server = TRUE, {
      req(rv$dataIn)
      data <- GetDataFor_sharedPeptidesInfos()
      req(data)
      .type <- DaparToolshed::typeDataset(rv$dataIn)
      c.tags <- names(BuildColorStyles(.type))
      c.colors <- unname(BuildColorStyles(.type))
      
      range.invisible <- c(((1 + (ncol(data)) / 2)):ncol(data))
      
      .colDef <- list(
        list(
          targets = range.invisible,
          visible = FALSE
        )
      )

      dt <- DT::datatable(
        data,
        extensions = c("Scroller"),
        options = list(
          initComplete = MagellanNTK::initComplete(),
          dom = "frtip",
          blengthChange = FALSE,
          displayLength = 20,
          ordering = FALSE,
          server = FALSE,
          columnDefs = .colDef
        )
      ) |>
        DT::formatStyle(
          colnames(data)[1:(ncol(data) / 2)],
          colnames(data)[range.invisible],
          backgroundColor = DT::styleEqual(c.tags, c.colors)
        ) |>
        DT::formatStyle(GetBorderIndices(), borderLeft = "3px solid #000000")

      dt
    })

    output$Warning_specificPeptidesInfos <- renderUI({
      GetDataFor_specificPeptidesInfos()
      if (nrow(GetDataFor_specificPeptidesInfos()) > 153) {
        p(MSG_WARNING_SIZE_DT)
      }
    })

    GetDataFor_specificPeptidesInfos <- reactive({
      req(rv$dataIn)
      ind <- GetSortingIndices()
      borders_index <- GetBorderIndices()
      # .ind <- last(grep(pattern = "peptide", names(rv$dataIn)))
      # prev.dataset <- rv$dataIn[[names(rv$dataIn)[.ind]]]

      prot <- rv.custom$dataGetExprsClickedProtein
      prot.indice <- rownames(prot)

      data <- Build_enriched_qdata(rv$dataIn, 3)
      data <- data[, c(ind, (ind + ncol(data) / 2))]


      Xspec <- ExtractUniquePeptides(QFeatures::adjacencyMatrix(rv$dataIn))

      i <- which(colnames(Xspec) == prot.indice)
      peptidesIndices <- which(Xspec[, i] == 1)
      data <- data[peptidesIndices, ]
      data
    })


    output$specificPeptidesInfos <- DT::renderDT(server = TRUE, {
      req(rv$dataIn)
      data <- GetDataFor_specificPeptidesInfos()
      
      
      .type <- DaparToolshed::typeDataset(rv$dataIn)
      c.tags <- names(BuildColorStyles(.type))
      c.colors <- unname(BuildColorStyles(.type))
      
      range.invisible <- c(((1 + (ncol(data)) / 2)):ncol(data))
      
      .colDef <- list(
        list(
          targets = range.invisible,
          visible = FALSE
        )
      )
      dt <- DT::datatable(
        data,
        extensions = c("Scroller"),
        options = list(
          initComplete = MagellanNTK::initComplete(),
          dom = "frtip",
          blengthChange = FALSE,
          displayLength = 20,
          ordering = FALSE,
          columnDefs = .colDef
        )
      ) |>
        DT::formatStyle(
          colnames(data)[1:(ncol(data) / 2)],
          colnames(data)[range.invisible],
          backgroundColor = DT::styleEqual(c.tags, c.colors)
        ) |>
        DT::formatStyle(GetBorderIndices(), borderLeft = "3px solid #000000")

      dt
    })


    ## ---------------------------------------------------------
    #GetExprsClickedProtein <- bindEvent({
    observeEvent(rv.custom$eventPointClicked, {
      req(rv$dataIn)
      req(rv.custom$eventPointClicked)

      rv.custom$dataGetExprsClickedProtein <- NULL
      ind <- GetSortingIndices()
   
      this.index <- as.integer(strsplit(rv.custom$eventPointClicked, "_")[[1]][1])
      this.series.name <- strsplit(rv.custom$eventPointClicked, "_")[[1]][2]

      .digits <- 3
      rv.custom$dataGetExprsClickedProtein <- Build_enriched_qdata(rv$dataIn, .digits)
      #data <- data[, c(ind, (ind + ncol(data) / 2))]

     
      nn_logFC <- paste0(comparison()[1], '_vs_', comparison()[2], '_logFC')
      nn_pval <- paste0(comparison()[1], '_vs_', comparison()[2], '_pval')
       .dat <- rowData(rv$dataIn)$HypothesisTest
      
      index.g1 <- which((-log10(.dat[, nn_pval]) >= thpval()
      ) & (abs(.dat[, nn_logFC]) >= as.numeric(thlogfc())))

      data.g1 <- rv.custom$dataGetExprsClickedProtein[index.g1, ]
      data.g2 <- rv.custom$dataGetExprsClickedProtein[-index.g1, ]

      switch(this.series.name,
        g1 = rv.custom$dataGetExprsClickedProtein <- data.g1[this.index + 1, ],
        g2 = rv.custom$dataGetExprsClickedProtein <- data.g2[this.index + 1, ]
      )
      #rv.custom$data
    })



    output$Warning_Infos <- renderUI({
      req(rv.custom$dataGetExprsClickedProtein)
      if (nrow(rv.custom$dataGetExprsClickedProtein) > 153) {
        p(MSG_WARNING_SIZE_DT)
      }
    })




    # GetDataFor_Infos <- reactive({
    #   #browser()
    #   req(rv.custom$dataGetExprsClickedProtein)
    #   data <- rv.custom$dataGetExprsClickedProtein
    #   data
    # })

    ## -------------------------------------------------------------
    output$Infos <- DT::renderDT(server = TRUE, {
      
      req(rv$dataIn)
req(rv.custom$dataGetExprsClickedProtein)
      borders_index <- GetBorderIndices()
      data <- rv.custom$dataGetExprsClickedProtein
      
      .type <- DaparToolshed::typeDataset(rv$dataIn)
      c.tags <- names(BuildColorStyles(.type))
      c.colors <- unname(BuildColorStyles(.type))

      range.invisible <- c(((1 + (ncol(data)) / 2)):ncol(data))
      
      .colDef <- list(
          list(
            targets = range.invisible,
            visible = FALSE
          )
        )
        
   
      dt <- DT::datatable(
        data,
        extensions = c("Scroller"),
        options = list(
          initComplete = MagellanNTK::initComplete(),
          dom = "frtip",
          blengthChange = FALSE,
          displayLength = 20,
          ordering = FALSE,
          header = FALSE,
          columnDefs = .colDef
        )
      ) |>
        DT::formatStyle(
          colnames(data)[1:(ncol(data) / 2)],
          colnames(data)[range.invisible],
          backgroundColor = DT::styleEqual(c.tags, c.colors)
        ) |>
        DT::formatStyle(borders_index, borderLeft = "3px solid #000000")
      dt
    })

    ## ---------------------------------------------------------------------
    output$volcanoPlot_UI <- highcharter::renderHighchart({
      req(rv$dataIn)
      req(comparison())

      withProgress(message = "Building plot...", detail = "", value = 0, {
        ht <- HypothesisTest(rv$dataIn)
        prefix <- paste0(comparison()[1], "_vs_", comparison()[2])

        df <- data.frame(
          x = ht[, paste0(prefix, "_logFC")],
          y = -log10(ht[, paste0(prefix, "_pval")]),
          index = seq(nrow(ht))
        )

        if (length(tooltip()) > 0 && sum(is.na(tooltip())) == 0) {
          df <- cbind(
            df,
            rowData(rv$dataIn)[tooltip()]
          )
        }

        colnames(df) <- gsub(".", "_", colnames(df), fixed = TRUE)
        if (ncol(df) > 3) {
          .range <- seq.int(from = 4, to = ncol(df), by = 1)
          colnames(df)[.range] <-
            paste("tooltip_", colnames(df)[.range], sep = "")
        }


        rv.custom$clickFun <-
          shinyjqui::JS(paste0(
            "function(event) {Shiny.onInputChange('",
            ns("eventPointClicked"),
            "', [this.index]+'_'+ [this.series.name]);}"
          ))

        diffAnaVolcanoplot_rCharts(
          df,
          th_logfc = as.numeric(thlogfc()),
          th_pval = as.numeric(thpval()),
          conditions = comparison(),
          clickFunction = rv.custom$clickFun,
          pal = rv.custom$colorsVolcanoplot
        )
      })
      # MagellanNTK::toggleWidget(widget, is.enabled())
    })
  })
}



#' @export
#' @rdname volcanoplot
#'
volcanoplot <- function(
    dataIn,
    comparison,
    group,
    thlogfc,
    thpval,
    tooltip = NULL) {
  ui <- fluidPage(
    mod_volcanoplot_ui("volcano")
  )

  server <- function(input, output) {
    mod_volcanoplot_server(
      id = "volcano",
      dataIn = reactive({dataIn}),
      comparison = reactive({comparison}),
      group = reactive({group}),
      thlogfc = reactive({thlogfc}),
      thpval = reactive({thpval}),
      tooltip = reactive({tooltip})
      # fdr = reactive({3.8})
    )
  }

  app <- shiny::shinyApp(ui, server)
}
