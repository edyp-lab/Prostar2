#' @title Build queries for filtering quantitative metadata
#'
#' @description
#'
#' This function is a shiny module to create a list of queries (instances of the
#' class `FunctionFilter` to filter the quantitative metadata of an instance
#' of the class `SummarizedExperiment`)
#'
#' @name mod_qMetacell_FunctionFilter_Generator
#' 
#' 
#' @param id xxx
#' @param dataIn An instance of the class `SummarizedExperiment`
#' @param obj xxx
#' @param conds A `character()` which contains the name of the conditions. The
#' length of this vector must be equal to the number of samples in the assay
#' (i.e. number of columns in assay(obj))
#' @param keep_vs_remove xxx
#' @param val_vs_percent xxx
#' @param operator xxx
#' @param remoteReset A `ìnteger(1)` xxxx
#' @param is.enabled A `logical(1)` that indicates whether the module is
#' enabled or disabled. This is a remote command.
#'
#'
#' @return As for all modules used with `MagellanNTK`, the return value is a
#' `list()` of two items:
#' - trigger : xxx
#' - value: In this case, it contains a list() of three slots:
#'   - ll.fun: a list() of instances of the class `FunctionFilter`,
#'   - ll.query: a list of `character()` which describe the queries in natural
#'   language,
#'   - ll.widgets.value: a list of the values of widgets.
#'
#' @examples
#' if (interactive()){
#' library(DaparToolshed)
#' library(shinyBS)
#' library(SummarizedExperiment)
#' data(Exp1_R25_prot, package = "DaparToolshedData")
#' obj <- Exp1_R25_prot
#' conds <- colData(Exp1_R25_prot)$Condition
#'
#' shiny::runApp(mod_qMetacell_FunctionFilter_Generator(obj, conds))
#' shiny::runApp(mod_qMetacell_FunctionFilter_Generator(obj, conds, is.enabled = FALSE))
#' }
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut format_DT_ui format_DT_server Timestamp toggleWidget mod_popover_for_help_server mod_popover_for_help_ui
#'
NULL





#' @export
#'
#' @rdname mod_qMetacell_FunctionFilter_Generator
#'
mod_qMetacell_FunctionFilter_Generator_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("tree_UI")),
    uiOutput(ns("chooseKeepRemove_ui")),
    uiOutput(ns("chooseScope_ui")),
    uiOutput(ns("qMetacellScope_widgets_set2_ui")),
    uiOutput(ns("qMetacellScope_request_ui")),
    mod_filtering_example_ui(ns("preview_filtering_query_result")),
    uiOutput(ns("Add_btn_UI"))
  )
}




#' @rdname mod_qMetacell_FunctionFilter_Generator
#'
#' @export
#' @importFrom stats setNames
#' @importFrom DaparToolshed typeDataset
#'
mod_qMetacell_FunctionFilter_Generator_server <- function(
    id,
    dataIn,
    conds,
    keep_vs_remove = reactive({setNames(nm = c("delete", "keep"))}),
    val_vs_percent = reactive({setNames(nm = c("Count", "Percentage"))}),
    operator = reactive({setNames(nm = SymFilteringOperators())}),
    remoteReset = reactive({0}),
    is.enabled = reactive({TRUE})) {
  # Define default selected values for widgets
  # This is only for simple workflows
  widgets.default.values <- list(
    tag = "None",
    scope = "None",
    keep_vs_remove = "delete",
    valueTh = 0,
    percentTh = 0,
    valPercent = "Count",
    operator = "None"
  )

  rv.custom.default.values <- list(
    indices = NULL,
    functionFilter = NULL,
    query = list(),
    fun.list = list(),
    widgets.value = list(),
    tmp.tags = reactive({
      NULL
    })
  )


  GetFiltersScope <- function() {
    c(
      "Whole Line" = "WholeLine",
      "Whole matrix" = "WholeMatrix",
      "For every condition" = "AllCond",
      "At least one condition" = "AtLeastOneCond"
    )
  }


  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # eval(
    #     str2expression(
    #         MagellanNTK::Get_AdditionalModule_Core_Code(
    #             w.names = names(widgets.default.values),
    #             rv.custom.names = names(rv.custom.default.values)
    #         )
    #     )
    # )


    core <- paste0(
      MagellanNTK::Get_Code_Declare_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_ObserveEvent_widgets(names(widgets.default.values)),
      MagellanNTK::Get_Code_for_rv_reactiveValues(),
      MagellanNTK::Get_Code_Declare_rv_custom(names(rv.custom.default.values)),
      MagellanNTK::Get_Code_for_dataOut(),
      MagellanNTK::Get_Code_for_remoteReset(widgets = TRUE, custom = FALSE, dataIn = "dataIn()"),
      sep = "\n"
    )
    eval(str2expression(core))

    output$Add_btn_UI <- renderUI({
      widget <- actionButton(ns("BuildFilter_btn"), "Add filter",
        class = "btn-info"
      )

      MagellanNTK::toggleWidget(widget, is.enabled())
    })

    MagellanNTK::mod_popover_for_help_server("tag_help",
      title = "Nature of data to filter",
      content = "Define xxx"
    )


    help.txt1 <- "To filter the missing values, the choice of the lines to
        be kept is made by different options:
    <ul>
    <li><strong>None</strong>: No filtering, the quantitative data is left
    unchanged.</li>
    <li><strong>(Remove) Empty lines</strong>: All the lines with 100% of
    missing values are filtered out.</li>
    <li><strong>Whole Matrix</strong>: The lines (across all conditions)
    which contain less quantitative value than a user-defined threshold are
    kept;</li>
    <li><strong>For every condition</strong>: The lines for which each
    condition contain less quantitative value than a user-defined threshold
    are deleted;</li>
    <li><strong>At least one condition</strong>: The lines for which at least
    one condition contain less quantitative value than a user-defined
    threshold are deleted.</li>
    </ul>"

    MagellanNTK::mod_popover_for_help_server("filterScope_help",
      title = "Scope",
      content = HTML(help.txt1)
    )


    # observeEvent(remoteReset(),{
    #   #browser()
    #   dataOut <- list()
    #   rv$dataIn <- obj()
    # })

    observeEvent(dataIn(), ignoreNULL = FALSE, {
        req(inherits(dataIn(), "QFeatures"))
        rv$dataIn <- dataIn()
      },
      priority = 1000
    )


    output$tree_UI <- renderUI({
      widget <- mod_metacell_tree_ui(ns("tree"))
      MagellanNTK::toggleWidget(widget, is.enabled())
    })


    rv.custom$tmp.tags <- mod_metacell_tree_server("tree",
      dataIn = reactive({rv$dataIn[[length(rv$dataIn)]]}),
      remoteReset = reactive({remoteReset()}),
      is.enabled = reactive({is.enabled()})
    )

    observeEvent(rv.custom$tmp.tags()$trigger, ignoreInit = FALSE, {
      rv.widgets$tag <- rv.custom$tmp.tags()$values
      dataOut$trigger <- as.numeric(Sys.time())
      dataOut$value <- list(
        ll.fun = NULL,
        ll.query = NULL,
        ll.widgets.value = NULL,
        ll.pattern = rv.widgets$tag,
        ll.indices = NULL
      )
    })



    keep_vs_remove <- reactive({
      setNames(nm = c("delete", "keep"))
    })


    output$chooseKeepRemove_ui <- renderUI({
      req(rv.widgets$tag != "None")
      widget <- radioButtons(ns("keep_vs_remove"),
        "Type of filter operation",
        choices = keep_vs_remove(),
        selected = rv.widgets$keep_vs_remove
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })

    output$chooseScope_ui <- renderUI({
      req(rv.widgets$tag != "None")
      widget <- selectInput(ns("scope"),
        MagellanNTK::mod_popover_for_help_ui(ns("filterScope_help")),
        choices = c(
          "None" = "None",
          "Whole Line" = "WholeLine",
          "Whole matrix" = "WholeMatrix",
          "For every condition" = "AllCond",
          "At least one condition" = "AtLeastOneCond"
        ),
        selected = rv.widgets$scope,
        width = "200px"
      )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })




    output$qMetacellScope_widgets_set2_ui <- renderUI({
      req(!(rv.widgets$scope %in% c("None", "WholeLine")))
      req(rv.widgets$tag != "None")

      MagellanNTK::mod_popover_for_help_server("chooseValPercent_help",
        title = paste("#/% of values to ", rv.widgets$keep_vs_remove),
        content = "Define xxx"
      )

      widget1 <- radioButtons(ns("valPercent"),
        MagellanNTK::mod_popover_for_help_ui(ns("chooseValPercent_help")),
        choices = val_vs_percent(),
        selected = rv.widgets$valPercent
      )

      widget2 <- selectInput(ns("operator"),
        "Choose operator",
        choices = operator(),
        selected = rv.widgets$operator,
        width = "100px"
      )


      tagList(
        fluidRow(
          column(4, MagellanNTK::toggleWidget(widget1, is.enabled())),
          column(8,
            MagellanNTK::toggleWidget(widget2, is.enabled()),
            uiOutput(ns("value_ui")),
            uiOutput(ns("percentage_ui"))
          )
        )
      )
    })


    output$value_ui <- renderUI({
      req(rv.widgets$valPercent == "Count")
      req(!(rv.widgets$scope %in% c("None", "WholeLine")))
      MagellanNTK::mod_popover_for_help_server("value_th_help",
        title = "Count threshold",
        content = "Define xxx"
      )

      widget <- selectInput(ns("valueTh"),
        MagellanNTK::mod_popover_for_help_ui(ns("value_th_help")),
        choices = getListNbValuesInLines(
          object = rv$dataIn[[length(rv$dataIn)]],
          conds = conds(),
          type = rv.widgets$scope
        ),
        selected = rv.widgets$valueTh,
        width = "150px"
      )
      tagList(
        MagellanNTK::mod_popover_for_help_ui(ns("keepVal_help")),
        MagellanNTK::toggleWidget(widget, is.enabled())
      )
    })

    output$percentage_ui <- renderUI({
      req(rv.widgets$valPercent == "Percentage")
      req(!(rv.widgets$scope %in% c("None", "WholeLine")))

      MagellanNTK::mod_popover_for_help_server("percentTh_help",
        title = "Percentage threshold",
        content = "Define xxx"
      )
      widget <- sliderInput(ns("percentTh"),
        MagellanNTK::mod_popover_for_help_ui(ns("percentTh_help")),
        min = 0,
        max = 100,
        step = 1,
        value = rv.widgets$percentTh,
        width = "250px"
      )
      tagList(
        MagellanNTK::mod_popover_for_help_ui(ns("keepVal_percent_help")),
        MagellanNTK::toggleWidget(widget, is.enabled())
      )
    })


    ## -------------------------------------------------------------------
    ##
    ## This function xxx
    ##
    ## -------------------------------------------------------------------
    WriteQuery <- reactive({
      txt_summary <- NULL
      if (rv.widgets$scope == "None") {
        txt_summary <- "No filtering is processed."
      } else if (rv.widgets$scope == "WholeLine") {
        txt_summary <- paste(
          rv.widgets$keep_vs_remove,
          "lines that contain only (",
          toString(rv.widgets$tag), ")"
        )
      } else {
        text_method <- switch(rv.widgets$scope,
          WholeMatrix = "the whole matrix.",
          AllCond = "each condition.",
          AtLeastOneCond = "at least one condition."
        )

        if (rv.widgets$valPercent == "Count") {
          text_threshold <- rv.widgets$valueTh
        } else {
          text_threshold <- paste(as.character(rv.widgets$percentTh),
            " %",
            sep = ""
          )
        }

        txt_summary <- paste(
          rv.widgets$keep_vs_remove,
          " lines where number of (",
          toString(rv.widgets$tag),
          ") data ",
          rv.widgets$operator,
          " ",
          text_threshold,
          " in ",
          text_method
        )
      }
      txt_summary
    })


    output$qMetacellFilter_request_ui <- renderUI({
      tags$p(paste("You are going to ", WriteQuery()),
        style = "font-size: small; text-align : center; color: purple;"
      )
    })


    # Set useless widgets to default values
    observeEvent(rv.widgets$scope == "WholeLine",
      {
        rv.widgets$percentThh <- 0
        rv.widgets$valueTh <- 0
        rv.widgets$valPercent <- "Percentage"
      },
      priority = 1000
    )




    BuildFunctionFilter <- reactive({
      req(rv$dataIn)
      req(rv.widgets$tag != "None")
      req(rv.widgets$scope != "None")

      th <- switch(rv.widgets$valPercent,
        Percentage = rv.widgets$percentTh / 100,
        Count = as.integer(rv.widgets$valueTh)
      )

      ff <- switch(rv.widgets$scope,
        WholeLine = FunctionFilter("qMetacellWholeLine",
          cmd = rv.widgets$keep_vs_remove,
          pattern = rv.widgets$tag
        ),
        WholeMatrix = FunctionFilter("qMetacellWholeMatrix",
          cmd = rv.widgets$keep_vs_remove,
          pattern = rv.widgets$tag,
          percent = rv.widgets$valPercent,
          th = th,
          operator = rv.widgets$operator
        ),
        AllCond = FunctionFilter("qMetacellOnConditions",
          cmd = rv.widgets$keep_vs_remove,
          mode = rv.widgets$scope,
          pattern = rv.widgets$tag,
          conds = conds(),
          percent = rv.widgets$valPercent,
          th = th,
          operator = rv.widgets$operator
        ),
        AtLeastOneCond = FunctionFilter("qMetacellOnConditions",
          cmd = rv.widgets$keep_vs_remove,
          mode = rv.widgets$scope,
          pattern = rv.widgets$tag,
          conds = conds(),
          percent = rv.widgets$valPercent,
          th = th,
          operator = rv.widgets$operator
        )
      )
      ff
    })


    GuessIndices <- reactive({
      req(rv.custom$ll.fun)
      
  
      tmp <- filterFeaturesOneSE(
        object = rv$dataIn,
        i = length(rv$dataIn),
        name = paste0("qMetacellFiltered", MagellanNTK::Timestamp()),
        filters = rv.custom$ll.fun
      )
      
      assaybefore <- assay(tmp[[length(tmp)-1]])
      assayafter <- assay(tmp[[length(tmp)]])
      namesbefore <- rownames(assaybefore)
      namesafter <- rownames(assayafter)
      
     
      indices <- 1:length(namesafter)
      if (rv.custom$ll.fun[[1]]@params$cmd == 'delete'){
        diff <- setdiff(namesbefore, namesafter)
        indices <- match(diff, namesbefore)
      } else {
        inter <- intersect(namesbefore, namesafter)
        indices <- match(inter, namesbefore)
      }
      
indices
    })

    
    
    observe({
      req(GuessIndices())
      req(BuildFunctionFilter())
      
       mod_filtering_example_server(id = "preview_filtering_query_result",
        dataIn = reactive({rv$dataIn[[length(rv$dataIn)]]}),
        indices = reactive({GuessIndices()}),
        operation = reactive({list(BuildFunctionFilter())[[1]]@params$cmd}),
        title = reactive({WriteQuery()})
      )
      
      
    })
    
    observeEvent(c(BuildFunctionFilter(), WriteQuery(), reactiveValuesToList(rv.widgets)), {
      rv.custom$ll.fun <- list(BuildFunctionFilter())
      rv.custom$ll.query <- list(WriteQuery())
      rv.custom$ll.widgets.value <- list(reactiveValuesToList(rv.widgets))
    })
    

    observeEvent(input$BuildFilter_btn, ignoreInit = TRUE, {
      req(rv.custom$ll.fun)
      req(rv.custom$ll.query)
      req(rv.custom$ll.widgets.value)
      
      
      # Append a new FunctionFilter to the list
      dataOut$trigger <- as.numeric(Sys.time())
      dataOut$value <- list(
        ll.fun = rv.custom$ll.fun,
        ll.query = rv.custom$ll.query,
        ll.widgets.value = rv.custom$ll.widgets.value,
        ll.pattern = rv.widgets$tag
        # ll.indices = GetIndices_FunFiltering(
        #   obj = rv$dataIn[[length(rv$dataIn)]],
        #   conds = conds(),
        #   level = DaparToolshed::typeDataset(rv$dataIn[[length(rv$dataIn)]]),
        #   pattern = rv.custom$ll.fun[[1]]@params$pattern,
        #   type = rv.widgets$scope,
        #   percent = rv.custom$ll.fun[[1]]@params$percent,
        #   op = rv.custom$ll.fun[[1]]@params$operator,
        #   th = rv.custom$ll.fun[[1]]@params$th
        # )
      )
    })

    return(reactive({dataOut}))
  })
}



#' @export
#' @rdname mod_qMetacell_FunctionFilter_Generator
#'
mod_qMetacell_FunctionFilter_Generator <- function(
    obj,
    conds,
    keep_vs_remove = setNames(nm = c("delete", "keep")),
    val_vs_percent = setNames(nm = c("Count", "Percentage")),
    operator = setNames(nm = SymFilteringOperators()),
    remoteReset = reactive({
      0
    }),
    is.enabled = TRUE) {
  ui <-
    tagList(
      actionButton("Reset", "Simulate reset"),
      mod_qMetacell_FunctionFilter_Generator_ui("query")
    )

  server <- function(input, output, session) {
    res <- mod_qMetacell_FunctionFilter_Generator_server("query",
      dataIn = reactive({
        obj
      }),
      conds = reactive({
        conds
      }),
      keep_vs_remove = reactive({
        keep_vs_remove
      }),
      val_vs_percent = reactive({
        val_vs_percent
      }),
      operator = reactive({
        operator
      }),
      is.enabled = reactive({
        is.enabled
      }),
      remoteReset = reactive({
        remoteReset() + input$Reset
      })
    )

    observeEvent(res()$trigger, {
      print(" --- res()$value ---")
      print(res()$value)
      print(" -------------------")
    })
  }

  app <- shiny::shinyApp(ui, server)
}
