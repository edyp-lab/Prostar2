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
#' library(SummarizedExperiment)
#' data(Exp1_R25_prot, package = "DaparToolshedData")
#' obj <- Exp1_R25_prot
#' conds <- SummarizedExperiment::colData(Exp1_R25_prot)$Condition
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
    shinyjs::useShinyjs(),
    uiOutput(ns("tree_UI")),
    uiOutput(ns("chooseKeepRemove_ui")),
    uiOutput(ns("chooseScope_ui")),
    uiOutput(ns("qMetacellScope_widgets_set2_ui")),
    uiOutput(ns("qMetacellScope_request_ui")),
    div(id = ns('div_buttons'),
      style = "display:inline-block; vertical-align: top;",
      uiOutput(ns('Preview_UI')),
    uiOutput(ns("Add_btn_UI"))
    )
  )
}




#' @rdname mod_qMetacell_FunctionFilter_Generator
#'
#' @export
#' @importFrom stats setNames
#'
mod_qMetacell_FunctionFilter_Generator_server <- function(
    id,
    dataIn = reactive({NULL}),
    conds,
    keep_vs_remove = reactive({setNames(nm = c("delete", "keep"))}),
    val_vs_percent = reactive({setNames(nm = c("Count", "Percentage"))}),
    operator = reactive({setNames(nm = DaparToolshed::SymFilteringOperators())}),
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
    tmp.tags = reactive({NULL}),
    showmodal = NULL
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


    
    output$tree_UI <- renderUI({
      widget <- div(id = ns('div_tree'),
        mod_metacell_tree_ui(ns("tree"))
        )
      MagellanNTK::toggleWidget(widget, is.enabled())
    })
    
    observeEvent(req(dataIn()), ignoreNULL = FALSE, {
      
      req(inherits(dataIn(), "SummarizedExperiment"))
        rv$dataIn <- dataIn()
        
        rv.custom$tmp.tags <- mod_metacell_tree_server("tree",
          dataIn = reactive({rv$dataIn}),
          remoteReset = reactive({remoteReset()}),
          is.enabled = reactive({is.enabled()})
        )
      },
      priority = 1000
    )


    observeEvent(rv.custom$tmp.tags()$trigger, ignoreInit = FALSE, {
      rv.widgets$tag <- rv.custom$tmp.tags()$values
      
      # Faire un reset des autres widgets
      rv.widgets[['keep_vs_remove']] <- widgets.default.values[['keep_vs_remove']]
      rv.widgets[['scope']] <- widgets.default.values[['scope']]
      rv.widgets[['valPercent']] <- widgets.default.values[['valPercent']]
      rv.widgets[['operator']] <- widgets.default.values[['operator']]
      rv.widgets[['valueTh']] <- widgets.default.values[['valueTh']]
      rv.widgets[['percentTh']] <- widgets.default.values[['percentTh']]
      
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
        title = "Threshold Type",
        content = "Define xxx"
      )

      widget1 <- radioButtons(ns("valPercent"),
        MagellanNTK::mod_popover_for_help_ui(ns("chooseValPercent_help")),
        choices = val_vs_percent(),
        selected = rv.widgets$valPercent
      )

      widget2 <- selectInput(ns("operator"),
        "Operator",
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
        title = "Threshold",
        content = "Define xxx"
      )

      widget <- selectInput(ns("valueTh"),
        MagellanNTK::mod_popover_for_help_ui(ns("value_th_help")),
        choices = getListNbValuesInLines(
          object = rv$dataIn,
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
        title = "Threshold",
        content = "Define xxx"
      )
      widget <- sliderInput(ns("percentTh"),
        MagellanNTK::mod_popover_for_help_ui(ns("percentTh_help")),
        min = 0, max = 100, step = 1,
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
      tags$p(paste("You are about to ", WriteQuery()),
        style = "font-size: small; text-align : center; color: purple;"
      )
    })


    # Set useless widgets to default values
    observeEvent(rv.widgets$scope == "WholeLine",
      {
        rv.widgets$percentThh <- 0
        rv.widgets$valueTh <- 0
        rv.widgets$valPercent <- "Percentage"
      }, priority = 1000)




    GetIndicesAndFunction <- function(){
      req(rv$dataIn)
      req(rv.widgets$tag != "None")
      req(rv.widgets$scope != "None")

      #rv.custom$indices <- NULL
      #rv.custom$ll.fun <- NULL

      th <- switch(rv.widgets$valPercent,
        Percentage = rv.widgets$percentTh / 100,
        Count = as.integer(rv.widgets$valueTh)
      )

      ff <- switch(rv.widgets$scope,
        WholeLine = {
          req(rv.widgets$keep_vs_remove)
          
          DaparToolshed::FunctionFilter("qMetacellWholeLine",
          cmd = rv.widgets$keep_vs_remove,
          pattern = rv.widgets$tag)
        }
        ,
        WholeMatrix = {
          req(rv.widgets$keep_vs_remove)
          req(rv.widgets$valPercent)
          req(rv.widgets$operator != "None")
          req(th)
          
          DaparToolshed::FunctionFilter("qMetacellWholeMatrix",
          cmd = rv.widgets$keep_vs_remove,
          pattern = rv.widgets$tag,
          percent = rv.widgets$valPercent,
          th = th,
          operator = rv.widgets$operator
        )
          },
        AllCond = {
          req(rv.widgets$keep_vs_remove)
          req(rv.widgets$operator != "None")
          req(th)
          req(rv.widgets$valPercent)
          req(conds())
          
          DaparToolshed::FunctionFilter("qMetacellOnConditions",
          cmd = rv.widgets$keep_vs_remove,
          mode = rv.widgets$scope,
          pattern = rv.widgets$tag,
          conds = conds(),
          percent = rv.widgets$valPercent,
          th = th,
          operator = rv.widgets$operator
        )
          },
        AtLeastOneCond = {
          req(rv.widgets$keep_vs_remove)
          req(rv.widgets$operator != "None")
          req(th)
          req(rv.widgets$valPercent)
          req(conds())
          
          DaparToolshed::FunctionFilter("qMetacellOnConditions",
          cmd = rv.widgets$keep_vs_remove,
          mode = rv.widgets$scope,
          pattern = rv.widgets$tag,
          conds = conds(),
          percent = rv.widgets$valPercent,
          th = th,
          operator = rv.widgets$operator
        )
        }
      )
      
      # store function filter
      rv.custom$ll.fun <- ff

      # Build a temporary QFeatures SE to compute indices (protect against small n)
      
      design.se <- data.frame(
        quantCols = colnames(rv$dataIn), 
        Condition = conds())
      rownames(design.se) <- colnames(rv$dataIn)
      
      tmp.se <- DaparToolshed::QFeaturesFromSE(
        obj.se = rv$dataIn, 
        colData = design.se, 
        name = 'myname')
      
      tmp <- DaparToolshed::filterFeaturesOneSE(
        object = tmp.se,
        i = length(tmp.se),
        name = paste0("qMetacellFiltered", MagellanNTK::Timestamp()),
        filters = list(rv.custom$ll.fun)
      )

      # choose "before" and "after" assays robustly
      assay_before <- SummarizedExperiment::assay(tmp[[1]])
      assay_after <- SummarizedExperiment::assay(tmp[[2]])
      
      names_before <- rownames(assay_before)
      names_after <- rownames(assay_after)

      #browser()
      indices <- 1:length(names_after)
      if (rv.custom$ll.fun@params$cmd == 'delete') {
        diff <- setdiff(names_before, names_after)
        indices <- match(diff, names_before)
        indices <- indices[!is.na(indices)]
      } else {
        inter <- intersect(names_before, names_after)
        indices <- match(inter, names_before)
        indices <- indices[!is.na(indices)]
      }
      
      
      rv.custom$indices <- indices
    }

    output$Preview_UI <- renderUI({
      tagList(
        mod_filtering_example_ui(ns("preview_filtering_query_result")),
        actionButton(ns("Preview_btn"), "Preview",
          class = "btn-info"
        )
      )
    })
    
    observeEvent(input$Preview_btn, ignoreInit = TRUE,{
      req(rv$dataIn)
      
      if (rv.widgets$tag == "None" || rv.widgets$scope == "None"){
        
        shiny::showModal(shiny::modalDialog(
          id = ns('preview_nofilter'),
          title = "No preview available",
          tagList(tags$head(tags$style(paste0(".modal-content:has(#", ns('preview_nofilter'), ") {width: 500px !important;}"))),
            "Please select a tag and scope to access the preview of the filtered results."
          ),
          easyClose = TRUE,
          size = "l",
          footer = tagList(
            modalButton("Close")
          )
        ))
      } else {
        GetIndicesAndFunction()
        
        if (length(rv.custom$indices) == 0){
          shiny::showModal(shiny::modalDialog(
            id = ns('preview_noindices'),
            title = "No preview available",
            tagList(tags$head(tags$style(paste0(".modal-content:has(#", ns('preview_noindices'), ") {width: 500px !important;}"))),
              "No lines are affected by this filter."
            ),
            easyClose = TRUE,
            size = "l",
            footer = tagList(
              modalButton("Close")
            )
          ))
        } else {
          rv.custom$showmodal <- MagellanNTK::Timestamp()
          req(rv.custom$indices)
          req(rv.custom$ll.fun)
          mod_filtering_example_server(id = "preview_filtering_query_result",
            dataIn = reactive({rv$dataIn}),
            indices = reactive({rv.custom$indices}),
            showModal = reactive({rv.custom$showmodal}),
            operation = reactive({list(rv.custom$ll.fun)[[1]]@params$cmd}),
            title = reactive({WriteQuery()}),
            remoteReset = reactive({remoteReset()})
          )
        }
      }
    })
   
    

    ## ---- Build-up reactive lists when function built --------------------
    observeEvent(c(rv.custom$ll.fun, WriteQuery(), reactiveValuesToList(rv.widgets)), {
      rv.custom$ll.query <- list(WriteQuery())
      rv.custom$ll.widgets.value <- list(reactiveValuesToList(rv.widgets))
    })

    observeEvent(input$BuildFilter_btn, ignoreInit = TRUE, {
      rv.custom$showmodal <- NULL
      GetIndicesAndFunction()
      req(rv.custom$ll.fun)
      req(rv.custom$ll.query)
      req(rv.custom$ll.widgets.value)
      
      # Append a new FunctionFilter to the list
      dataOut$trigger <- as.numeric(Sys.time())
      dataOut$value <- list(
        ll.fun = list(rv.custom$ll.fun),
        ll.query = rv.custom$ll.query,
        ll.widgets.value = rv.custom$ll.widgets.value,
        ll.pattern = rv.widgets$tag,
         ll.indices = list(rv.custom$indices)
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
    operator = setNames(nm = DaparToolshed::SymFilteringOperators()),
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
      dataIn = reactive({obj[[length(obj)]]}),
      conds = reactive({conds}),
      keep_vs_remove = reactive({keep_vs_remove}),
      val_vs_percent = reactive({val_vs_percent}),
      operator = reactive({operator}),
      is.enabled = reactive({is.enabled}),
      remoteReset = reactive({remoteReset() + input$Reset})
    )

    observeEvent(res()$trigger, {
      print(" --- res()$value ---")
      print(res()$value)
      print(" -------------------")
    })
  }

  app <- shiny::shinyApp(ui, server)
}
