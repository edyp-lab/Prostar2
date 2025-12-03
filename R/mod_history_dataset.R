#' @title   history_dataset_ui and history_dataset_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param dataIn An instance of the class `QFeatures`.
#'
#' @return A shiny app
#'
#'
#' @name history_dataset
#'
#' @examples
#' if (interactive()){
#' data(Exp1_R25_prot, package = "DaparToolshedData")
#' shiny::runApp(history_dataset(Exp1_R25_prot))
#' }
#' 
#' @importFrom QFeatures addAssay removeAssay
#' @import DaparToolshed
#' @importFrom MagellanNTK Get_Code_Declare_widgets Get_Code_for_ObserveEvent_widgets Get_Code_for_rv_reactiveValues Get_Code_Declare_rv_custom Get_Code_for_dataOut format_DT_ui format_DT_server Timestamp toggleWidget
NULL



#'
#'
#' @rdname history_dataset
#'
#' @export
#' @importFrom shiny NS tagList
#'
history_dataset_ui <- function(id) {
  ns <- NS(id)
  
  MagellanNTK::format_DT_ui(ns("history"))
}





# Module Server

#' @rdname history_dataset
#' @export
#'
#' @keywords internal
#'
#' @importFrom tibble as_tibble
#' @importFrom SummarizedExperiment rowData assay colData
#' @importFrom S4Vectors metadata
#' @importFrom MultiAssayExperiment experiments
#' @importFrom QFeatures nNA
#'
history_dataset_server <- function(
    id,
  dataIn = reactive({NULL}),
  remoteReset = reactive({0}),
  is.enabled = reactive({TRUE})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      dataIn = NULL
    )
    observeEvent(req(inherits(dataIn(), "QFeatures")), {
      rv$dataIn <- dataIn()
    })
    

    Get_QFeatures_History <- reactive({
      req(rv$dataIn)
      
      df <- data.frame(
        Name = "-",
        History = "-"
      )
      
      for (i in (names(rv$dataIn))){
        .se <- rv$dataIn[[i]]
        se_history <- "-"
      
        tmp <- data.frame(
          Name = "-",
          History = "-"
        )
        
      if (!is.null(DaparToolshed::paramshistory(.se))) {
        se_history <- lapply(DaparToolshed::paramshistory(.se), function(x) {
          ConvertListToHtml(paste0(names(x), " = ", x))
        })
        
        tmp <- data.frame(
          Name = names(se_history),
          History = unlist(se_history)
        )
      }
        
        df <- rbind(df, tmp)
      }
      
      
      df
    })
    
    
    
    MagellanNTK::format_DT_server("history",
      dataIn = reactive({Get_QFeatures_History()})
    )
    
  })
}



#' @export
#' @rdname history_dataset
#'
history_dataset <- function(obj) {
  ui <- fluidPage(history_dataset_ui("mod_info"))
  
  server <- function(input, output, session) {
    history_dataset_server("mod_info",
      dataIn = reactive({
        obj
      })
    )
  }
  
  app <- shiny::shinyApp(ui, server)
}
