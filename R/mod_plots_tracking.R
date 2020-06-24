#' @title   mod_plots_boxplots_ui and mod_plots_boxplots_server
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
#' @rdname mod_plots_tracking
#'
#' @keywords internal
#' 
#' @export 
#' 
#' @importFrom shiny NS tagList 
#' 
mod_plots_tracking_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
   # shinyjs::hidden(actionButton(ns('rst_btn'), 'Reset')),
   selectInput(ns("typeSelect"), "Type of selection", 
                choices=c("None"="None",
                          "Protein list"="ProteinList", 
                          "Random"="Random", 
                          "Specific column"="Column"),
                width=('130px')),
    shinyjs::hidden(uiOutput(ns("listSelect_UI"))),
    shinyjs::hidden(uiOutput(ns("randomSelect_UI"))),
    shinyjs::hidden(uiOutput(ns("columnSelect_UI")))
  )
}

#' plots_tracking Server Function
#'
#' @param obj Object SummarizedExperiment
#' 
#' @param metadata Metadata of Features containing the SummarizedExperiment
#'
#' @rdname mod_plots_tracking
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @import shinyjs
#' 
mod_plots_tracking_server <- function(input, output, session,
                                      obj,
                                      params,
                                      keyId,
                                      reset=FALSE, 
                                      slave = FALSE){
  ns <- session$ns
  
  rv.track <- reactiveValues(
    res = list(typeSelect = "None",
               listSelect = NULL,
               randSelect = '',
               colSelect = NULL,
               list.indices = NULL,
               rand.indices = NULL,
               col.indices = NULL),
    params = NULL
  )
  
 
  observeEvent(req(obj()),{
    if (class(obj()) != "SummarizedExperiment") { return(NULL) }
  })
  
  
  
  
  output$listSelect_UI <- renderUI({
    selectInput(ns("listSelect"), 
                "Protein for normalization", 
                choices = SummarizedExperiment::rowData(obj())[[keyId()]], 
                selected = rv.track$res$listSelect,
                multiple = TRUE, 
                width='400px'
    )
  })
  
  
  output$randomSelect_UI <- renderUI({
    textInput(ns("randSelect"), 
              "Random", 
              value = rv.track$res$randSelect, 
              width = ('120px'))
  })
  
  output$columnSelect_UI <- renderUI({
    selectInput(ns("colSelect"), 
                "Column", 
                choices = colnames(SummarizedExperiment::rowData(obj())),
                selected = rv.track$res$colSelect)
  })
  
  
  
  
  observe({
    
   reset()
    if (reset() > 0) {
      updateSelectInput(session, "typeSelect", selected='None')
      updateSelectInput(session, "listSelect", NULL)
      updateSelectInput(session, "randSelect", selected='')
      updateSelectInput(session, "colSelect", selected=NULL)
      rv.track$res <-list(typeSelect = "None",
                          listSelect = NULL,
                          randSelect = '',
                          colSelect = NULL,
                          list.indices = NULL,
                          rand.indices = NULL,
                          col.indices = NULL) 
    }
  })
  
  
  
  observe({
    params()
    
    print('params() changed')
    if (is.null(params())){
      updateSelectInput(session, "typeSelect", selected='None')
      updateSelectInput(session, "listSelect", NULL)
      updateSelectInput(session, "randSelect", selected='')
      updateSelectInput(session, "colSelect", selected=NULL)
      rv.track$res <-list(typeSelect = "None",
                          listSelect = NULL,
                          randSelect = '',
                          colSelect = NULL,
                          list.indices = NULL,
                          rand.indices = NULL,
                          col.indices = NULL)
      
    } else {
      rv.track$res$typeSelect <- params()$type
      
      rv.track$res$listSelect <- params()$listSelect
      rv.track$res$randSelect <- params()$randSelect
      rv.track$res$colSelect <- params()$colSelect
      
      rv.track$res$list.indices <- params()$list.indices
      rv.track$res$rand.indices <- params()$rand.indices
      rv.track$res$col.indices <- params()$col.indices
    }
  })
  
  
  
  observe({
    slave()
    if (slave() == FALSE) {
      shinyjs::show("typeSelect")
      } else {
      updateSelectInput(session, "typeSelect", selected='None')
      updateSelectInput(session, "listSelect", NULL)
      updateSelectInput(session, "randSelect", selected='')
      updateSelectInput(session, "colSelect", selected=NULL)
    
      shinyjs::hide("typeSelect")
      shinyjs::hide("listSelect_UI")
      shinyjs::hide("randomSelect_UI")
      shinyjs::hide("columnSelect_UI")
      }
    
  })
  
  
  
  
  
  observeEvent(input$typeSelect,{
    if (!is.null(params())) return(NULL)
    rv.track$res$typeSelect <- input$typeSelect
    
    updateSelectInput(session, "listSelect", NULL)
    updateSelectInput(session, "randSelect", selected='')
    updateSelectInput(session, "colSelect", selected=NULL)
    
    shinyjs::toggle("listSelect_UI", condition = input$typeSelect=="ProteinList")
    shinyjs::toggle("randomSelect_UI", condition = input$typeSelect=="Random")
    shinyjs::toggle("columnSelect_UI", condition = input$typeSelect=="Column")
  })
  
  
  
  
  
  observeEvent(input$listSelect,{
    if (!is.null(params())) return(NULL)
    rv.track$res$listSelect <- input$listSelect
    
    updateSelectInput(session, "randSelect", selected='')
    updateSelectInput(session, "colSelect", selected=NULL)
    
    if(is.null(rv.track$res$listSelect))
      rv.track$res$list.indices <- NULL
    else
      rv.track$res$list.indices <-  match(rv.track$res$listSelect, SummarizedExperiment::rowData(obj())[[keyId()]])
  })
  
  
  
  
  
  observeEvent(input$randSelect,{
    if (!is.null(params())) return(NULL)
    rv.track$res$randSelect <- input$randSelect
    
    updateSelectInput(session, "listSelect", NULL)
    updateSelectInput(session, "colSelect", selected=NULL)
    
    if (is.null(rv.track$res$randSelect) || rv.track$res$randSelect==''
        || (as.numeric(rv.track$res$randSelect) < 0))
    {
      rv.track$res$rand.indices <- NULL
    } else { 
        rv.track$res$rand.indices <- sample(1:nrow(obj()), as.numeric(rv.track$res$randSelect), replace=FALSE)
    }
  })
  
  observeEvent(input$colSelect,{
    if (!is.null(params())) return(NULL)
    rv.track$res$colSelect <- input$colSelect
    
    updateSelectInput(session, "listSelect", NULL)
    updateSelectInput(session, "randSelect", selected='')
    
    if (is.null(rv.track$res$colSelect))
      rv.track$res$col.indices <- NULL
    else
      rv.track$res$col.indices <- which(SummarizedExperiment::rowData(obj())[[rv.track$res$colSelect]] == 1)
    
    
  })
  
 
 
  
 
 
  
  return(reactive({rv.track$res}))
}

## To be copied in the UI
# mod_plots_tracking_ui("plots_tracking_ui_1")

## To be copied in the server
# callModule(mod_plots_tracking_server, "plots_tracking_ui_1")

