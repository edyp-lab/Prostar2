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
#' @param metadata Metadata of Fetaures containing the SummarizedExperiment
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
                                      reset=FALSE){
  ns <- session$ns
  
  rv.track <- reactiveValues(
    typeSelect = "None",
    listSelect = NULL,
    randSelect = "1",
    colSelect = NULL
  )
  
  
  observeEvent(req(obj()),{
    if (class(obj()) != "SummarizedExperiment") { return(NULL) }
  })
  
  
  observe({
    reset()
    if (reset() > 0) {
      updateSelectInput(session, "typeSelect", selected="None")
      updateSelectInput(session, "listSelect", NULL)
      updateSelectInput(session, "randSelect", selected=1)
      updateSelectInput(session, "colSelect", selected=NULL)
    }
  })
  
  observe({
    params()
    
    print('in params de slave')
    print(params())
    updateSelectInput(session, "typeSelect", selected=params()$type)
    updateSelectInput(session, "listSelect", selected=if(is.null(params()$list)) character(0) else params()$list)
    
    if (is.null(params()$rand))
      updateSelectInput(session, "randSelect", selected=character(0))
    else
      updateSelectInput(session, "randSelect", selected=params()$rand)
    
    updateSelectInput(session, "colSelect", selected=params()$col)
  })
  
  
  
  observeEvent(input$typeSelect, ignoreInit = TRUE, {
    
    shinyjs::toggle("listSelect_UI", condition=input$typeSelect=="ProteinList")
    shinyjs::toggle("randomSelect_UI", condition=input$typeSelect=="Random")
    shinyjs::toggle("columnSelect_UI", condition=input$typeSelect=="Column")
  })
  
  output$listSelect_UI <- renderUI({
    selectInput(ns("listSelect"), 
                "Protein for normalization", 
                choices=SummarizedExperiment::rowData(obj())[[keyId()]], 
                multiple = TRUE, 
                width='400px'
                )
  })
  
  
  output$randomSelect_UI <- renderUI({
    textInput(ns("randSelect"), 
              "Random", 
              value=1, 
              width=('120px'))
  })
  
  output$columnSelect_UI <- renderUI({
    selectInput(ns("colSelect"), 
                "Column", 
                choices=colnames(SummarizedExperiment::rowData(obj())))
  })
  
  
  
  BuildResult <- reactive({
    input$listSelect
    input$randSelect
    input$colSelect
    
    
    
    res <- list(type= input$typeSelect,
                list = input$listSelect,
                rand = if (is.null(input$randSelect) 
                           || input$randSelect=="" 
                           || input$randSelect=="-"
                           || (as.numeric(input$randSelect) <0))
                           character(0)
                       else 
                         as.numeric(input$randSelect),
                col = input$colSelect,
                list.indices = if (is.null(input$listSelect) 
                                   || length(input$listSelect)==0
                                   || input$listSelect==''){
                  NULL
                } else {
                  match(input$listSelect, SummarizedExperiment::rowData(obj())[[keyId()]])
                },
                rand.indices = if (is.null(input$randSelect) 
                                   || input$randSelect=="" 
                                   || input$randSelect=="-"
                                   || (as.numeric(input$randSelect) <0)){
                  NULL
                } else {
                  sample(1:nrow(obj()), as.numeric(input$randSelect), replace=FALSE)
                },
                col.indices =  if (is.null(input$colSelect) || length(input$colSelect)==0){
                  NULL
                } else {
                  which(SummarizedExperiment::rowData(obj())[[input$colSelect]] == 1)
                }
    )
    res
  })
  
  return(reactive({BuildResult()}))
}

## To be copied in the UI
# mod_plots_tracking_ui("plots_tracking_ui_1")

## To be copied in the server
# callModule(mod_plots_tracking_server, "plots_tracking_ui_1")

