#' @title   mod_plots_boxplots_ui and mod_plots_boxplots_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_plots_tracking
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
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
#' @rdname mod_plots_tracking
#' @export
#' @keywords internal
#' @import shinyjs
mod_plots_tracking_server <- function(input, output, session, obj, params, reset=FALSE){
  ns <- session$ns
  
  
  observeEvent(req(obj()),{
    if (class(obj()) != "SummarizedExperiment") { return(NULL) }
  })
  
  
  observe({
    reset()
    if (reset() > 0) {
      updateSelectInput(session, "typeSelect", selected="None")
      updateSelectInput(session, "listSelect", NULL)
      updateSelectInput(session, "randSelect", selected="1")
      updateSelectInput(session, "colSelect", selected=NULL)
    }
  })
  
  observe({
    params()
    updateSelectInput(session, "typeSelect", selected=params()$type)
    updateSelectInput(session, "listSelect", selected=params()$list)
    updateSelectInput(session, "randSelect", selected=params()$rand)
    updateSelectInput(session, "colSelect", selected=params()$col)
  })
  
  
  
  observeEvent(input$typeSelect, ignoreInit = TRUE, {
    shinyjs::toggle("listSelect_UI", condition=input$typeSelect=="ProteinList")
    shinyjs::toggle("randomSelect_UI", condition=input$typeSelect=="Random")
    shinyjs::toggle("columnSelect_UI", condition=input$typeSelect=="Column")
  })
  
  output$listSelect_UI <- renderUI({
    #ll <-  Biobase::fData(obj())[,DAPAR::keyId(obj())]
    ll <-  SummarizedExperiment::rowData(obj())[["Protein_IDs"]]
    selectInput(ns("listSelect"), "Protein for normalization", choices=ll, multiple = TRUE, width='400px')
  })
  
  
  output$randomSelect_UI <- renderUI({
    textInput(ns("randSelect"), "Random", value="1", width=('120px'))
  })
  
  output$columnSelect_UI <- renderUI({
    ll <-  colnames(SummarizedExperiment::rowData(obj()))
    selectInput(ns("colSelect"), "Column", choices=ll)
  })
  
  
  
  BuildResult <- reactive({
    
    res <- list(type= input$typeSelect,
                list = input$listSelect,
                rand = as.numeric(input$randSelect),
                col = input$colSelect,
                list.indices = if (is.null(input$listSelect) || length(input$listSelect)==0){
                  NULL
                } else {
                  match(input$listSelect, SummarizedExperiment::rowData(obj())[['Protein_IDs']])
                },
                rand.indices = if (is.null(input$randSelect) || input$randSelect==""){
                  NULL
                } else {
                  sample(1:nrow(obj()), as.numeric(input$randSelect), replace=FALSE)
                },
                col.indices =  if (is.null(input$colSelect) || length(input$colSelect)==0){
                  NULL
                } else {
                  which(SummarizedExperiment::rowData(obj())[,input$colSelect] == 1)
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

