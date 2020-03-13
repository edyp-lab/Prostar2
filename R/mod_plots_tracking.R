#' plots_tracking UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plots_tracking_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("typeSelect"), "Type of selection", 
                choices=c("None"="None","Protein list"="ProteinList", "Random"="Random", "Column"="Column"),
                width=('130px')),
    uiOutput(ns("listSelect_UI")),
    uiOutput(ns("randomSelect_UI")),
    uiOutput(ns("columnSelect_UI"))
  )
}
    
#' plots_tracking Server Function
#'
#' @noRd 
mod_plots_tracking_server <- function(input, output, session, obj, params, reset=FALSE){
  ns <- session$ns
  
  if (is.null(obj) | class(obj) != "MSnSet") { return(NULL) }
  
  # observe({
  #   reset()
  #   print("In track module =RESET observe")
  #   print(reset())
  #   if (reset() > 0) {
  #     updateSelectInput(session, "typeSelect", selected="None")
  #     updateSelectInput(session, "listSelect", NULL)
  #     updateSelectInput(session, "randSelect", selected="1")
  #     updateSelectInput(session, "colSelect", selected=NULL)
  #   }
  # })
  # 
  # observe({
  #   params()
  #   updateSelectInput(session, "typeSelect", selected=params()$type)
  #   updateSelectInput(session, "listSelect", selected=params()$list)
  #   updateSelectInput(session, "randSelect", selected=params()$rand)
  #   updateSelectInput(session, "colSelect", selected=params()$col)
  # })
  
  observeEvent(input$typeSelect, ignoreInit = TRUE, {
    shinyjs::toggle("listSelect", condition=input$typeSelect=="ProteinList")
    shinyjs::toggle("randSelect", condition=input$typeSelect=="Random")
    shinyjs::toggle("colSelect", condition=input$typeSelect=="Column")
  })
  
  output$listSelect_UI <- renderUI({
    isolate({
      ll <-  Biobase::fData(obj)[,obj@experimentData@other$proteinId]
      shinyjs::hidden(selectInput(ns("listSelect"), "Protein for normalization", choices=ll, multiple = TRUE, width='400px'))
    })
  })
  
  
  output$randomSelect_UI <- renderUI({
    isolate({
      ll <-  Biobase::fData(obj)[,obj@experimentData@other$proteinId]
      shinyjs::hidden(textInput(ns("randSelect"), "Random", value="1", width=('120px')))
    })
  })
  
  output$columnSelect_UI <- renderUI({
    isolate({
      ll <-  colnames(Biobase::fData(obj))
      shinyjs::hidden(selectInput(ns("colSelect"), "Column", choices=ll))
    })
  })
  
  
  BuildResult <- reactive({
    
    #isolate({
    ll <-  Biobase::fData(obj)[,obj@experimentData@other$proteinId]
    
    
    res <- list(type= input$typeSelect,
                list = input$listSelect,
                rand = as.numeric(input$randSelect),
                col = input$colSelect,
                list.indices = if (length(input$listSelect)==0){NULL} else match(input$listSelect, ll),
                rand.indices = if (length(input$randSelect)==0){NULL} else sample(1:length(ll), as.numeric(input$randSelect), replace=FALSE),
                col.indices =  if (length(input$colSelect)==0){NULL} else which(input$colSelect == 1)
    )
    
    # })
    print("res")
    res
  })
  
  return(reactive({BuildResult()}))
  
  
}
    
## To be copied in the UI
# mod_plots_tracking_ui("plots_tracking_ui_1")
    
## To be copied in the server
# callModule(mod_plots_tracking_server, "plots_tracking_ui_1")
 
