#' plots_tracking UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyjs useShinyjs
mod_plots_tracking_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
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
#' @noRd 
mod_plots_tracking_server <- function(input, output, session, obj, params, reset=FALSE){
  ns <- session$ns
  
  r <- reactiveValues(
    indices =NULL
  )
  
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
    shinyjs::toggle("listSelect_UI", condition=input$typeSelect=="ProteinList")
    shinyjs::toggle("randomSelect_UI", condition=input$typeSelect=="Random")
    shinyjs::toggle("columnSelect_UI", condition=input$typeSelect=="Column")
  })
  
  output$listSelect_UI <- renderUI({
    #isolate({
      ll <-  Biobase::fData(obj)[,DAPAR::keyId(obj)]
      selectInput(ns("listSelect"), "Protein for normalization", choices=ll, multiple = TRUE, width='400px')
   # })
  })
  
  
  output$randomSelect_UI <- renderUI({
    #isolate({
      ll <-  Biobase::fData(obj)[,DAPAR::keyId(obj)]
      textInput(ns("randSelect"), "Random", value="1", width=('120px'))
   # })
  })
  
  output$columnSelect_UI <- renderUI({
    #isolate({
      ll <-  colnames(Biobase::fData(obj))
      selectInput(ns("colSelect"), "Column", choices=ll)
    #})
  })
  
  
  observeEvent(input$listSelect,{
    r$indices  <- if (length(input$listSelect)==0){NULL} else match(input$listSelect, ll)
  })
  
  
  observeEvent(input$randSelect,{
    r$indices <- if (length(input$randSelect)==0){NULL} else sample(1:nrow(obj), as.numeric(input$randSelect), replace=FALSE)
  })
  
  
  observeEvent(input$colSelect,{
    ll <-  Biobase::fData(obj)[,input$colSelect]
    print(head(ll))
     r$indices <- if (length(input$colSelect)==0){NULL} else which(ll == 1)
  })
  
  
  observeEvent(r$indices,{
    print(r$indices)
  })
  
  # BuildResult <- reactive({
  #   
  #   #isolate({
  #   ll <-  Biobase::fData(obj)[,keyId(obj)]
  #   
  #   res <- list(type= input$typeSelect,
  #               list = input$listSelect,
  #               rand = as.numeric(input$randSelect),
  #               col = input$colSelect,
  #               list.indices = if (length(input$listSelect)==0){NULL} else match(input$listSelect, ll),
  #               rand.indices = if (length(input$randSelect)==0){NULL} else sample(1:length(ll), as.numeric(input$randSelect), replace=FALSE),
  #               col.indices =  if (length(input$colSelect)==0){NULL} else which(input$colSelect == 1)
  #   )
  #   
  #   # })
  #   res
  # })
  
  return(reactive({r$indices}))
  
  
}
    
## To be copied in the UI
# mod_plots_tracking_ui("plots_tracking_ui_1")
    
## To be copied in the server
# callModule(mod_plots_tracking_server, "plots_tracking_ui_1")
 
