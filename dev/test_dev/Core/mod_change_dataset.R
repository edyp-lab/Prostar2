#' change_assay UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_change_dataset_ui <- function(id){
  ns <- NS(id)
  
  absolutePanel(
    # id  = "#AbsolutePanel",
    #class = "panel panel-default",
    style= "text-align: center; z-index: 10000;",
    top = 0, 
    right = 50, 
    width = "250px",
    height = "50px",
    draggable = FALSE,
    fixed = FALSE,
    cursor = "default",
    uiOutput(ns("datasetAbsPanel"))
    
  )
}

#' change_assay Server Function
#'
#' @noRd 
mod_change_dataset_server <- function(input, output, session, ll.se, indice){
  ns <- session$ns
  
  rv.change <- reactiveValues(
    out = NULL
  )
  
  
  observe({
    req(indice())
    rv.change$out <- indice()
  })
  
  
  output$datasetAbsPanel <- renderUI({
    req(c(ll.se(), indice()))

        tagList(
      div(
        div(
          style="display:inline-block; vertical-align: middle; margin:0px",
          p('Current assay')
        ),
        div(
          style="display:inline-block; vertical-align: middle; margin:0px",
          selectInput(ns('currentDataset'), '',
                      choices = ll.se(),
                      selected = ll.se()[indice()],
                      width='150px')
        )
      )
    )
    
  })
  
  
  
  ## manual change of current dataset
  observeEvent(input$currentDataset,{
    
    n <- which(ll.se() == input$currentDataset)
    if (length(n)==0){
      rv.change$out <- 1
    } else {
      rv.change$out <- n
    }
    
  })
  
  
  return(reactive({rv.change$out}))
}

## To be copied in the UI
# mod_change_assay_ui("change_assay_ui_1")

## To be copied in the server
# callModule(mod_change_assay_server, "change_assay_ui_1")

