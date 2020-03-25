# Module UI

#' @title   mod_plots_corr_matrix_ui and mod_plots_corr_matrix_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_plots_corr_matrix
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_plots_corr_matrix_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      tags$div(style="display:inline-block; vertical-align: middle;",
               tags$p("Plot options")
      ),
      
      tags$div(style="display:inline-block; vertical-align: middle;",
               
               tags$div(
                 tags$div(style="display:inline-block; vertical-align: top; material-circle;",
                          shinyWidgets::dropdownButton(
                            tags$div(
                              tags$div(style="display:inline-block; vertical-align: bottom;",
                                       sliderInput(ns("expGradientRate"),
                                                   "Tune to modify the color gradient",
                                                   #min = 0,max = 1,value = defaultGradientRate,step=0.01),
                                                   min = 0,max = 1,value = 0.9,step=0.01),
                                       tooltip="Plots parameters"
                                       
                              )
                            ),
                            tooltip="Plots parameters",
                            icon = icon("gear"), status = "info" #status = optionsBtnClass
                          ))
               )
               
      )
    ),
    #highchartOutput(ns("corrMatrix"),width = plotWidth,height = plotHeight)
    highchartOutput(ns("corrMatrix"),width = "800px",height = "600px")
  )
}

# Module Server

#' @rdname mod_plots_corr_matrix
#' @export
#' @keywords internal

mod_plots_corr_matrix_server <- function(input, output, session, obj = NULL){
  ns <- session$ns
  
  observe({
    req(obj())
    if (class(obj())[1] != "MSnSet") {return(NULL)}
  })
  
  corrMatrix <- reactive({
    
    req(obj())
    input$expGradientRate
    
    gradient <- NULL
    #if (is.null(input$expGradientRate)){gradient <- rv.prostar$settings()$corrMatrixGradient}
    if (is.null(input$expGradientRate)){gradient <- 0.9}
    else{
      gradient <- input$expGradientRate}
    isolate({
      tmp <- wrapper.corrMatrixD_HC(obj(),gradient)
      
    })
    tmp
  })
  
  
  output$corrMatrix <- renderHighchart({
    corrMatrix()
  }) 
  
  
}

## To be copied in the UI
# mod_plots_corr_matrix_ui("plots_corr_matrix_ui_1")

## To be copied in the server
# callModule(mod_plots_corr_matrix_server, "plots_corr_matrix_ui_1")

