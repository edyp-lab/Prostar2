# Module UI

#' @title   mod_plots_corr_matrix_ui and mod_plots_corr_matrix_server
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
#' @rdname mod_plots_corr_matrix
#'
#' @keywords internal
#' 
#' @export 
#' 
#' @importFrom shiny NS tagList
#' 
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
                                       uiOutput(ns('gradient_ui')),
                                       tooltip="Plots parameters"
                                       
                              )
                            ),
                            tooltip="Plots parameters",
                            icon = icon("gear"), status = optionsBtnClass
                          ))
               )
               
      )
    ),
    #highchartOutput(ns("corrMatrix"),width = plotWidth,height = plotHeight)
    highchartOutput(ns("corrMatrix"),width = '600px',height = '500px')
  )
}

# Module Server

#' @param obj xxx
#' 
#' @param names xxx
#' 
#' @param gradientRate xxx
#' 
#' @rdname mod_plots_corr_matrix
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @importFrom SummarizedExperiment assay
#' 
#' @importFrom DAPAR2 corrMatrixD_HC
#' 
mod_plots_corr_matrix_server <- function(input, output, session,
                                         obj,
                                         names=NULL,
                                         gradientRate=NULL){
  
  ns <- session$ns
  
  
  rv.corr <- reactiveValues(
    gradient = NULL
  )
  
  
  output$gradient_ui <- renderUI({
    req(rv.corr$gradient)
    sliderInput(ns("expGradientRate"),
                "Tune to modify the color gradient",
                min = 0,
                max = 1,
                value = rv.corr$gradient,
                step=0.01)
  })
  
  observeEvent(req(gradientRate()),{
    rv.corr$gradient <- gradientRate()
  })
  
  observeEvent(req(input$expGradientRate),{
    rv.corr$gradient <- input$expGradientRate
  })
  
  
  corrMatrix <- reactive({
    req(obj())
    rv.corr$gradient 
1
    isolate({
      withProgress(message = 'Making plot', value = 100, {
        tmp <- DAPAR2::corrMatrixD_HC(obj(), names(), rv.corr$gradient)
      })
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

