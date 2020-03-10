# Module UI
  
#' @title   mod_plots_intensity_ui and mod_plots_intensity_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_plots_intensity
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_plots_intensity_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      tags$div(style="display:inline-block; vertical-align: middle;",
               tags$p("Plot options")
      ),
      
      tags$div(style="display:inline-block; vertical-align: middle;",
               
               tags$div(
                 tags$div(style="display:inline-block; vertical-align: top;",
                          shinyWidgets::dropdownButton(
                            tags$div(
                              tags$div(style="display:inline-block; vertical-align: bottom;",
                                       selectInput(ns("whichGroup2Color"),
                                                   "Color lines",
                                                   choices=list("By condition" = "Condition",
                                                                "By replicate" = "Replicate"),
                                                   #selected= Group2Color(), width='150px')
                                                   selected= 'Condition', width='150px')
                                                  
                              ),
                              tags$div(style="display:inline-block; vertical-align: bottom;",
                                       uiOutput(ns("ChooseLegendForSamples"))
                              )
                            ),
                            tooltip="Plots parameters",
                            style = "material-circle", icon = icon("gear"), status = optionsBtnClass
                          )))
               
      )),
    
    fluidRow(
      column(width=6, mod_plots_density_ui(ns("densityPlot_AbsPanel"))),
      #column(width=6, moduleBoxplotUI(ns("boxPlot_AbsPanel")))
    )
  )
}
    
# Module Server
    
#' @rdname mod_plots_intensity
#' @export
#' @keywords internal
    
mod_plots_intensity_server <- function(input, output, session, obj = NULL){
  ns <- session$ns
  
  
  # callModule(moduleBoxplot, "boxPlot_AbsPanel", dataIn = reactive({dataIn()}),
  #            params = reactive({NULL}),
  #            reset=reactive({FALSE}))
  callModule(mod_plots_density_server, "densityPlot_AbsPanel", obj = obj)
  
  
  
  output$ChooseLegendForSamples <- renderUI({
    #req(data())
    print("IN output$ChooseLegendForSamples <- renderUI")
    .names <- colnames(Biobase::pData(obj))
    checkboxGroupInput(ns("legendForSamples"),
                       label = "Choose data to show in legend",
                       choices = .names,
                       selected=.names[2])
  })
  
  # observeEvent(input$legendForSamples, {
  #   rv.prostar$settings()$legendForSamples <- as.vector(apply(as.data.frame(Biobase::pData(dataIn()$obj())[,input$legendForSamples]), 1, function(x) paste(x, collapse="_")))
  # })
  
  
  
}
    
## To be copied in the UI
# mod_plots_intensity_ui("plots_intensity_ui_1")
    
## To be copied in the server
# callModule(mod_plots_intensity_server, "plots_intensity_ui_1")
 
