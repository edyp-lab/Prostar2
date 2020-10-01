#' observe_dynamic_colourPicker_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @param n The number of inputs to create
#' 
#' @return A vector containing n hex color values
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList tags
mod_observe_dynamic_colourPicker_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    # keep track of the last selection on all selectInput created dynamically
    tags$script("$(document).on('change', '.dynamicSI select', function () {
                              Shiny.onInputChange('lastSelectId',this.id);
                              // to report changes on the same selectInput
                              Shiny.onInputChange('lastSelect', Math.random());
                             });"),            
    uiOutput(ns("dyn_inputs"))
  )
}

#' observe_dynamic_colourPicker_input Server Function
#'
#' @noRd 
mod_observe_dynamic_colourPicker_input_server <- function(id, n=NULL, label=NULL){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    dynColors <- reactiveVal()
    defaultColor <- '#CAC8C6'
    
    observe({
      if(is.null(n()))
        stop("'n' is null.")
      
      if (!is.null(n()) && !is.null(label()))
        if (length(label()) != n())
          stop("The length of 'label' must be equal to the value of 'n'.")
    })
    
    #Dynamic inputs  
    output$dyn_inputs <- renderUI({
      buttons <- as.list(1:n())
      # use a div with class = "dynamicSI" to distinguish from other selectInput's
      div( class = "dynamicSI",
           lapply(buttons, function(i)
             column(n(),
                    colourpicker::colourInput(inputId = ns(paste0("input_",i)), 
                                              label = if (is.null(label())) paste0('color ', i) else label()[i],
                                              value = defaultColor,
                                              showColour = "background")
             )
           )
      )
    })
    
    
    # react to changes in dynamically generated selectInput's
    observe({
      input$lastSelect
      input$lastSelectId
      
      tmp <- NULL
      for(i in 1:n()){
        tmp <- c(tmp, input[[paste0("input_",i)]])
      }
      dynColors(tmp)
    })
    
    return(reactive({dynColors()}))
    
    
  })
  
  
}
    
## To be copied in the UI
# mod_observe_dynamic_colourPicker_input_ui("observe_dynamic_colourPicker_input_ui_1")
    
## To be copied in the server
# callModule(mod_observe_dynamic_colourPicker_input_server, "observe_dynamic_colourPicker_input_ui_1")
 
