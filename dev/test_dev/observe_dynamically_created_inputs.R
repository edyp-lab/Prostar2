
#' Tutorial from https://stackoverflow.com/questions/40631788/shiny-observe-triggered-by-dynamicaly-generated-inputs
#' 
#' Finally, you can extend this approach to any other Shiny widget just 
#' by modifying the selector on the JavaScript function. For instance, 
#' if you want to have actionButton's, you can change the event and the selector 
#' from change and select to click and button.


library(shiny)


ui <- fluidPage(
  
    # keep track of the last selection on all selectInput created dynamically
    tags$script("$(document).on('change', '.dynamicSI select', function () {
                              Shiny.onInputChange('lastSelectId',this.id);
                              // to report changes on the same selectInput
                              Shiny.onInputChange('lastSelect', Math.random());
                             });"),            
    uiOutput("dyn_inputs"),
    uiOutput("text")
  )


server <- function(input, output, session) {
  
  
  rv.dyn <- reactiveVal(rep('#000000',3))
  
  #Dynamic inputs  
  output$dyn_inputs <- renderUI({
    buttons <- as.list(1:3)
    # use a div with class = "dynamicSI" to distinguish from other selectInput's
    div( class = "dynamicSI",
         lapply(buttons, function(i)
           column(3,
                  # selectInput(inputId = paste0("input_",i),
                  #             label = paste("Element",i),
                  #             choices = paste0(LETTERS[i],seq(1,i*2)),
                  #             selected = 1)
                  colourpicker::colourInput(inputId = paste0("input_",i), 
                                            label = paste("Element",i),
                                            '#000000',
                                            showColour = "background")
           )
         )
    )
  })
  
  # react to changes in dynamically generated selectInput's
  observe({
    input$lastSelect
    
    if (!is.null(input$lastSelectId)) {
      cat("lastSelectId:", input$lastSelectId, "\n")
      cat("Selection:", input[[input$lastSelectId]], "\n\n")
    }
    
    #isolate({ #I dont want to have the numericInput input$graph_tytle_num to be a trigger
      #Create the graph title
      rv.dyn <- c()
      for(i in 1:3){
        rv.dyn <- paste(rv.dyn,input[[paste0("input_",i)]])
      }
      
      output$text <- renderUI({
        # rv.dyn
        # 
        # print(rv.dyn)
        p(rv.dyn)
      })
    #})
  }) 
  
  
  
  
}

shinyApp(ui, server)