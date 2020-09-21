library(shiny)
 library(shinyWidgets)

 ui <- fluidPage(
       pickerInput(
             inputId = "id1",
             label = "Select :",
             choices = c("A", "B", "C"),
             multiple = FALSE,
            choicesOpt = list(
                   disabled = c("A", "B", "C") %in% c("C")
               )
         ),
       verbatimTextOutput(outputId = "result")
   )
 server <- function(input, output) {
       output$result <- renderPrint(input$id1)
   }
 shinyApp(ui, server)
 
 