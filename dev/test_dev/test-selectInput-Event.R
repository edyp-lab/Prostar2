library(shiny)

ui <- fluidPage(
  selectInput('select', 'Select', 
              choices=1:20,
              multiple = TRUE
              )
)



server <- function(input, output, session) {
  
 observeEvent(input$select, ignoreNULL = FALSE, {
   
   print(input$select)
 })
  
}


shinyApp(ui, server)
