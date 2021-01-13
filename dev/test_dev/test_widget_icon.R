library(shiny)
ui <- fluidPage(
  actionButton('test', 'test', icon('folder'))
)


server <- function(input, output,session) {

}

shinyApp(ui, server)