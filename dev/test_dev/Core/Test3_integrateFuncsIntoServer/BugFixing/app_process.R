library(shiny)
library(R6)

#----------------------- Class ProcessManager ----------------------------------
source(file.path('.', 'class_Process.R'), local=TRUE)$value

processDescription <- Process$new("ProcessDescription")
processA <- Process$new("ProcessA")
#processDescription <- Process$new("ProcessDescription")

ui = function() { fluidPage(
  tagList(
    processDescription$ui(),
    processA$ui()
    )
)}

server = function(input, output, session) {
  
  processA$server()
  processDescription$server()
}

shinyApp(ui, server)