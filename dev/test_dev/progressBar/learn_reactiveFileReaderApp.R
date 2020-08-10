source(file.path('.', 'learn_reactiveFileReaderCalcul.R'), local=TRUE)$value

ui <- fluidPage(
  verbatimTextOutput("fileReaderText")
)

server <- function(input, output,session) {
  
  logFile="./sink-steps.txt"
  toto(logFile)
  fileReaderData <- reactiveFileReader(500, session, logFile, readLines)
  
  
  output$fileReaderText <- renderText({
    text <- fileReaderData()
    paste(text, collapse = '\n')
  })
  
}

shinyApp(ui, server)

