toto <- function(logFile) {
  sink(logFile, split=T)
  Sys.sleep(5)
  cat("step0\n")
  sink(logFile,append=T, split=T)
  for (i in 1:2) {
    Sys.sleep(5)
    cat(paste0("step",i,"\n"))
  }
  sink()
}



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