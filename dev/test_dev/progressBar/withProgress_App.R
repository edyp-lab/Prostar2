library(shiny)

ui <- fluidPage(
  actionButton("btn_start",label = "Let's stream"),
  actionButton("btn_stop",label = "Stop"),
  #verbatimTextOutput("fileReaderText"),
  htmlOutput("textstream_output")
  #,plotOutput("plot")
)


server <- function(input, output, session) {
  
  cat(NULL,file="sink-steps.txt")
  #details <- reactiveFileReader(500, NULL, "./sink-steps.txt", readLines)
  
  rv <- reactiveValues(textstream = c(""),
                       timer = reactiveTimer(500),
                       started = FALSE)
  
  observeEvent(input$btn_start, { 
    rv$started <- TRUE
    system2("Rscript", "withProgress_Calcul.R", wait = FALSE)
  })
  observeEvent(input$btn_stop, { rv$started <- FALSE })
  observe({
    rv$timer()
    if (isolate(rv$started))
      rv$textstream <- paste(readLines("sink-steps.txt"), collapse = "<br/>")
  })
  output$textstream_output <- renderUI({
    HTML(rv$textstream)
  })
  

  ###################### a reparer ##############################
  # output$fileReaderText <- renderText({ # trouver comment afficher ca en temps reel
  #   text <- details()
  #   length(text) <- 5
  #   text[is.na(text)] <- ""
  #   paste(text, collapse = '\n')
  # })
  
  
  #### pour faire disparaitre fichier sink-steps.txt quand session terminee
  plop <- observe({
    rv$started <- FALSE
  })
  
  # Remove sink file when session ended
  session$onSessionEnded(function() {
    plop$suspend()
    unlink("sink-steps.txt")
  })
  
  ###################### a reparer ##############################
  # output$plot <- renderPlot({ # pour le Return de withProgress_appCalcul()
  #   N=5
  #   progress <- shiny::Progress$new()
  #   on.exit(progress$close())
  #   progress$set(message = "Calculation in progress", value = 100)
  #   for (i in 1:N) {
  #     progress$inc(1/N, detail = details()[i])
  #   }
  # })
}

shinyApp(ui, server)

