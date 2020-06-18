library(shiny)

#source(file.path('.', 'withProgress_Calcul.R'), local=TRUE)$value


ui <- fluidPage(
  actionButton("btn_start",label = "Let's stream"),
  actionButton("btn_stop",label = "Stop"),
  htmlOutput("textstream_output")
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
    
    output$textstream_output <- renderUI({
      
      N=3
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Calculation in progress")
      
      for (i in 1:N) {
        progress$inc(1/N, detail = HTML(rv$textstream) )
      }
      
      HTML(rv$textstream)
    })
  })
  
  observeEvent(input$btn_stop, { rv$started <- FALSE })
  
  
  observe({
    rv$timer()
    if (isolate(rv$started)){
      rv$textstream <- paste(readLines("sink-steps.txt"), collapse = "<br/>")
    }
  })
  
  
  
  
  #### pour faire disparaitre fichier sink-steps.txt quand session terminee
  plop <- observe({
    rv$started <- FALSE
  })
  
  # Remove sink file when session ended
  session$onSessionEnded(function() {
    plop$suspend()
    unlink("sink-steps.txt")
  })
  
  
}

shinyApp(ui, server)

