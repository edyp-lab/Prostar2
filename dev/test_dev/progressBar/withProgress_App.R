library(shiny)

ui <- fluidPage(
  actionButton("btn_start",label = "Let's stream"),
  actionButton("btn_stop",label = "Stop"),
  htmlOutput("textstream_output")
)


server <- function(input, output, session) {
  
  cat(NULL,file="sink-steps.txt")
  N=5
  rv <- reactiveValues(textstream = c(""),
                       timer = reactiveTimer(500),
                       started = FALSE)
  details <- ""
  
  
  observeEvent(input$btn_start, {
    
    rv$started <- TRUE
    
    
    system2("Rscript", "withProgress_Calcul.R", wait = FALSE)
    
    progress <- Progress$new(session, min=0, max=N+1)
    #observe({ if (rv$textstream == "end") on.exit(progress$close()) })
    
    progress$set(value=NA, message='plop')
    
    output$textstream_output <- renderUI({
      
      if (rv$textstream != details) {
        for (i in 1:N) {
          
          print(paste("OLD details : ", details))
          print(paste("NEW 'sink-steps.txt' : ", rv$textstream))
          
          details <- gsub("<br/>"," ",rv$textstream)
          
          progress$set(i, detail = details)
        }
      }
      
      HTML(rv$textstream)
      
      
    })
    
    
  })
  
  
  observe({
    rv$timer()
    if (isolate(rv$started)){
      # as soon as started==TRUE, every 1/2 sec, check sink-steps.txt
      rv$textstream <- paste(readLines("sink-steps.txt"), collapse = "<br/>")
    }
  })
  
  
  observeEvent(input$btn_stop, { 
    rv$started <- FALSE
  })
  
  
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

