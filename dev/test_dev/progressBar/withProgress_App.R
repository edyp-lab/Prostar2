library(shiny)

ui <- fluidPage(
  actionButton("btn_start",label = "Let's stream"),
  actionButton("btn_stop",label = "Stop"),
  htmlOutput("textstream_output")
)


server <- function(input, output, session) {
  
  cat(NULL,file="sink-steps.txt")
  N=6
  rv <- reactiveValues(textstream = c(""),
                       timer = reactiveTimer(500),
                       started = FALSE)
  progress <- NULL
  
  observeEvent(input$btn_start, {
    
    rv$started <- TRUE
    
    system2("Rscript", "withProgress_Calcul.R", wait = FALSE)
    
    progress <- Progress$new(session, min=1, max=N+1)
    
    progress$set(value=0, message='plop')
    
    output$textstream_output <- renderUI({
      
      for (i in 1:N) {
        progress$inc(1/N, detail = tail(rv$textstream,1))
      }
      
      HTML(paste(rv$textstream, collapse = "<br/>"))
      
    })
    
    # remove bar at the end of script, <=> when print("end")
    observe({
      if( identical(tail(rv$textstream,1), "end") ) {
        Sys.sleep(1)
        progress$close()
      }
    })
    
    
  })
  
  
  observe({
    rv$timer()
    if (isolate(rv$started)){
      # as soon as started==TRUE, every 1/2 sec, check sink-steps.txt
      rv$textstream <- readLines("sink-steps.txt")
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

