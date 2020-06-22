library(shiny)

ui <- fluidPage(
  actionButton("btn_start",label = "Let's stream"),
  actionButton("btn_stop",label = "Stop"),
  htmlOutput("textstream_output")
)


server <- function(input, output, session) {
  
  cat(NULL,file="sink-steps.txt")
  N=3
  rv <- reactiveValues(textstream = c(""),
                       timer = reactiveTimer(500),
                       started = FALSE)
  
  observe({
    rv$timer()
    if (isolate(rv$started)){
      rv$textstream <- paste(readLines("sink-steps.txt"), collapse = "<br/>")
    }
  })
  
  
  observeEvent(input$btn_start, {
    
    progress <- Progress$new(session)
    observeEvent(input$btn_stop, { 
      progress$close()
    })
    
    rv$started <- TRUE
    
    system2("Rscript", "withProgress_Calcul.R", wait = FALSE)
    
    output$textstream_output <- renderUI({
      
      progress$set(value=0, message='plop')
      #ajouter truc genre, si pareil sinon update
      # plus gerer amount progress$inc
      for (i in 1:N) {
        progress$set(i, detail = gsub("<br/>"," ",rv$textstream))
      }
      
      HTML(rv$textstream)
      
    })
    
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

