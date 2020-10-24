library(shiny)
library(gargoyle)
ui <- function(){
  fluidPage(
    tagList(
      # Creating an action button to launch the computation
      actionButton("compute", "Compute"), 
      # Output for all runif()
      verbatimTextOutput("result"), 
      # This output will change only if runif() > 0.5
      verbatimTextOutput("result2"),
      # This button will reset x$results to 0, we use it 
      # to show that it won't launch a series of reactivity 
      # invalidation
      actionButton("reset", "Reset x")
    )
  )
}

server <- function(
  input, 
  output, 
  session
){
  
  # Mimic an R6 class, i.e a non-reactive object
  x <- environment()
  
  # Creating two watchers
  init("render_result", "render_result2")
  
  observeEvent( input$compute , {
    # When the user presses compute, we launch runif()
    x$results <- runif(1)
    # Every time a new value is stored, we render result
    trigger("render_result")
    # Only render the second result if x$results is over 0.5
    if (x$results > 0.5){
      trigger("render_result2")
    }
  })
  
  output$result <- renderPrint({
    # Will be rendered every time
    watch("render_result")
    # require x$results before rendering the output
    req(x$results)
    x$results
  })
  
  
  output$result2 <- renderPrint({
    # This will only be rendered if trigger("render_result2")
    # is called
    watch("render_result2")
    req(x$results)
    x$results
  })
  
  observeEvent( input$reset , {
    # This resets x$results. This code block is here
    # to show that reactivity is not triggered in this app unless
    # a trigger() is called
    x$results <-  0
    print(x$results)
  })
  
}

shinyApp(ui, server)