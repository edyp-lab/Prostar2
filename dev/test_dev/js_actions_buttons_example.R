library(shiny)
ui <- function(){
  tagList(
    # Adding a button with an onclick event, that will show or hide the plot
    actionButton(
      "change", 
      "show/hide graph", 
      # The toggle() function hide or show the queried element
      onclick = "$('#plot').toggle()"
    ), 
    plotOutput("plot")
  )
}

server <- function(
  input, 
  output, 
  session
){
  output$plot <- renderPlot({
    # This renderPlot will only be called once
    cli::cat_rule("Rendering plot")
    plot(iris)
  })
}

shinyApp(ui, server) 




#--------------------- with shinyjs hide/show

ui <- function(){
  tagList(
    shinyjs::useShinyjs(),
    # Adding a button with an onclick event, that will show or hide the plot
    actionButton(
      "change", 
      "show/hide graph"), 
    plotOutput("plot")
  )
}

server <- function(
  input, 
  output, 
  session
){
  output$plot <- renderPlot({
    # This renderPlot will only be called once
    cli::cat_rule("Rendering plot")
    plot(iris)
  })
  
  observeEvent(input$change,{
    shinyjs::toggle('plot', condition=input$change%%2)
  })
}

shinyApp(ui, server) 