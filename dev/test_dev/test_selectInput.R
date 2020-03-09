library(shiny)


ui <- fluidPage(
  tagList(
   uiOutput('selectInput_ui'),
   verbatimTextOutput('show'),
   verbatimTextOutput('show_req')
  )
)


server <- function(input, output,session) {
  output$selectInput_ui <- renderUI({
   choices = c('','A', 'B', 'C')
   names(choices) <- c('None', 'choix A', 'choix B', 'choix C')
   selectInput('choice', 'test', choices = choices, selected=character(0), width=100)
  })

  output$show <- renderText({
    input$choice
  })
  
  output$show_req <- renderText({
    req(input$choice)
  input$choice
  })
}

shinyApp(ui = ui, server = server)