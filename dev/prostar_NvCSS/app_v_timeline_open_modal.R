library(shiny)
library(shinyWidgets)
ui <- fluidPage(
  
  tags$h2("Titre"),
  
  tags$ul(id='timeline',
          circleButton(inputId = "button1", status = "primary", size = "sm"),
          #tags$p("paragraphe 1")
          tags$div(style='display:inline;',),
          circleButton(inputId = "button2",status="info", size = "sm"),
          #tags$p("paragraphe 2")
  )
  #radioButtons(inputId, label, choices, selected = NULL, inline = FALSE,width = NULL)
  
)

server <- function(input, output){
  
  
}

shinyApp(ui, server)
