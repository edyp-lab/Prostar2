


library(shiny)
library(shinyjs)

moduleTestUI <- function(id, label='ui1'){
  ns <- NS(id)
  shinyUI(fluidPage(
    actionButton(ns("btn"), "Submit", class='btn-primary'),
    actionButton(ns("toggle"), "Toggle"),
    shinyjs::hidden(uiOutput(ns('test')))
  ))
}

moduleTest <- function(input, output, session) {
  
  ns <- session$ns
  observeEvent(input$toggle, {
    toggleState("btn")
    toggle("test")
  })
  
  observeEvent(input$btn1, {
    toggleState("btn")
  })
  
    output$test <- renderUI({

      tagList(
        div(
          div(
            # edit1
            style="display:inline-block; vertical-align: middle;",
            tags$b("Test")
          ),
          div(
            # edit2
            style="display:inline-block; vertical-align: middle;",
            actionLink(ns('btn1'), tags$sup("?"),style="background-color: white, color: blue")
          )
        )
      )

    })



    observe({
      req(input$chooseExpDesign)
      shinyjs::onclick("btn1",{
        print(input$btn)
        # shinyjs::toggle(id = "designExamples", anim = TRUE)
      }
      )
    })
  
}

#ui <- tagList(useShinyjs(), htmlOutput("page"))

ui <- fluidPage(
  useShinyjs(),
  tagList(
    htmlOutput("page")
  )
)


server <- function(input, output,session) {
  output$page <- renderUI({
    moduleTestUI('ui1')
  })
  callModule(moduleTest,'ui1')
}

shinyApp(ui = ui, server = server)