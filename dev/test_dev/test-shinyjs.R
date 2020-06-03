


library(shiny)
library(shinyjs)

moduleTestUI <- function(id, label='ui1'){
  ns <- NS(id)
  shinyjs:: useShinyjs()
  tagList(
    #shinyUI(fluidPage(
    actionButton(ns("btn"), "Submit", class='btn-primary'),
    actionButton(ns("toggle"), "Toggle"),
    hidden(uiOutput(ns('showExamples')))
   
  #)
  )
}

moduleTest <- function(input, output, session) {
  
  ns <- session$ns
  observeEvent(input$toggle, {
    toggleState("btn")
    toggle("showExamples")
  })
  
  callModule(mod_build_design_example_server, 'designExamples', designLevel = reactive({2})  )
  
  
  output$showExamples <- renderUI({
    mod_build_design_example_ui(ns('designExamples') )
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
      shinyjs::onclick("btn1",{
        print(input$btn)
        # shinyjs::toggle(id = "designExamples", anim = TRUE)
      }
      )
    })
  
}

#ui <- tagList(useShinyjs(), htmlOutput("page"))

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tagList(
    #htmlOutput("page")
    moduleTestUI('ui1')
  )
)


server <- function(input, output,session) {
  output$page <- renderUI({
    moduleTestUI('ui1')
  })
  callModule(moduleTest,'ui1')
}

shinyApp(ui = ui, server = server)