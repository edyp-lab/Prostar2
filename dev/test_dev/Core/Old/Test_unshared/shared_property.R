library(shiny)
library(R6)

testBaseModel <- R6Class("testBaseModel",
                         public = list(
                           nonReactiveVar = FALSE,
                           reactiveVar = reactiveValues(bTest = FALSE)
                         )
)

testModel <- R6Class("testModel",
                     inherit = testBaseModel,
                     public = list(
                     )
)

ui <- function() {
  fluidPage(
    actionButton("button", "button"),
    textOutput("instanceA"),
    textOutput("instanceB")
  )
}

server <- function(input, output, session) {
  modelInstanceA <- testModel$new()
  modelInstanceB <- testModel$new()
  
  observeEvent(input$button, {
    modelInstanceA$reactiveVar$bTest <- TRUE
    modelInstanceA$nonReactiveVar <- TRUE
    
    # changes both instances
    output$instanceA <- renderText({
      paste(modelInstanceA$reactiveVar$bTest, modelInstanceA$nonReactiveVar)
    })
    output$instanceB <- renderText({ 
      paste(modelInstanceB$reactiveVar$bTest, modelInstanceB$nonReactiveVar)
    })
  })
}

shinyApp(ui, server)