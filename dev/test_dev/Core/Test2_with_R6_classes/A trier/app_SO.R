library(shiny)
library(R6)

MyModule <- R6Class(
  "MyModule",
  public = list(id = NULL,
                initialize = function(id) {
                  self$id <- id
                }, 
                ui = function() {
                  ns <- NS(self$id)
                  tagList(h4(self$id), 
                          actionButton(ns("do"), "Calc!"), 
                          verbatimTextOutput(ns("print")))
                },
                
                server = function() {
                  moduleServer(self$id, function(input, output, session) {
                    output$print <- renderPrint({
                      input$do
                      sample(100, 1)
                    })
                  })
                }
  )
)

MyMotherModule <- R6Class(
  "MyMotherModule",
  public = list(id = NULL,
                child = NULL,
                initialize = function(id) {
                  self$id <- id
                  self$child <- MyModule$new(NS(id)("child"))
                  
                },
                ui = function() {
                  self$child$ui()
                },
                server = function() {
                  moduleServer(self$id, function(input, output, session) {
                    self$child$server()
                  }
                  )}
  )
)

App <- R6Class(
  "App",
  public = list(child1 = NULL,
                child2 = NULL,
                mother = NULL,
                initialize = function() {
                  self$mother <- MyMotherModule$new("mother1")
                },
                ui = function() {
                  fluidPage(
                    fluidRow(
                      self$mother$ui()
                    )
                  )
                },
                server = function() {
                  function(input, output, session) {
                    self$mother$server()
                  }
                }
  )
)

app <- App$new()

shinyApp(app$ui(), app$server())