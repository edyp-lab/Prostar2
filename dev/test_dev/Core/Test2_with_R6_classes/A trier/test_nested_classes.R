# app.R
library(shiny); library(R6)

# TODO make uiOutput work with nested classes

MyChildModule = R6Class(
  "MyChildModule",
  public = list(id = NULL,
                res = NULL,
                initialize = function(id){
                  self$id = id
                  },
                ui = function(){
                  ns = NS(self$id)
                  tagList(h4(self$id),
                          uiOutput(ns("showText")),
                          selectInput(ns("select_UI"), 'Select in UI', choices=1:3),
                          uiOutput(ns("showSelect2")),
                          actionButton(ns("do"), "Calc!")
                          )
        },
    
    server = function(){
      moduleServer(self$id, function(input, output, session) {
        ns = NS(self$id)
        output$showText <- renderUI({
          print(names(input))
          tagList(
            p("I am the showText renderUI of the child class")
            )
          })
        output$showSelect2 <- renderUI({
          selectInput(ns("select2"),'Select2', choices=1:3)
        })
      observeEvent(input$do,{print(input$do)})
      }
  )
    }
  )
  )


#----------------------------------------------------------


MyMotherModule = R6Class(
  "MyMotherModule",
  public = list(child = NULL,
                id = NULL,
                rv=reactiveValues(res=NULL),
                initialize = function(id){
                  self$id = id
                  self$child <- MyChildModule$new(NS(id)("child"))
                  },
                ui = function(){
                  self$child$ui()
                  },
                server = function(){
                  moduleServer(self$id, function(input, output, session) {
                    self$rv$res <- self$child$server()
                    #observeEvent(self$rv$res(),{print(self$rv$res())})
                    }
                  )
                  }
                )
  )

#----------------------------------------------------------


mother = MyMotherModule$new('mother')
child = MyChildModule$new('child')

# UI
ui = function(){
      tagList(
        mother$ui(),
        hr(),
        child$ui()
      )
}
    
# server
server = function(input, output, session){
      child$server()
      mother$server()
}



shiny::shinyApp(ui, server)