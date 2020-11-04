library(R6)


Test <- R6Class(
  "Test",
  public = list(
    rv = reactiveValues(
      config = list()
    ),
    id = NULL,
    initialize = function(id){
      self$id <- id
    },
    ui = function(){
      ns <- NS(self$id)
      tagList(
        uiOutput(ns('show_config'))
        )
    },
    server = function(config){
      ns <- NS(self$id)
      
      observeEvent(config$steps, {
        print("new event on config parameter")
        self$rv$config <- config})
      
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        observeEvent(self$rv$config,{print('toto')
          self$GetConfig()})
        observeEvent(self$rv$configsteps,{
          print('tutu')
          self$GetConfig()})
        output$show_config <- renderUI({ p(paste0(self$rv$config$steps, collapse=' '))})
      }
      
      )
      
    },
    GetConfig = function(){print(paste0(self$config$steps, collapse=' '))}
  )
)


General <- R6Class(
  "General",
  public = list(
    id = NULL,
    app = NULL,
    rv = reactiveValues(
      config = list()
    ),
    
    initialize = function(id, config){
      self$id <- id
      #self$rv$config$name <- config$name
      #self$rv$config$steps <- config$steps
      self$rv$config <- config
      
    },
    
   
    ui = function(){
      ns <- NS(self$id)
      tagList(
        actionButton(ns('change'), 'Simulate change config'),
        uiOutput(ns('screen'))
      )
    },
    server = function(){
      ns <- NS(self$id)
      observe({
        self$rv$config
        print('nev event on self$rv$config')
      })
      
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        self$app <- Test$new(ns(self$id))
        output$screen <- renderUI({ self$app$ui()})
        self$app$server(config = self$rv$config)
        
        
        observeEvent(self$rv$config$steps,{p(paste0(self$rv$config$steps, collapse=' '))})
        observeEvent(input$change,{
          print('new event on input$change')
          self$rv$config$steps[1] <- input$change%%2 ==0
          })
      }
      
      )
      
    }
  )
)

ui = function(){
  fluidPage(
    tagList(
      p('test'),
      uiOutput('screen')
    )
  )
  }
server = function(input, output, session) {
  processDescription <- General$new(NS('app')("process_Description"), config = list(name = 'Description', steps = LETTERS[1:5]))
  processA <- General$new("process_A", list(name = 'A', steps = LETTERS[1:2]))
  processDescription$server()
  
  output$screen <- renderUI({
    tagList(
      processDescription$ui()
      )
    })

}

shinyApp(ui, server)
