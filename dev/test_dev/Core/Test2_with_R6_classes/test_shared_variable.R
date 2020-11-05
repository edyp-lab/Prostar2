library(R6)


Test <- R6Class(
  "Test",
  inherit = General,
  public = list(
    id = NULL,
    initialize = function(id, config){
      self$id <- id
      
      self$config[[self$id]] <- config
      # observe({
      #   lapply(names(config), function(x){self$config[[x]] <- config[[x]]})
      # })
    }
   
  )
)


General <- R6Class(
  "General",
  public = list(
    id = NULL,
    config = reactiveValues(),
    
    initialize = function(){ },
    
   
    ui = function(){
      ns <- NS(self$id)
      tagList(
        actionButton(ns('change'), 'Simulate change config'),
        uiOutput(ns('show_config'))
      )
    },
    
    server = function(){
      ns <- NS(self$id)
      
      observeEvent(self$config[[self$id]]$steps,{
        p(paste0(self$config[[self$id]]$steps, collapse=' '))
      })
      
     
      
      
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        output$show_config <- renderUI({ p(paste0(self$config[[self$id]]$steps, collapse=' '))})
        
        observeEvent(input$change,{
          print('new event on input$change')
          self$config[[self$id]]$steps[1] <- input$change%%2 ==0
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
  processDescription <- Test$new(NS('app')("process_Description"), 
                                    config = list(name = 'Description', 
                                                  steps = LETTERS[1:5])
                                    )
  processA <- Test$new("process_A", 
                          config = list(name = 'A', 
                                        steps = LETTERS[1:2]))
  processDescription$server()
  processA$server()
  
  output$screen <- renderUI({
    tagList(
      wellPanel(h3('Description'),
                processDescription$ui())
      ,
      wellPanel(h3('process A'),
                processA$ui()
      )
      )
    })

}

shinyApp(ui, server)
