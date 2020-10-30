library(shiny)
library(R6)

ChildClass <- R6Class(
  "ChildClass",
  private = list(
    config = list()
  ),
  public = list(
    id = NULL,
    initialize = function(id){
      self$id <- id
    },
    
    ui = function(){
      ns <- NS(self$id)
      tagList(
        shinyjs::useShinyjs(),
        uiOutput(ns('showScreen'))
        )
    },
    
    server = function(show, config){
      ns <- NS(self$id)
      
      
      observeEvent(config,{
        private$config <- config
        private$config$screens <- lapply(1:length(private$config$screens),
                                         function(x){
                                           private$config$screens[[x]] <- div(id = ns(paste0("div_screen", x)),  private$config$screens[[x]])
                                         })
      })
      
      
      
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        
        observeEvent(show(), ignoreNULL=T,{
          lapply(1:length(private$config$screens), function(x){
            shinyjs::toggle(paste0('div_screen', x), condition = show())})
        })
        
        output$showScreen <- renderUI({ tagList(private$config$screens)})

      }
      )
    }
  )
)


MotherClass <- R6Class(
  "MotherClass",
  public = list(id = NULL,
                config = reactiveValues(),
                child = NULL,
                rv = reactiveValues(
                  showScreenBtn=0
                  ),
                
                initialize = function(id){
                  self$id <- id
                  self$child <- ChildClass$new(NS(self$id)('child'))
                  
                },
                
                ui = function(){
                  ns <- NS(self$id)
                  tagList(
                    shinyjs::useShinyjs(),
                    actionButton(ns('showScreenBtn'), "Show/hide screen"),
                    self$child$ui()
                  )
                },
                
                server = function(){
                  ns <- NS(self$id)
                  
                  self$child$server(show = reactive({self$rv$showScreenBtn}),
                                    config = self$config
                                    )
                  self$config$screens <- list(Description = uiOutput(ns('Description')))
                  
                  moduleServer(self$id, function(input, output, session) {
                    
                    observeEvent(input$showScreenBtn,{self$rv$showScreenBtn <- (input$showScreenBtn %% 2 ) == 0})
                    
                    output$Description <- renderUI({p('Description') })
                  }
                  )
                }
  )
)

app <- MotherClass$new('toto')
ui = fluidPage(app$ui())
server = function(input, output){app$server()}
shiny::shinyApp(ui, server)