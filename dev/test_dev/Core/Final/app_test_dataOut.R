library(shiny)
library(R6)
library(tibble)

verbose = F

options(shiny.fullstacktrace = T)

#----------------------------------------------------------------------------

Child1 <- R6Class(
  "Child1",
  public = list(
    id = NULL,
    rv = reactiveValues(
      dataIn = NULL),
    initialize = function(id){
      self$id <- id
    },
    
    ui = function() {
      ns <- NS(self$id)
      fluidPage(
        actionButton(ns('send'), 'Send result')
      )
    },
    
    server = function(dataIn, dataOut ) {
      ns <- NS(self$id)
      utils::data(Exp1_R25_prot, package='DAPARdata2')
      
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        observeEvent(input$send, {
          dataOut$trigger <- rnorm(1,1)
          dataOut$value <- input$send
          dataOut$name <- self$id
        })
        
      })
    }
  )
)



#---------------------------------------------------------------------------

Child2 <- R6Class(
  "Child2",
  public = list(
    id = NULL,
    rv = reactiveValues(
      dataIn = NULL),
    initialize = function(id){
      self$id <- id
    },
    
    ui = function() {
      ns <- NS(self$id)
      fluidPage(
        actionButton(ns('send'), 'Send result')
      )
    },
    
    server = function(dataIn, dataOut ) {
      ns <- NS(self$id)
      utils::data(Exp1_R25_prot, package='DAPARdata2')
      
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        observeEvent(input$send, {
          dataOut$trigger <- rnorm(1,1)
          dataOut$value <- input$send
          dataOut$name <- self$id
        })

      })
    }
  )
)

#---------------------------------------------------------------------------

Parent <- R6Class(
  "Parent",
  public = list(
    id = NULL,
    rv = reactiveValues(
      dataIn = NULL),
    #return_of_process = "<reactiveValues>",
    return_of_process = reactiveValues(
      value = 'init'
    ),
    
    child.process = list(
      #ProcessOriginal = NULL
      Child1 = NULL,
      Child2 = NULL
    ),
    
    initialize = function(id){
      self$id <- id
      
    },
    
    
    
    ui = function() {
      ns <- NS(self$id)
      fluidPage(
        uiOutput(ns('show_ui'))
      )
    },
    
    server = function(dataIn ) {
      ns <- NS(self$id)
      utils::data(Exp1_R25_prot, package='DAPARdata2')
      
      
      self$child.process <- setNames(lapply(names(self$child.process),
                                            function(x){
                                              assign(x, get(x))$new(x)
                                            }),
                                     names(self$child.process)
      )
      
      lapply(names(self$child.process), function(x){
        self$child.process[[x]]$server(dataIn = reactive({Exp1_R25_prot}),
                                       dataOut = self$return_of_process
                                       )
      })
      

      
      observeEvent(self$return_of_process$trigger, {
        #browser()
        cat('New data for return_of_process :\n')
        cat(paste0('Name = ', self$return_of_process$name, '\n'))
        cat(paste0('Value = ', self$return_of_process$value, '\n'))
      })
      
      
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)

        output$show_ui <- renderUI({
          tagList(
            lapply(names(self$child.process), function(x){
              wellPanel(h3(x), self$child.process[[x]]$ui())
            })
          )
        })

      })
    }
  )
)





#---------------------------------------------------------------------------

## Main app
rv <- reactiveValues()
parent <- Parent$new('App')
ui = fluidPage(
  parent$ui()
)

server = function(input, output){
  utils::data(Exp1_R25_prot, package='DAPARdata2')
  
  parent$server(dataIn = reactive({rv$dataIn}))

  
}
shiny::shinyApp(ui, server)