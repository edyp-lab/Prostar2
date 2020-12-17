library(shiny)
library(shinyjs)
library(R6)

NUM_PAGES <- 3


Timeline  <- R6Class(
  "Timeline",
  public = list(
    id = NULL,
    rv = "<reactiveValues>",
    initialize = function(id){
      self$id <- id
      self$rv =reactiveValues(page=1,
                              ll.screens=NULL)
    },
    
GetScreens = function(){
  ns <- NS(self$id)
  
    lapply(1:3, function(i) {
      if (i==1) div(
    class = paste0("page_", self$id),
    id = ns(paste0("step", i)),
    self$rv$ll.screens[[i]]
  )
      else 
        hidden(div(
        class = paste0("page_", self$id),
        id = ns(paste0("step", i)),
        self$rv$ll.screens[[i]]
      )
      )
  }
  )

},
    
    ui = function(){
      ns <- NS(self$id)
      fluidPage(
        useShinyjs(),
        self$GetScreens(),
        br(),
        actionButton(ns("prevBtn"), "< Previous"),
        actionButton(ns("nextBtn"), "Next >"),
        actionButton(ns('toggle'), 'toggle')
      )
    },
    
    server = function(ll.screens){
      ns <- NS(self$id)
      
      
      
       moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        observeEvent(ll.screens,{self$rv$ll.screens <- ll.screens})
        
        observe({
          toggleState(id = "prevBtn", condition = self$rv$page > 1)
          toggleState(id = "nextBtn", condition = self$rv$page < NUM_PAGES)
          shinyjs::hide(selector = paste0(".page_", self$id))
          shinyjs::show(paste0("step", self$rv$page))
        })


        navPage <- function(direction) {
          self$rv$page <- self$rv$page + direction
        }
        
        observeEvent(input$prevBtn, navPage(-1))
        observeEvent(input$nextBtn, navPage(1))
        
        observeEvent(input$toggle,{
          lapply(1:3, function(x){shinyjs::toggleState(paste0('step', x), condition=input$toggle%%2 == 0)})
          })
        
      }
      )
    }
  )
)



Child <- R6Class(
  "Child",
  inherit = Process,
  public = list(
    
    ### Step 1
    step1 = function(){
      ns <- NS(self$id)
      
      select1 <- function(){
        renderUI({
          ns <- NS(self$id)
          selectInput(ns('sel1'), 'Select 1', choices=1:3)
        })
      }
      
      observeEvent(self$input$btn1, {cat('btn 1 clicked, selection = ', self$input$sel1, '\n')})
      
      tagList(
        h4(paste0("Step", 1)),
        selectInput(ns('sel1'), 'Select 1', choices=1:3),
        actionButton(ns(paste0('btn',1)), paste0('btn ',1))
      )
      
    },
    
    
    
    ### Step 2
    step2 = function(input){
      ns <- NS(self$id)
      
      observeEvent(self$input$btn2, {cat('btn 2 clicked, selection = ', self$input$sel2, '\n')})
      
      tagList(
        h4(paste0("Step", 2)),
        selectInput(ns('sel2'), 'Select 2', choices=1:3),
        actionButton(ns(paste0('btn',2)), paste0('btn ',2))
      )
      
    },
    
    ### Step 3
    step3 = function(){
      ns <- NS(self$id)
      
      observeEvent(self$input$btn3, {cat('btn 3 clicked, selection = ', self$input$sel3, '\n')})
      
      tagList(
        h4(paste0("Step", 3)),
        selectInput(ns('sel3'), 'Select 3', choices=1:3),
        actionButton(ns(paste0('btn',3)), paste0('btn ',3))
      )
    }

    
  )
)







Process  <- R6Class(
  "Process",
  public = list(
    id = NULL,
    input = NULL,
    rv = "<reactiveValues>",
    tl = NULL,
    length= 3,
    
    initialize = function(id){
      self$id <- id
      self$rv = reactiveValues(
        toto = NULL,
        ll.screens = NULL
      )
    },
    
    GetScreensDefinition = function(){
      lapply(1:self$length, function(x){
        eval(parse(text = paste0("self$step", x, '()')))
      })
      },
    
    
    ui = function(){
      ns <- NS(self$id)
      fluidPage(
        uiOutput(ns('show_tl'))
      )
    },
    
    server = function(){
      ns <- NS(self$id)

      #self$Add_UIs()
     # ll.screens <- lapply(1:3, function(x){do.call(paste0('self$step', x), list())})
     
      self$tl <- Timeline$new(ns('App'))
      self$tl$server(self$rv$ll.screens)
      
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        observe({ self$input <- input })
        
        
        output$show_tl <- renderUI({
          req(self$tl)
          self$tl$ui()
        })

        self$rv$ll.screens <- self$GetScreensDefinition()

      }
      )
    }
  )
)

#proc <- Process$new('App')
#ui = fluidPage(proc$ui())
#server = function(input, output){proc$server()}

child1 <- Child$new('App1')
child2 <- Child$new('App2')
ui = fluidPage(
  tagList(
    child1$ui(),
    child2$ui()
    )
)
server = function(input, output){
  child1$server()
  child2$server()
  }


shiny::shinyApp(ui, server)