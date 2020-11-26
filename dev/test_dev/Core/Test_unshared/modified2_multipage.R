library(shiny)
library(shinyjs)
library(R6)

NUM_PAGES <- 3


Timeline  <- R6Class(
  "Timeline",
  public = list(
    id = NULL,
    rv =reactiveValues(page=1,
                        ll.screens=NULL),
    initialize = function(id){
      self$id <- id
    },
    
GetScreens = function(){
  ns <- NS(self$id)
  
    lapply(1:3, function(i) {
      if (i==1) div(
    class = "page",
    id = ns(paste0("step", i)),
    self$rv$ll.screens[[i]]
  )
      else 
        hidden(div(
        class = "page",
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
          shinyjs::hide(selector = ".page")
          shinyjs::show(paste0("step", self$rv$page))
        })


        navPage <- function(direction) {
          self$rv$page <- self$rv$page + direction
        }
        
        observeEvent(input$prevBtn, navPage(-1))
        observeEvent(input$nextBtn, navPage(1))
        observeEvent(input$toggle,{
          shinyjs::toggleState('step1', condition=input$toggle%%2 == 0)
          shinyjs::toggleState('step2', condition=input$toggle%%2 == 0)
          shinyjs::toggleState('step3', condition=input$toggle%%2 == 0)

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
    step1 = function(input){
      ns <- NS(self$id)
      
      select1 <- function(){
        renderUI({
          ns <- NS(self$id)
          selectInput(ns('sel1'), 'Select 1', choices=1:3)
        })
      }
      
      observeEvent(input$btn1, {cat('btn 1 clicked, selection = ', input$sel1, '\n')})
      
      tagList(
        h4(paste0("Step", 1)),
        selectInput(ns('sel1'), 'Select 1', choices=1:3),
        actionButton(ns(paste0('btn',1)), paste0('btn ',1))
      )
      
    },
    
    
    
    ### Step 2
    step2 = function(input){
      ns <- NS(self$id)
      
      observeEvent(input$btn2, {cat('btn 2 clicked, selection = ', input$sel2, '\n')})
      
      tagList(
        h4(paste0("Step", 2)),
        selectInput(ns('sel2'), 'Select 2', choices=1:3),
        actionButton(ns(paste0('btn',2)), paste0('btn ',2))
      )
      
    },
    
    ### Step 3
    step3 = function(input){
      ns <- NS(self$id)
      
      observeEvent(input$btn3, {cat('btn 3 clicked, selection = ', input$sel3, '\n')})
      
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
    rv = reactiveValues(
      toto = NULL,
      ll.screens = NULL
    ),
    tl = NULL,
    length= 3,
    
    initialize = function(id){
      self$id <- id
    },
    
    GetScreensDefinition = function(input){
      lapply(1:self$length, function(x){
        eval(parse(text = paste0("self$step", x, '(input)')))
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
        
        output$show_tl <- renderUI({
          req(self$tl)
          self$tl$ui()
        })

        self$rv$ll.screens <- self$GetScreensDefinition(input)

      }
      )
    }
  )
)

#proc <- Process$new('App')
#ui = fluidPage(proc$ui())
#server = function(input, output){proc$server()}

child <- Child$new('App')
ui = fluidPage(child$ui())
server = function(input, output){child$server()}


shiny::shinyApp(ui, server)