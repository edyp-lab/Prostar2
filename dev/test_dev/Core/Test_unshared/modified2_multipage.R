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
        
        observeEvent(ll.screens,{
          cat('toto')
          self$rv$ll.screens <- ll.screens
        })
        
        observe({
          
          cat(paste0(self$rv$page, '\n'))
          toggleState(id = "prevBtn", condition = self$rv$page > 1)
          toggleState(id = "nextBtn", condition = self$rv$page < NUM_PAGES)
          shinyjs::hide(selector = ".page")
          shinyjs::show(paste0("step", self$rv$page))
        })
        
        output$toto <- renderUI({
          
         # hidden(
            lapply(1:3, function(i) {div(
              class = "page",
              id = ns(paste0("step", i)),
             ll.screens[[i]]
            )
            }
            )
         # )
        })

        navPage <- function(direction) {
          self$rv$page <- self$rv$page + direction
        }
        
        observeEvent(input$prevBtn, navPage(-1))
        observeEvent(input$nextBtn, navPage(1))
        observeEvent(input$toggle,{
          shinyjs::toggleState('step2', condition=input$toggle%%2 == 0)
          shinyjs::toggleState('step3', condition=input$toggle%%2 == 0)})
        
        
      }
      )
    }
  )
)


Process  <- R6Class(
  "Process",
  public = list(
    id = NULL,
    rv = reactiveValues(
      toto = NULL
    ),
    initialize = function(id){
      self$id <- id
    },
    
    
    ui = function(){
      ns <- NS(self$id)
      fluidPage(
        uiOutput(ns('show_tl'))
      )
    },
    
    server = function(){
      ns <- NS(self$id)
      
      screens <- list('step1',
                      'step2',
                      'step3')
      
      ll.screens <- lapply(1:3, function(i) {
        tagList(
          h4(paste0("Step", i)),
          actionButton(ns(paste0('btn',i)), paste0('btn ',i))
        )
      })
          
          
      
      tl <- Timeline$new(ns('App'))
      self$rv$toto <- tl$server(ll.screens)
      
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        output$show_tl <- renderUI({
          req(self$rv$toto)
          tl$ui()
        })
        
        output$step1 <- renderUI({
          p('step1')
        })
        
        output$step2 <- renderUI({
          p('step2')
        })
        
        output$step3 <- renderUI({
          p('step3')
        })
        
      }
      )
    }
  )
)

proc <- Process$new('App')
ui = fluidPage(proc$ui())
server = function(input, output){proc$server()}
shiny::shinyApp(ui, server)