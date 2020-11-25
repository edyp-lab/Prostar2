library(shiny)
library(shinyjs)
library(R6)

NUM_PAGES <- 5




Timeline  <- R6Class(
  "Timeline",
  public = list(
    id = NULL,
    rv = "<reactiveValues>",
    initialize = function(id){
      self$id <- id
      rv <- reactiveValues( page = 1,
                            screens = NULL)
    },

    GetScreens = function(){
      ns <- NS(self$id)
      lapply(1:length(self$rv$screens), function(i) {
        div(
          class = "page",
          id = ns(self$rv$screens[[i]]),
          uiOutput(ns(self$rv$screens[[i]]))
        )
      })
    },
    
    ui = function(){
      ns <- NS(self$id)
      fluidPage(
        useShinyjs(),
        hidden(
          self$GetScreens()
        ),
        br(),
        actionButton(ns("prevBtn"), "< Previous"),
        actionButton(ns("nextBtn"), "Next >"),
        actionButton(ns('toggle'), 'toggle')
      )
    },
    
    server = function(screens){
      ns <- NS(self$id)
      
      observeEvent(screens(), {self$rv$screens <- screens()})
      
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        
        output$step1 <- renderUI({
          h4('Step 1')
          actionButton(ns('btn1'), 'btn1')
        })
        
        output$step2 <- renderUI({
          h4('Step 2')
          actionButton(ns('btn2'), 'btn2')
        })
        
        output$step3 <- renderUI({
          h4('Step 3')
          actionButton(ns('btn3'), 'btn3')
        })
        
        observeEvent(self$rv$page,{
          cat(paste0(self$rv$page, '\n'))
          toggleState(id = "prevBtn", condition = self$rv$page > 1)
          toggleState(id = "nextBtn", condition = self$rv$page < NUM_PAGES)
          hide(selector = ".page")
          show(paste0("step", self$rv$page))
        })
        
        observeEvent(input$toggle,{
          shinyjs::toggleState('step2', condition=input$toggle%%2 == 0)
        })
        
        navPage <- function(direction) {
          self$rv$page <- self$rv$page + direction
        }
        
        observeEvent(input$prevBtn, navPage(-1))
        observeEvent(input$nextBtn, navPage(1))
      
    }
  )
    }
)
)


Process  <- R6Class(
  "Process",
  public = list(
    id = NULL,
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
                      'step3',
                      'step4',
                      'step5')
      
      tl <- Timeline$new(ns('App'))
      tl$server(screens=reactive({screens}))
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        output$show_tl <- renderUI({
          req(tl)
          tl$ui()
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