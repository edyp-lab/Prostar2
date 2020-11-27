library(shiny)
library(shinyjs)
library(R6)

NUM_PAGES <- 3




Timeline  <- R6Class(
  "Timeline",
  public = list(
    id = NULL,
    rv = reactiveValues(
      page = 1,
      screens = NULL
    ),
    initialize = function(id){
      self$id <- id
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
        lapply(seq(NUM_PAGES), function(i) {
          div(
            class = "page",
            id = ns(paste0("step", i)),
            h4(paste0("Step", i)),
            actionButton(ns(paste0('btn',i)), paste0('btn ',i))
          )
        })
      ),
      br(),
      actionButton("prevBtn", "< Previous"),
      actionButton("nextBtn", "Next >"),
      actionButton('toggle', 'toggle')
      )
    },
    
    server = function(screens){
      ns <- NS(self$id)
       
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        rv <- reactiveValues(page = 1)
        
        observe({
          cat(paste0('page ', rv$page, '\n'))
          toggleState(id = "prevBtn", condition = rv$page > 1)
          toggleState(id = "nextBtn", condition = rv$page < NUM_PAGES)
          hide(selector = ".page")
          show(paste0("step", rv$page))
        })
        
        observeEvent(input$toggle,{
          shinyjs::toggleState('step2', condition=input$toggle%%2 == 0)
        })
        
        navPage <- function(direction) {
          rv$page <- rv$page + direction
        }
        
        observeEvent(input$prevBtn, navPage(-1))
        observeEvent(input$nextBtn, navPage(1))
        
        output$step1 <- renderUI({
          tagList(
            h4('Step 1'),
            actionButton(ns('btn1'), 'btn1')
          )
        })
        
        output$step2 <- renderUI({
          tagList(
            h4('Step 2'), actionButton(ns('btn2'), 'btn2')
          )
        })
        
        output$step3 <- renderUI({
          tagList(
            h4('Step 3'),
            actionButton(ns('btn3'), 'btn3')
          )
        })

      
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
                      'step3')
      
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