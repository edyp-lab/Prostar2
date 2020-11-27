library(R6)
library(shinyalert)
###-----------------------------------------------------------------
foo <- function() {
  match.call()[[1]]
}


###----------------------------------------------------------------

Timeline  <- R6Class(
  "Timeline",
  public = list(
    id = NULL,
    rv = "<reactiveValues>",
    initialize = function(id){
      self$id <- id
      self$rv = reactiveValues(
        current.pos = 1,
        reset = NULL,
        state = NULL
      )
      },
    ChangeState = function(){
      shinyjs::toggleState('reset', cond = self$rv$state)
    },
    
    ui = function(){
      ns <- NS(self$id)
      wellPanel(style="background-color: lightblue;",
      tagList(
        shinyjs::useShinyjs(),
        actionButton(ns('reset'), 'Simulate reset'),
        actionButton(ns('curpos'), 'Change current position'),
        actionButton(ns('state'), 'Change state')
      )
      )
    },
    server = function(){
      ns <- NS(self$id)
      
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)

        
        observeEvent(input$state, {
          self$rv$state <- input$state%%2 != 0
          self$ChangeState()
        })
        
        observeEvent(input$reset,{
          print(paste0(class(self)[1], '::', 'observeEvent(input$reset)'))
          print("action reset")
          self$rv$reset <- input$reset
          })
        
        observeEvent(input$curpos,{
          print(paste0(class(self)[1], '::', 'observeEvent(input$curpos)'))
          self$rv$current.pos <- input$curpos
        })
        
      }
      )
      
      list(current.pos = reactive({self$rv$current.pos}),
           reset = reactive({self$rv$reset})
      )
      
    }
  )
)

Filtering <- R6Class(
  "Filtering",
  inherit = Process,
  public = list(
    
    
    
  )
)


Process <- R6Class(
  "Process",
  public = list(
    id = NULL,
    timeline = NULL,
    dataOut = "<reactiveValues>",
    config = "<reactiveValues>",
    dataIn = "<reactiveValues>",
    rv = "<reactiveValues>",

    timeline.res = NULL,
    initialize = function(id){
      self$id <- id
      self$dataOut = reactiveValues()
      self$config = reactiveValues(
        name = NULL,
        steps = NULL
      )
      self$dataIn = reactiveValues()
      self$dataOut = reactiveValues()
      self$rv = reactiveValues(
        current.pos = 1
      )
      
      self$config$name <- description
      self$config$steps <- LETTERS[1:sample.int(10,1)]
      
    },
    
    
    ui = function(){
      ns <- NS(self$id)
      wellPanel(style="background-color: lightgreen;",
                tagList(
                  useShinyalert(),
                  uiOutput(ns('show_tl')),
                  uiOutput(ns('isSkipped')),
                  actionButton(ns('change'), 'Simulate change config'),
                  actionButton(ns('send'), 'Send value'),
                  actionButton(ns('curpos'), 'Change current position'),
                  uiOutput(ns('show_config'))
                  )
                )
      },
    
    server = function(dataIn, isSkipped = FALSE){
      ns <- NS(self$id)

      
      self$timeline <- Timeline$new(NS(self$id)('test'))
      self$timeline.res <- self$timeline$server()
      
      
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        
        
        observeEvent(dataIn(), {
          self$dataIn <- dataIn()
          
        })
        
        observeEvent(self$timeline.res$reset(), {
          print(paste0("self$timeline.res$reset() : ", self$id))
          })
        
        observeEvent(self$timeline.res$current.pos(), {
          print(paste0("self$timeline.res$current.pos() : ", self$timeline.res$current.pos()))
          self$rv$current.pos = self$timeline.res$current.pos()
        })
        
        
        output$isSkipped <- renderUI({
          req(isSkipped())
          # showModal(modalDialog(
          #   title = "You have logged in.",
          #   paste0("It seems you have logged in as",input$userid,'.'),
          #   easyClose = F,
          #   footer = NULL
          # ))
          #shinyalert("Oops!", "Something went wrong.", type = "error")
          wellPanel(
          style = "background-color: yellow; opacity: 0.72",
                        
                        h2("ZIP explorer")
          )
        })
        
        
        output$show_tl <- renderUI({
          req(self$timeline)
          self$timeline$ui()
        })
        
        output$show_config <- renderUI({ 
          self$dataIn
          self$config

          tagList(
            p(paste0("steps = ", paste0(self$config$steps, collapse=' '))),
            p(paste0(', dataIn = ', self$dataIn)),
            p(paste0('Current.pos = ', self$rv$current.pos))
          )
        })
        
        
        
        observeEvent(input$curpos, {
          self$rv$current.pos = input$curpos
          })
        
        observeEvent(input$change,{
          print("change a value")
          self$config$steps[1] <- input$change%%2 ==0
        })
        
        observeEvent(input$send,{
          self$dataOut <- self$config$steps
        })
        
      }
      
      )
      reactive({self$dataOut})
      
    }
  )
)


Pipeline <- R6Class(
  "Pipeline",
  public = list(
    id = NULL,
    ll.process = list(
      Description = NULL,
      processA = NULL
    ),
    rv = reactiveValues(
      dataIn = 3),
    steps = setNames(c('Description', 'processA'),c('Description', 'processA')),
    
    initialize = function(id){
      self$id <- id
    },
    res = reactiveValues(),
    tmp.return = reactiveValues(),
    ui = function(){
      ns <- NS(self$id)
      fluidPage(style="background-color: lightblue;",
                          
        tagList(
          actionButton(ns('sendToProcesses'), "Send dataIn"),
          actionButton(ns('skip'), "Set skipped"),
          uiOutput(ns('screen')),
          uiOutput(ns('res'))
        )
      )
    },
    server = function(input, output, session) {
      ns <- NS(self$id)
      
      lapply(names(self$ll.process), function(x){
        self$ll.process[[x]] <- Filtering$new(ns(x))
      })
      
      
      observeEvent(self$tmp.return[['Description']]()$trigger, {
        print("change in Description")
        print(paste0("self$rv$tmp.return[['Description']]()= ", paste0(self$tmp.return[['Description']]()$value, collapse=' ')))
      })
      
      observeEvent(self$tmp.return[['processA']]()$trigger, {
        print("change in processA")
        print(paste0("self$rv$tmp.return[['processA']]() = ", paste0(self$tmp.return[['processA']]()$value, collapse=' ')))
      })
      
      # rv$description <- processDescription$server()
      # rv$processA <- processA$server()
      self$tmp.return[['Description']] <- self$ll.process[['Description']]$server(dataIn = reactive({self$rv$dataIn}),
                                                                                  isSkipped = reactive({self$rv$isSkipped %%2 ==0}))
      self$tmp.return[['processA']] <- self$ll.process[['processA']]$server(dataIn = reactive({self$rv$dataIn}),
                                                                            isSkipped = reactive({self$rv$isSkipped %%2 ==0}))
      
      moduleServer(self$id, function(input, output, session) {
        ns <- NS(self$id)
        
        
        observeEvent(input$skip,{ self$rv$isSkipped <- input$skip})
        
        output$screen <- renderUI({
          tagList(
            wellPanel(h3('Description'), self$ll.process[['Description']]$ui()),
            wellPanel(h3('process A'), self$ll.process[['processA']]$ui())
          )
        })
        
        observeEvent(input$sendToProcesses, {
          self$rv$dataIn <- input$sendToProcesses
        })
        
        output$res <- renderUI({
          tagList(
            lapply(names(reactiveValuesToList(self$tmp.return)), function(x){p(paste0(x, ' = ', paste0(self$tmp.return[[x]]()$value, collapse=' ')))})
          )
        })
        
      })
    }
  )
)


super <- Pipeline$new('App')
ui = fluidPage(super$ui())
server = function(input, output){super$server()}
shiny::shinyApp(ui, server)
