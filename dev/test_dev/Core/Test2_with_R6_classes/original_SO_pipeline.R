library(shiny)
library(R6)

options(shiny.fullstacktrace = F)

Process <- R6Class(
  "Process",
  private=list(dataIn = NULL),
  public = list(id = NULL,
                initialize = function(id) {
                  self$id <- id
                }, 
                
                ui = function() {
                  ns <- NS(self$id)
                  tagList(h4(paste0('child id : ', self$id)), 
                          uiOutput(ns('show_steps')),
                          uiOutput(ns("showBtn")) 
                          )
                },
                
                server = function(steps, status, dataIn, dataOut) {
                  ns <- NS(self$id)
                  
                  moduleServer(self$id, function(input, output, session) {

                    output$showBtn <- renderUI({actionButton(ns("do"), "Calc")})
                    output$show_steps <- renderUI({p(paste0(paste0(steps, collapse=' '), ' | ',paste0(status, collapse=' ')))})

                    
                    observeEvent(input$do,{
                      dataOut$name = self$id
                      dataOut$obj = input$do
                      dataOut$trigger = runif(1,0,1)
                      print(paste0('send dataOut :', paste0(lapply(reactiveValuesToList(dataOut),function(x){ x}), collapse=' ')))
                    })
                  })
                }
  )
)

#----------------------------------------------------------------------------
Pipeline <- R6Class(
  "Pipeline",
  public = list(id = NULL,
                child = NULL,
                childs = list(),
                n = 3,
                rv = reactiveValues(
                  data = 3,
                  selectData = 0),
                res = reactiveValues(
                  name = NULL,
                  obj = NULL,
                  trigger = NULL
                ),
                
                initialize = function(id) {
                  self$id <- id
                  lapply(1:self$n, function(x){self$childs[[x]] <- Process$new(NS(id)(paste0('child_', x)))})
                },
                
                ui = function() {
                  ns <- NS(self$id)
                  tagList(h3(paste0('Mother id : ', self$id)),
                          uiOutput(ns("select")),
                          hr(),
                          lapply(1:self$n, function(x){self$childs[[x]]$ui()})
                  )
                },
                
                server = function(config) {
                  ns <- NS(self$id)
                  dataOut <- reactiveValues()
                  LaunchServers <- function() 
                      lapply(1:self$n, function(x){self$childs[[x]]$server(steps = config$steps,
                                                                           status = config$status,
                                                                           dataIn = reactive({self$rv$data}),
                                                                           dataOut = dataOut)
                      })

                  LaunchServers()
                  moduleServer(self$id, function(input, output, session) {
                  
                    output$select <- renderUI({selectInput(ns('selectData'), 'Mother : Select data', 1:4) })
                    
                    observeEvent(dataOut$trigger,{print(paste0('receive new dataOut : ', paste0(lapply(reactiveValuesToList(dataOut),function(x){ x}), collapse=' ')))})
                    
                    observeEvent(input$selectData,{self$rv$data <- input$selectData
                    })
                  }
                  ) }
  )
)

#----------------------------------------------------------------------------
config <- reactiveValues(
  steps = data.frame(mandatory = c(T, F, T),
                     status = c(0, 0, 0),
                     row.names  = c('Description', 'Step1', 'Step2')
                     ),
  screens = NULL
)
mother <- Pipeline$new("Pipeline")
ui = function() {mother$ui()}
server = function(input, output, session) {mother$server(config)}
shinyApp(ui, server)
