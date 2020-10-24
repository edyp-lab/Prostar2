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
                          uiOutput(ns("showBtn")) 
                          )
                },
                
                server = function(dataIn, dataOut) {
                  ns <- NS(self$id)
                  moduleServer(self$id, function(input, output, session) {

                    output$showBtn <- renderUI({
                      actionButton(ns("do"), "Calc")
                      })
                    
                     
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
                
                server = function() {
                  ns <- NS(self$id)
                  dataOut <- reactiveValues()
                  LaunchServers <- function() 
                      lapply(1:self$n, function(x){self$childs[[x]]$server(dataIn = reactive({self$rv$data}),
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


mother <- Pipeline$new("mother")
ui = function() {mother$ui()}
server = function(input, output, session) {mother$server()}
shinyApp(ui, server)
