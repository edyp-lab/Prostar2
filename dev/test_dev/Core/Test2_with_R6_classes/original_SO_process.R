library(shiny)
library(R6)

options(shiny.fullstacktrace = F)

Timeline <- R6Class(
  "Timeline",
  private=list(dataIn = NULL),
  public = list(id = NULL,
                initialize = function(id) {
                  self$id <- id
                }, 
                
                ui = function() {
                  ns <- NS(self$id)
                  tagList(h4(paste0('child id : ', self$id)), 
                          uiOutput(ns('show_steps'))
                  )
                },
                
                server = function(steps, status, pos) {
                  ns <- NS(self$id)
                  
                  moduleServer(self$id, function(input, output, session) {
                    output$show_steps <- renderUI({p(paste0(paste0(steps, collapse=' '), ' | ',paste0(status, collapse=' ')))})

                  })
                }
  )
)

#----------------------------------------------------------------------------
Process <- R6Class(
  "Process",
  public = list(id = NULL,
                child = NULL,
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
                  self$child <- Timeline$new(NS(id)('child_1'))
                },
                
                ui = function() {
                  ns <- NS(self$id)
                  tagList(h3(paste0('Mother id : ', self$id)),
                          uiOutput(ns("select")),
                          actionButton(ns("dataOut_btn"), "Simulate change in dataOut"),
                          actionButton(ns("status_btn"), "Simulate change in status"),
                          actionButton(ns("pos_btn"), "Simulate change in pos"),
                          hr(),
                          self$child$ui()
                  )
                },
                
                server = function(config, dataIn, dataOut) {
                  ns <- NS(self$id)

                  self$child$server(steps = config$steps,
                                    status = config$status,
                                    pos = 2)

                  moduleServer(self$id, function(input, output, session) {
                    
                    output$select <- renderUI({selectInput(ns('selectData'), 'Mother : Select data', 1:4) })
                    
                    observeEvent(dataOut$trigger,{print(paste0('receive new dataOut : ', paste0(lapply(reactiveValuesToList(dataOut),function(x){ x}), collapse=' ')))})
                    
                    observeEvent(input$selectData,{self$rv$data <- input$selectData})
                    
                    observeEvent(input$dataOut_btn,{
                      dataOut$name = self$id
                      dataOut$obj = input$do
                      dataOut$trigger = runif(1,0,1)
                      print(paste0('send dataOut :', paste0(lapply(reactiveValuesToList(dataOut),function(x){ x}), collapse=' ')))
                    })
                    
                    observeEvent(input$pos_btn,{ self$pos <- input$pos_btn %% 2})
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

rv = reactiveValues(data = 3)
dataOut <- reactiveValues()

process <- Process$new("Process")
ui = function() {process$ui()}
server = function(input, output, session) {
  process$server(config,
                 dataIn = reactive({rv$data}),
                 dataOut = dataOut)
}

shinyApp(ui, server)
