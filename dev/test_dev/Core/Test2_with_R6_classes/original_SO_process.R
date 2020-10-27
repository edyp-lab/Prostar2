library(shiny)
library(R6)
library(tibble)

options(shiny.fullstacktrace = F)

#----------------------------------------------------------------------------------
TimelineDraw <- R6Class(
  "TimelineDraw",
  private=list(verbose = T,
               length = NULL,
               VALIDATED = 1,
               UNDONE = 0,
               SKIPPED = -1,
               RESETED = 2,
               reset_OK = NULL),
  public = list(id = NULL,
                
                style = NULL,
                steps = NULL,
                rv = reactiveValues(
                  current.pos = 1
                ),
                initialize = function(id, steps, style) {
                  self$id = id
                  self$style <- style
                  self$steps <- steps
                  private$length <- length(steps)
                }, 
                
                ui = function() {
                  ns <- NS(self$id)
                  wellPanel(h3('TimelineDraw'),
                     uiOutput(ns('show'))
                  )
                },
                
                server = function(status, position) {
                  ns <- NS(self$id)
                  
                  moduleServer(self$id, function(input, output, session) {
                    output$show <- renderUI({
                      tagList(
                        p(paste0('status = ', paste0(status(), collapse=' '))),
                        p(paste0('current.pos = ', position()))
                      )
                    })


                  })
                }
  )
)



# ------------- Class TimelineDataManager  --------------------------------------
TimelineManager <- R6Class(
  "TimelineManager",
  private=list(verbose = T,
               style = 2,
               length = NULL,
               modal_txt = NULL,
               DEFAULT_SKIPPED_POSITION = 1,
               DEFAULT_VALIDATED_POSITION = 1,
               DEFAULT_UNDONE_POSITION = 1,
               reset_OK = 0,
               timelineDraw  = NULL),
  
  public = list(id = NULL,
                tl_draw = NULL,
                rv = reactiveValues(
                  current.pos = 1
                  ),
                initialize = function(id) {
                  self$id <- id
                  self$tl_draw <- TimelineDraw$new(NS(id)('tl_draw'),
                                                   steps = reactive({NULL}),
                                                   style = 2)
                }, 
                
                ui = function() {
                  ns <- NS(self$id)
                  fluidPage(
                    wellPanel(
                    tagList(h3('TimelineManager'), 
                            self$tl_draw$ui(),
                            actionButton(ns('rstBtn'), "reset"),
                            actionButton(ns('pos'), "Simulate change position"),
                            actionButton(ns("status_btn"), "Simulate change in status"),
                            dataTableOutput(ns('show_config')),
                            uiOutput(ns('show_pos'))
                  )
                  )
                  )
                },
                
                server = function(config, wake) {
                  ns <- NS(self$id)
                  
                  self$tl_draw$server(status = reactive({config$steps$status}),
                                      position = reactive({self$rv$current.pos}))
                  
                  moduleServer(self$id, function(input, output, session) {
                    
                    output$show_config <- renderDataTable(
                      config$steps,
                      options = list(dom = 't', rownames= TRUE))
                    
                    
                    observeEvent(input$rstBtn,{
                      private$reset_OK <- input$rstBtn
                      self$rv$current.pos <- 1
                      })
                    
                    observeEvent(input$pos,{self$rv$current.pos <- input$pos})
                    
                    observeEvent(input$status_btn,{config$steps$status[1] <- input$status_btn %% 2})
                    
                    list(current.pos = reactive({self$rv$current.pos}),
                         reset = reactive({input$rstBtn})
                    )
                  })
                }
  )
)

#----------------------------------------------------------------------------
ProcessManager <- R6Class(
  "ProcessManager",
  private = list(
    TimelineManager = NULL
  ),
  public = list(id = NULL,

                config = reactiveValues(
                  steps = tibble(
                    name = c("Description", "Step1", "Step2"),
                    mandatory = c(T, F, T),
                    status = c(0, 0, 0),
                    screens = c('a', 'a', 'a')
                  )
                ),
                
                
                rv = reactiveValues(
                  dataIn = NULL,
                  current.pos = 1,
                  selectData = 0),
                
                
                res = reactiveValues(
                  name = NULL,
                  obj = NULL,
                  trigger = NULL
                ),
                
                initialize = function(id) {
                  self$id <- id
                  private$TimelineManager <- TimelineManager$new(NS(id)('timeline'))
                },
                
                ui = function() {
                  ns <- NS(self$id)
                  fluidPage(
                  wellPanel(
                    tagList(h3('ProcessManager'),
                          uiOutput(ns("select")),
                          actionButton(ns("dataOut_btn"), "Simulate validate (change in dataOut)"),
                          actionButton(ns("pos_btn"), "Simulate change in pos"),
                          hr(),
                          private$TimelineManager$ui(),
                          uiOutput(ns('show_dataIn'))
                  )
                  )
                  )
                },
                
                server = function(dataIn, dataOut) {
                  ns <- NS(self$id)
                  
                  current.pos = reactiveVal()
                  res <- private$TimelineManager$server(config = self$config,
                                                 wake = NULL)
                  observeEvent(dataIn(), {self$rv$dataIn <- dataIn })

                  moduleServer(self$id, function(input, output, session) {
                    
                    output$select <- renderUI({selectInput(ns('selectData'), 'Mother : Select data', 1:4) })
                    
                    output$show_dataIn <- renderUI({paste0('received dataIn = ', dataIn())})
                    
                    observeEvent(dataOut$trigger,{print(paste0('receive new dataOut : ', paste0(lapply(reactiveValuesToList(dataOut),function(x){ x}), collapse=' ')))})
                    
                    observeEvent(input$selectData,{self$rv$data <- input$selectData})
                    
                    observeEvent(self$rv$data,{print(paste0('new value for self$rv$data : ', self$rv$data)) })
                    
                    observeEvent(input$dataOut_btn,{
                      dataOut$name = self$id
                      dataOut$obj = input$do
                      dataOut$trigger = runif(1,0,1)
                      print(paste0('send dataOut :', paste0(lapply(reactiveValuesToList(dataOut),function(x){ x}), collapse=' ')))
                    })
                    
                    observeEvent(input$pos_btn,{current.pos(input$pos_btn)})
                    
                    
                    observeEvent(lapply(res,function(x){x()}),{
                      print(paste0('retour de la timeline : ', res$reset(), ' ', res$current.pos()))
                    })
                    
                  }
                  ) }
  )
)

#----------------------------------------------------------------------------


rv = reactiveValues(data = 3)
dataOut <- reactiveValues()

processManager <- ProcessManager$new("ProcessManager")
ui = function() {
  fluidPage(
    wellPanel(h3('Prostar'),
              processManager$ui()
    )
  )
    }
server = function(input, output, session) {
  processManager$server(dataIn = reactive({rv$data}),
                 dataOut = dataOut)
}

shinyApp(ui, server)
