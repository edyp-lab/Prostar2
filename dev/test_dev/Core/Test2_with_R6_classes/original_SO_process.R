library(shiny)
library(R6)
library(tibble)

options(shiny.fullstacktrace = F)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('.', 'class_TimelineDraw.R'), local=TRUE)$value




# ------------- Class TimelineDataManager  --------------------------------------
TimelineManager <- R6Class(
  "TimelineManager",
  private=list(verbose = T,
               style = 2,
               length = NULL,
               modal_txt = NULL,
               global = list(VALIDATED = 1,
                             UNDONE = 0,
                             SKIPPED = -1,
                             RESETED = 2),
               reset_OK = 0,
               timelineDraw  = NULL,
               
               CheckConfig = function(conf){},
               toggleState_Steps = function(cond, i){},
               Analyse_status_Process = function(){},
               Update_Cursor_position = function(){},
               Analyse_status = function(){}
               ),
  
  public = list(id = NULL,
                steps = NULL,
                rv = reactiveValues(
                  current.pos = 1
                  ),
                initialize = function(id, steps, style=2 ) {
                  self$id <- id
                  self$steps = steps
                  private$timelineDraw <- TimelineDraw$new(NS(id)('tl_draw'),
                                                   steps = steps,
                                                   style = style)
                }, 
                
                # UI
                ui = function() {
                  ns <- NS(self$id)
                  fluidPage(
                    wellPanel(style="background-color: lightblue;",
                              tagList(h3('TimelineManager'),
                                      private$timelineDraw$ui(),
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
                  
                  private$timelineDraw$server(
                    status = reactive({config$steps$status}),
                    position = reactive({self$rv$current.pos})
                    )
                  
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

#----------------------- Class ProcessManager ----------------------------------
ProcessManager <- R6Class(
  "ProcessManager",
  private = list(
    timelineManager = NULL
  ),
  public = list(id = NULL,
                length = 3,
                config = reactiveValues(
                  steps = list(
                    mandatory = setNames(c(T, F, T), c("Description", "Step1", "Step2")),
                    status = setNames(c(0, 0, 0), c("Description", "Step1", "Step2")),
                    screens = setNames(c('a', 'a', 'a'), c("Description", "Step1", "Step2"))
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
                  
                },
                # UI
                ui = function() {
                  ns <- NS(self$id)
                  fluidPage(
                  wellPanel(style="background-color: yellow;",
                            h3('ProcessManager'),
                            uiOutput(ns('show_timeline_ui')),
                            uiOutput(ns("select")),
                            actionButton(ns("dataOut_btn"), "Simulate validate (change in dataOut)"),
                            actionButton(ns("pos_btn"), "Simulate change in pos"),
                            hr(),
                            #private$timelineManager$ui(),
                            uiOutput(ns('show_dataIn'))
                            )
                  )
                },
                
                Wake = function(){ runif(1,0,1)},
                
                # SERVER
                server = function(dataIn, dataOut) {
                  ns <- NS(self$id)
                  
                  current.pos = reactiveVal()
                  private$timelineManager <- TimelineManager$new(
                    id = NS(self$id)('timeline'),
                    steps = reactive({self$config$steps$mandatory})
                    )
                  
                  res <- private$timelineManager$server(
                    config = self$config,
                    wake = reactive({self$Wake()})
                    )
                  
                  
                  observeEvent(dataIn(), {self$rv$dataIn <- dataIn })

                  # MODULE SERVER
                  moduleServer(self$id, function(input, output, session) {
                    
                    output$show_timeline_ui <- renderUI({
                      private$timelineManager$ui()
                    })
                    
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
    wellPanel(style="background-color: green;",
              h3('Prostar'),
              processManager$ui()
    )
  )
    }
server = function(input, output, session) {
  processManager$server(
    dataIn = reactive({rv$data}),
    dataOut = dataOut
    )
}

shinyApp(ui, server)
