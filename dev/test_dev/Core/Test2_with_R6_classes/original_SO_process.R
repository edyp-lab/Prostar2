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
               btn_style = "display:inline-block; vertical-align: middle; padding: 7px",
              # global = list(VALIDATED = 1,
              #               UNDONE = 0,
              #               SKIPPED = -1,
              #               RESETED = 2),
               reset_OK = 0,
               timelineDraw  = NULL,
               
               CheckConfig = function(conf){
                 passed <- T
                 msg <- ""
                 if (!is.list(conf)){
                   passed <- F
                   msg <- c(msg, "'config' is not a list")
                 }
                 if (length(conf)!=3){
                   passed <- F
                   msg <- c(msg, "The length of 'config' is not equal to 4")
                 }
                 names.conf <- c("process.name", "type", "steps")
                 if (!all(sapply(names.conf, function(x){x %in% names(conf)}))){
                   passed <- F
                   msg <- c(msg, "The names of elements in 'config' must be the following: 'process.name', 'type', 'steps'")
                 }
                 if (!is.list(conf$steps)){
                   passed <- F
                   msg <- c(msg, "The 'steps' slot is not a list")
                 }
                 
                 passed <- T
                 list(passed=passed,
                      msg = msg)
               },
              
               toggleState_Steps = function(cond, i){
                 if(private$verbose)
                   print(paste0('TL(', self$id, ') : toggleState_Steps() : cond = ', cond, ', i = ', i))
                 
                 lapply(1:i, function(x){
                   shinyjs::toggleState(paste0('div_screen', x), condition = cond)})
               },
              
               Analyse_status_Process = function(){},
              
               Update_Cursor_position = function(){
                 req(private$length)
                 
                 if(private$verbose)
                   print(paste0('TL(', self$id, ') : Update_Cursor_position() :'))
                 
                 if ((self$rv$status[[private$length]] == VALIDATED))
                   self$rv$current.pos <- private$global$VALIDATED
                 else if (self$rv$status[[private$length]] == SKIPPED)
                   self$rv$current.pos <- private$global$SKIPPED
                 else if (self$rv$status[[private$length]] == UNDONE)
                   self$rv$current.pos <- private$global$UNDONE
                 if(self$rv$current.pos==0)
                   browser()
               },
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
                                      #private$timelineDraw$ui(),
                                      actionButton(ns('pos'), "Simulate change position"),
                                      actionButton(ns("status_btn"), "Simulate change in status"),
                                      dataTableOutput(ns('show_config')),
                                      uiOutput(ns('show_pos')),
                                      shinyjs::useShinyjs(),
                                      div(id = 'GlobalTL',
                                          fluidRow(
                                            align= 'center',
                                            column(width=2,div(style = private$btn_style,
                                                         uiOutput(ns('showPrevBtn')),
                                                         uiOutput(ns('showResetBtn'))
                                                         )
                                                   ),
                                            column(width=8,div( style = private$btn_style,
                                                                private$timelineDraw$ui())),
                                            column(width=2,div(style = private$btn_style,
                                                               uiOutput(ns('showNextBtn')),
                                                               uiOutput(ns('showSaveExitBtn'))
                                                               )
                                                   )
                                            ),
                                          uiOutput(ns('show_screens'))
                                          )
                                      )
                              )
                  )
                },
                
                SetModalTxt = function(txt){private$modal_txt <- txt},
                
                # SERVER
                server = function(config, wake) {
                  ns <- NS(self$id)
                  
                  private$timelineDraw$server(
                    status = reactive({config$steps$status}),
                    position = reactive({self$rv$current.pos})
                    )
                  
                  # MODULE SERVER
                  moduleServer(self$id, function(input, output, session) {
                    ns <- NS(self$id)
                    
                    output$show_config <- renderDataTable(
                      config$steps,
                      options = list(dom = 't', rownames= TRUE))
                    
                    
                    observeEvent(input$rstBtn,{
                      private$reset_OK <- input$rstBtn
                      self$rv$current.pos <- 1
                      })
                    
                    observeEvent(input$pos,{self$rv$current.pos <- input$pos})
                    
                    observeEvent(input$status_btn,{
                      print("change in status")
                      config$steps$status[1] <- input$status_btn %% 2})
                    
                    
                    
                    ###############################
                    output$showResetBtn <- renderUI({
                      print(paste0('TL(',self$id, ') : output$showResetBtn <- renderUI'))
                      actionButton(ns("rstBtn"), paste0("Reset ", config$type),
                                   style='padding:4px; font-size:80%')
                    })
                    
                    output$showPrevBtn <- renderUI({
                      shinyjs::disabled(actionButton(ns("prevBtn"), "<<",
                                                     style='padding:4px; font-size:80%'))
                    })
                    
                    output$showNextBtn <- renderUI({
                      shinyjs::disabled(actionButton(ns("nextBtn"), "next",
                                                     style='padding:4px; font-size:80%'))
                    })
                    
                    
                    
                    #-------------------------------------------------------
                    # Return the UI for a modal dialog with data selection input. If 'failed' is
                    # TRUE, then display a message that the previous value was invalid.
                    dataModal <- function() {
                      modalDialog(
                        span(private$modal_txt),
                        footer = tagList(
                          modalButton("Cancel"),
                          actionButton(ns("modal_ok"), "OK")
                        )
                      )
                    }
                    
                    
                    # Show modal when button reset is clicked
                    observeEvent(input$rstBtn, {
                      showModal(dataModal())
                    })
                    
                    # When OK button is pressed, update the reactive value which will be sent
                    # to the caller
                    observeEvent(input$modal_ok, {
                      private$reset_OK <- input$rstBtn
                      removeModal()
                    })
                    
                    navPage <- function(direction) {
                      newval <- self$rv$current.pos + direction 
                      newval <- max(1, newval)
                      newval <- min(newval, private$length)
                      if(newval == 0)
                        browser()
                      
                      self$rv$current.pos <- newval
                    }
                    
                    observeEvent(req(wake()),{
                      if(private$verbose)
                        print(paste0('TL(',self$id, ') : observeEvent(current$wake() '))
                      
                      private$Update_Cursor_position()
                    })
                    
                    
                    Init_Default_Positions <- reactive({
                      private$global$VALIDATED <- private$length
                      private$global$SKIPPED <- private$length
                      private$gloabl$UNDONE <- 1
                    })
                    
                    
                    
                    observeEvent(input$prevBtn, ignoreInit = TRUE, {navPage(-1)})
                    observeEvent(input$nextBtn, ignoreInit = TRUE, {navPage(1)})
                    
                    
                    output$show_screens <- renderUI({tagList(config$screens)})
                    
                    
                    # Catch a new position or a change in the status list
                    observeEvent(req(c(self$rv$current.pos, config$status)), {
                      req(private$length)
                      if(private$verbose){
                        print(paste0('TL(', self$id, ') : observeEvent(req(c(self$rv$current.pos, config$status)) : '))
                        print(paste0('TL(', self$id, ') : status = ', paste0(config$status, collapse=' ')))
                      }
                      
                      private$Analyse_status()
                      Update_Buttons()
                    })
                    
                    
                    observeEvent(req(config), ignoreInit=F,{
                      if(private$verbose)
                        print(paste0('TL(',self$id, ') : observeEvent(req(config)() '))
                      req(length(config$screens)>0)
                      
                      if (!private$CheckConfig(config)$passed)
                        stop(paste0("Errors in 'config'", paste0(private$CheckConfig(config)$msg, collapse=' ')))
                      
                      EncapsulateScreens()
                      
                    })
                    
                    # Initialization of the screens by integrating them into a div specific
                    # to this module (name prefixed with the ns() function
                    # Those div englobs the div of the caller where screens are defined
                    EncapsulateScreens <- reactive({
                      req(config$screens)
                      private$length <- length(config$steps)
                      Init_Default_Positions() 
                      config$screens <- lapply(1:private$length,
                                               function(x){
                                                 config$screens[[x]] <- if (x == 1) 
                                                   div(id = ns(paste0("div_screen", x)),  config$screens[[x]])
                                                 else 
                                                   shinyjs::hidden(div(id = ns(paste0("div_screen", x)),  config$screens[[x]]))
                                               })
                    })
                    
                    Update_Buttons <- reactive({
                      # Compute status for the Next button
                      end_of_tl <- self$rv$current.pos == private$length
                      mandatory_step <- isTRUE(config$steps[[self$rv$current.pos]])
                      validated <- config$status[[self$rv$current.pos]] == VALIDATED
                      skipped <- config$status[[self$rv$current.pos]] == SKIPPED
                      entireProcessSkipped <- config$status[[private$length]] == SKIPPED
                      NextBtn_logics <- !end_of_tl && !entireProcessSkipped && (!mandatory_step || (mandatory_step && (validated || skipped)))
                      
                      # Compute status for the Previous button
                      start_of_tl <- self$rv$current.pos == 1
                      entireProcessSkipped <- config$status[[private$length]] == SKIPPED
                      PrevBtn_logics <- !start_of_tl && !entireProcessSkipped
                      
                      shinyjs::toggleState('prevBtn', cond = PrevBtn_logics)
                      shinyjs::toggleState('nextBtn', cond = NextBtn_logics)
                    })
                    
                    #####################################
                    list(current.pos = reactive({self$rv$current.pos}),
                         reset = reactive({private$reset_OK})
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
                    screens = list(uiOutput(ns('screenStep1')),
                                   uiOutput(ns('screenStep2')),
                                   uiOutput(ns('screenStep3')))
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
