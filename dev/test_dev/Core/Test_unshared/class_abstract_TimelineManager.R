TimelineManager <- R6Class(
  "TimelineManager",
  private=list(id = NULL,
               style = 2,
               type = 'generic',
               config = reactiveValues(),
               rv = reactiveValues(
                 current.pos = 1,
                 reset_OK = NULL
               ),
               global = list(VALIDATED = 1,
                             SKIPPED = -1,
                             UNDONE = 1
               ),
               default_pos =list(VALIDATED = 1,
                                 SKIPPED = 1,
                                 UNDONE = 1
               ), 
               nbSteps = 0,
               modal_txt = NULL,
               btn_style = "display:inline-block; vertical-align: middle; padding: 7px",
               timelineDraw  = NULL,
               
               
               Analyse_status = function(){},
               
               Init_Default_Positions = function(){
                 cat(paste0(class(self)[1], '::Init_Default_Positions()\n'))
                 private$default_pos <- list(VALIDATED = private$nbSteps,
                                             SKIPPED = private$nbSteps,
                                             UNDONE = 1
                 )
               },
               
               NextBtn_logics = function(){
                 cat(paste0(class(self)[1], '::NextBtn_logics()\n'))
                 # Compute status for the Next button
                 end_of_tl <- private$rv[[private$id]]$current.pos == private$nbSteps
                 mandatory_step <- isTRUE(private$config[[private$id]]$mandatory[private$rv[[private$id]]$current.pos])
                 validated <- private$config[[private$id]]$status[private$rv[[private$id]]$current.pos] == private$global$VALIDATED
                 skipped <- private$config[[private$id]]$status[private$rv[[private$id]]$current.pos] == private$global$SKIPPED
                 entireProcessSkipped <- private$config[[private$id]]$status[private$nbSteps] == private$global$SKIPPED
                 NextBtn_logics <- !end_of_tl && !entireProcessSkipped && (!mandatory_step || (mandatory_step && (validated || skipped)))
                 NextBtn_logics
               },
               
               PrevBtn_logics = function(){
                 cat(paste0(class(self)[1], '::PrevBtn_logics()\n'))
                 # Compute status for the Previous button
                 start_of_tl <- private$rv[[private$id]]$current.pos == 1
                 entireProcessSkipped <- private$config[[private$id]]$status[private$nbSteps] == private$global$SKIPPED
                 PrevBtn_logics <- !start_of_tl && !entireProcessSkipped
                 PrevBtn_logics
               },
               
               
               CheckConfig = function(conf){
                 cat(paste0(class(self)[1], '::CheckConfig()\n'))
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
               
               Update_Cursor_position = function(){
                 cat(paste0(class(self)[1], '::Update_Cursor_position()\n'))
                 req(private$config[[private$id]]$status)
                 if (private$config[[private$id]]$status[private$nbSteps] == private$global$VALIDATED)
                   private$rv[[private$id]]$current.pos <- private$default_pos$VALIDATED
                 else if (private$config[[private$id]]$status[private$nbSteps] == private$global$SKIPPED)
                   private$rv[[private$id]]$current.pos <- private$default_pos$SKIPPED
                 else if (private$config[[private$id]]$status[private$nbSteps] == private$global$UNDONE)
                   private$rv[[private$id]]$current.pos <- private$default_pos$UNDONE
               },
               
               Display_Current_Step = function(){},
               
               Analyse_Status = function(){},
               # Initialization of the screens by integrating them into a div specific
               # to this module (name prefixed with the ns() function
               # Those div englobs the div of the caller where screens are defined
               EncapsulateScreens = function(){
                 cat(paste0(class(self)[1], '::EncapsulateScreens()\n'))
                 req(private$config[[private$id]]$screens)
                 ns <- NS(private$id)
                 private$Init_Default_Positions() 
                 private$config[[private$id]]$screens <- lapply(1:private$nbSteps,
                                                                function(x){
                                                                  private$config[[private$id]]$screens[[x]] <- if (x == 1) 
                                                                    div(id = ns(paste0("div_screen", x)),  private$config[[private$id]]$screens[[x]])
                                                                  else 
                                                                    shinyjs::hidden(div(id = ns(paste0("div_screen", x)),  private$config[[private$id]]$screens[[x]]))
                                                                })
               }
               
               
  ),
  
  public = list(
    initialize = function(){
      stop(" TimelineManager is an abstract class that can't be initialized.")
    },
    
    
    toggleState_Steps = function(cond, i){
      cat(paste0(class(self)[1], '::toggleState_Steps()\n'))
      lapply(1:i, function(x){
        shinyjs::toggleState(paste0('div_screen', x), condition = cond)})
    },
    
    
    # UI
    ui = function() {
      ns <- NS(private$id)
      fluidPage(
        wellPanel(style="background-color: lightblue;",
                  tagList(
                    uiOutput(ns('title')),
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
    server = function(config, wake, remoteReset) {
      ns <- NS(private$id)
      cat(paste0(class(self)[1], '::server()\n'))
      
      observeEvent(config,{
        cat(paste0(class(self)[1], '::observeEvent(config)\n'))
        private$config[[private$id]] <- config
        private$rv[[private$id]]$current.pos <- 1
      })
      
      observeEvent(config$status,{
        cat(paste0(class(self)[1], '::observeEvent(config$status)\n'))
        private$config[[private$id]]$status <- config$status
      })
      
      observeEvent(req(wake()),{
        cat(paste0(class(self)[1], '::observeEvent(req(wake()))\n'))
        private$Update_Cursor_position()
        })
      
      observeEvent(remoteReset(),ignoreInit = T, {
        cat(paste0(class(self)[1], '::observeEvent(remoteReset())\n'))
        private$rv[[private$id]]$current.pos <- 1
      })
      
      cat(paste0(class(self)[1], '::private$timelineDraw$server()\n'))
      private$timelineDraw$server(
        status = reactive({private$config[[private$id]]$status}),
        position = reactive({private$rv[[private$id]]$current.pos})
      )
      
      # MODULE SERVER
      moduleServer(private$id, function(input, output, session) {
        ns <- NS(private$id)
        
        cat(paste0(class(self)[1], '::moduleServer()\n'))
        # Show modal when button reset is clicked
        observeEvent(input$rstBtn, {
          cat(paste0(class(self)[1], '::observeEvent(input$rstBtn)\n'))
          showModal(dataModal())
          })
        
        ###############################
        output$showResetBtn <- renderUI({
          actionButton(ns("rstBtn"), paste0("Reset ", private$type),
                       style='padding:4px; font-size:80%')
        })
        
        output$showPrevBtn <- renderUI({
          shinyjs::disabled(actionButton(ns("prevBtn"), "<<",
                                         style='padding:4px; font-size:80%'))
        })
        
        output$showNextBtn <- renderUI({
          actionButton(ns("nextBtn"), "next",
                       style='padding:4px; font-size:80%')
        })
        
        output$title <- renderUI({ h3(paste0('private$id = ',private$id)) })
        
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
        
       
        
        # When OK button is pressed, update the reactive value which will be sent
        # to the caller
        observeEvent(req(c(input$modal_ok)), ignoreInit=T,{
          cat(paste0(class(self)[1], '::observeEvent(req(c(input$modal_ok)))\n'))
          browser()
          private$rv[[private$id]]$reset_OK <- input$rstBtn
          private$rv[[private$id]]$current.pos <- 1
          removeModal()
        })
        
        navPage <- function(direction) {
          newval <- private$rv[[private$id]]$current.pos + direction 
          newval <- max(1, newval)
          newval <- min(newval, private$nbSteps)
          if(newval == 0)
            browser()
          
          private$rv[[private$id]]$current.pos <- newval
        }
        
        
        observeEvent(input$prevBtn, ignoreInit = TRUE, {navPage(-1)})
        observeEvent(input$nextBtn, ignoreInit = TRUE, {navPage(1)})
        
        output$show_screens <- renderUI({
          cat(paste0(class(self)[1], '::output$show_screens\n'))
          tagList(private$config[[private$id]]$screens)
          })
        
        
        # Catch a new position or a change in the status list
        observeEvent(req(c(private$rv[[private$id]]$current.pos, private$config[[private$id]]$status)), {
           cat(paste0(class(self)[1], '::observeEvent(req(c(private$rv[[private$id]]$current.pos, private$config[[private$id]]$status))\n'))
          browser()
          private$Update_Cursor_position()
          private$Analyse_Status()
          private$Display_Current_Step()
          
          shinyjs::toggleState('prevBtn', cond = private$PrevBtn_logics())
          shinyjs::toggleState('nextBtn', cond = private$NextBtn_logics())
        })
        
        
        observeEvent(req(private$config[[private$id]]), ignoreInit=F,{
          cat(paste0(class(self)[1], '::observeEvent(req(private$config[[private$id]])\n'))
          req(private$nbSteps>0)
          check <- private$CheckConfig(private$config[[private$id]])
          if (!check$passed)
            stop(paste0("Errors in 'config'", paste0(check$msg, collapse=' ')))
          
          private$EncapsulateScreens()
        })
        
        
          list(current.pos = reactive({private$rv[[private$id]]$current.pos}),
               reset = reactive({private$rv[[private$id]]$reset_OK})
          )
      })
    }
  )
)